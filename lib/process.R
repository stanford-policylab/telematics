library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))

source(path(ROOT, "lib", "common.R"))
source(path(ROOT, "lib", "load.R"))

################################### GLOBALS ####################################
CONFIG <- read_yaml(path(ROOT, "lib", "config.yaml"))

################################### PROCESSING #################################

process_stops <- function(stops, city) {
  # Filters stops that happened on a highway (functional road class 2 or below).
  # Params:
  #   `stops`: A dataframe of stops with the following columns: `date`, `beat`,
  #   and `location`.
  #   `city`: The name of the city as a character vector, e.g., `"chicago"`.

  # Get configuration information
  hwy_regex <- pluck(CONFIG, city, "highways")
  if (is_null(hwy_regex)) {
    warn(glue("No highways filtered for { city }. Should there be?"))
  } else {
    hwy_regex <- str_c("(?i)", str_c(hwy_regex, collapse = "|"))
  }
  
  speeding_regex <- pluck(CONFIG, city, "speeding")
  if (is_null(speeding_regex)) {
    abort(glue("No speeding regex for { city }."))
  } else {
    speeding_regex <- str_c("(?i)", str_c(speeding_regex, collapse = "|"))
  }
  
  min_year <- pluck(CONFIG, city, "min_year", .default = 2013)
  max_year <- pluck(CONFIG, city, "max_year", .default = 2019)
  
  # Filter out highway stops and incomplete years
  stops %>%
    filter(
      ! str_detect(location, hwy_regex),
      year(date) >= min_year, year(date) <= max_year
    ) %>%
    # Annotate speeding stops
    mutate(is_speeding = str_detect(violation, speeding_regex)) %>%
    replace_na(list(is_speeding = FALSE))
}

clean_beats <- function(beats, city) {
  # Removes bad beats from beat map.
  
  bad_beats <- pluck(CONFIG, city, "bad_beats") %||% character(0)
  
  if (identical(bad_beats, character(0))) {
    warn(glue("No beats dropped for {city}. Should there have been?"))
  }
  
  beats %>%
    filter(! beat %in% bad_beats)
}

process_beats <- function(beats, city, demographics = NULL) {
  # Imputes demographic data for a beat map.
  # Params:
  #   `beat_map`: A dataframe with two columns: `beat`, which is the geographic
  #   identifier for a given beat, and `geography`, which is an `sf` object
  #   with the boundaries of each corresponding beat.
  #   `city`: The name of the city as a character vector, e.g., `"chicago"`.

  if (is_null(demographics)) {
    demographics <- get_census_geography(city)
  }

  beats %>%
    mutate(area_of_beat = st_area(geometry)) %>%
    st_set_agr("constant") %>%
    st_intersection(demographics) %>%
    mutate(
      area = st_area(geometry),
      pct_of_geo = area / area_of_geo,
      pct_of_beat = area / area_of_beat
    ) %>%
    # Strip units
    units::drop_units() %>% 
    group_by(beat, year) %>%
    summarize(
      area_accounted_for = sum(pct_of_beat),
      pop_tot = round(sum(pop_tot * pct_of_geo)),      
      pop_w = round(sum(pop_w * pct_of_geo)), 
      pop_b = round(sum(pop_b * pct_of_geo)),       
      pop_a = round(sum(pop_a * pct_of_geo)), 
      pop_h = round(sum(pop_h * pct_of_geo)),       
      med_inc = round(sum(med_inc * pct_of_beat)), # Use percentage of beat to
      .groups = "drop"                             # get average of income
    ) %>%
    mutate(
      pct_w = pop_w / pop_tot,
      pct_b = pop_b / pop_tot,      
      pct_a = pop_a / pop_tot,
      pct_h = pop_h / pop_tot,      
    ) %>%
    mutate_at(vars(starts_with("pop_")), as.integer) %>%
    select(beat, year, pct_w, pct_b, pct_a, pct_h, pop_tot, pop_w, pop_b, pop_a,
           pop_h, med_inc, area_accounted_for) %>%
    # To aid in debugging, sort in order of the area of beats that were matched
    # to block groups.
    arrange(desc(area_accounted_for))
}

get_census_geography <- function(city, geography = "block group") {
  # Gets all of the census geographies necessary to impute demographics for a
  # given city.

  # NOTE: Since ACS data is only available at the blockgroup level beginning in
  # 2013, all earlier data will be dropped.
  # Total, white, black, asian, hispanic population, median income
  variables <- c(
    'B02001_001', # Total population
    'B03002_003', # Non-Hispanic White population
    'B02001_003', # Black population
    'B02001_005', # Asian population
    'B03002_012', # Hispanic population
    'B19013_001'  # Median income
  )
  config <- CONFIG[[city]][c("state", "county")]
  min_year <- pluck(CONFIG, city, "min_year", .default = 2013)
  max_year <- pluck(CONFIG, city, "max_year", .default = 2019)

  filled_df <- expand_grid(year = seq(min_year, max_year), !!! config) %>%
    # Get ACS data for each year and county.
    mutate(
      bg = pmap(
        .l = .,
        .f = quietly(get_acs),
        geography = {{ geography }},
        variables = variables,
        output = "wide",
        geometry = TRUE
      ),
      bg = map(bg, "result")
    ) %>%
    unnest(bg) %>%
    # Assume values are unchanged for 2019
    complete(GEOID, year = 2013:2019) %>%
    arrange(GEOID, year) %>%
    select(
      geoid = GEOID,
      year,
      pop_tot = B02001_001E,
      pop_w = B03002_003E,
      pop_b = B02001_003E,
      pop_a = B02001_005E,
      pop_h = B03002_012E,      
      med_inc = B19013_001E,
      geometry
    ) %>%
    fill(pop_tot, pop_w, pop_b, pop_a, pop_h, med_inc, geometry) 

  # `rbind` doesn't return the right class so we have to cast
  class(filled_df[['geometry']]) <- 'list'
  filled_df %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    st_set_crs(4269) %>%
    # Calculate area.
    mutate(area_of_geo = st_area(geometry)) %>%
    # Restrict to years of usable data.
    filter(year >= min_year, year <= max_year) %>%
    # Eliminate annoying warning about attributes.
    st_set_agr("constant")
}

impute_beat <- function(stops, beats) {
  # Imputes the beat that a stop took place in from its lattitude and longitude.
  # (Note that if the beat is provided in the raw data, that takes precedence.)
  
  # Early return if beat already exists.
  if ("beat" %in% colnames(stops)) {
    stops %<>% drop_na(beat)
    return(stops)
  }

  # Strip beats dataframe down to beat number and geometry
  beats %<>%
    group_by(beat) %>%
    summarize(.groups = "drop")
  
  stops %<>% mutate(rn = row_number())
  
  imputed_stops <- stops %>%
    filter(! are_na(lat), ! are_na(lng)) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4269) %>%
    st_join(beats, join = st_within) %>%
    select(beat, rn)
  
  stops %>%
    left_join(imputed_stops, by = "rn") %>%
    select(beat, date, violation, race, location, lat, lng, is_speeding) %>%
    drop_na(beat)
}
