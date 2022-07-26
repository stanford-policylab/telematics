library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

############################## LOADING STOPS ################################
# Functions for loading raw stop data. Each function should take no arguments,
# and return an `sf` object with six columns: `beat`, `date`, `race`,
# `location`, `lat`, and `lng`.

load_stops <- function(city) {
  # NOTE: This recursive source is OK because of lazy evaluation and the fact
  # that the loading functions are not recursive. It would be nice if `R`
  # namespacing didn't require this...
  source(path(ROOT, "lib", "load.R"), local = TRUE)
  
  f <- current_env()[[str_c("load_stops_", city)]]
  
  if (is_null(f)) {
    abort(glue("No `load_stops` function for { city }."))
  }
  
  f()
}

load_stops_san_antonio <- function() {
  read_rds(path(ROOT, "data", "san_antonio", "raw", "san_antonio.rds")) %>%
    extract2("data") %>%
    mutate(beat = str_c(district, str_sub(substation, 1, 1))) %>%
    select(beat, date, violation, race = subject_race, location, lat, lng)
}

load_stops_houston <- function() {
  read_rds(path(ROOT, "data", "houston", "raw", "houston.rds")) %>%
    extract2("data") %>%
    select(beat, date, violation, race = subject_race, location, lat, lng) %>%
    mutate(beat = str_pad(beat, 5, "left", "0"))
}

load_stops_oklahoma_city <- function() {
  read_rds(path(ROOT, "data", "oklahoma_city", "raw", "oklahoma_city.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(beat, date, violation, race = subject_race, location, lat, lng)
}

load_stops_tulsa <- function() {
  read_rds(path(ROOT, "data", "tulsa", "raw", "tulsa.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(beat, date, violation, race = subject_race, location, lat, lng)
}

load_stops_wichita <- function() {
  read_rds(path(ROOT, "data", "wichita", "raw", "wichita.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(date, violation, race = subject_race, location, lat, lng)
}

load_stops_aurora <- function() {
  read_rds(path(ROOT, "data", "aurora", "raw", "aurora.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(date, violation, race = subject_race, location, lat, lng)
}

load_stops_plano <- function() {
  read_rds(path(ROOT, "data", "plano", "raw", "plano.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(date, violation, race = subject_race, location, lat, lng)
}

load_stops_mesa <- function() {
  read_rds(path(ROOT, "data", "mesa", "raw", "mesa.rds")) %>%
    extract2("data") %>%
    filter(type == "vehicular") %>%
    select(date, violation, race = subject_race, location, lat, lng)
}

load_stops_chicago <- function() {
  old_ord <- read_rds(path(ROOT, "data", "chicago", "raw", "statewide.rds")) %>%
    extract2("data") %>%
    filter(department_name == "CHICAGO POLICE") %>%
    mutate(
      beat = str_replace(beat, ".0", ""),
      beat = str_pad(beat, width = 4, side = "left", pad = "0"),
      lat = na_dbl,
      lng = na_dbl
    ) %>%
    select(beat, date, violation, race = subject_race, location, lat, lng)

  new_ord <- read_excel(
      path = path(ROOT, "data", "chicago", "raw", "15327-P580999-Traffic-ISR.xlsx"),
      sheet = 3,
      col_types = "text"
    ) %>%
    bind_rows(read_excel(
      path = path(ROOT, "data", "chicago", "raw", "15327-P580999-Traffic-ISR.xlsx"),
      sheet = 4,
      col_types = "text"
    )) %>%
    transmute(
      beat = BEAT,
      date = as_date(dmy_hm(CONTACTDATE)),
      violation = STATUTE,
      race = case_when(
        RACE %in% c("HAWAIIAN/PACIFIC ISLANDER", "ASIAN") ~ "asian/pacific islander",
        RACE == "BLACK" ~ "black",
        RACE == "HISPANIC" ~ "hispanic",
        RACE == "WHITE" ~ "white",
        TRUE ~ "other"
      ),
      race = factor(
        race,
        levels = c(
          "asian/pacific islander",
          "black",
          "hispanic",
          "white",
          "other",
          "unknown"
        )
      ),
      location = STREET_NME
    )

  bind_rows(old_ord, new_ord)
}

load_stops_madison <- function() {
  read_rds(path(ROOT, "data", "madison", "raw", "madison.rds")) %>%
    extract2("data") %>%
    mutate(beat = sector) %>%
    select(beat, date, violation, race = subject_race, location, lat, lng)
}

################################# LOAD BEATS ###################################
# Functions for loading the beats for each city as a tibble with two columns:
# `beat`, and `geometry`. Geometry should be an `sfc` object, and `beat` should
# match the beat identifier in the stop data. The geometry should use CRS 4269
# to match US Census data.
# NOTE: Be sure to read shapefile data in with `read_sf`, not `st_read`.

load_beats <- function(city) {
  # NOTE: This recursive source is OK because of lazy evaluation and the fact
  # that the loading functions are not recursive. It would be nice if `R`
  # namespacing didn't require this voodoo...
  source(path(ROOT, "lib", "load.R"), local = TRUE)
  
  f <- current_env()[[str_c("load_beats_", city)]]
  
  if (is_null(f)) {
    abort(glue("No `load_beats` function for { city }."))
  }
  
  f()
}

load_beats_san_antonio <- function() {
  read_sf(path(ROOT, "data", "san_antonio", "shapefiles", "sSAPDDistricts.shp")) %>%
    transmute(beat = str_c(DISTRICT, SUBCODE)) %>%
    st_transform(4269)
}

load_beats_chicago <- function() {
  read_sf(path(ROOT, "data", "chicago", "shapefiles", "beats.shp")) %>%
    transmute(beat = beat_num) %>%
    st_transform(4269)
}

load_beats_houston <- function(){
  read_sf(path(ROOT, "data", "houston", "shapefiles", "Houston_Police_Beats.kml")) %>%
    transmute(beat = Beats) %>%
    mutate(beat = str_pad(beat, 5, "left", "0")) %>%
    st_transform(4269)
}

load_beats_oklahoma_city <- function() {
  read_sf(path(ROOT, "data", "oklahoma_city", "shapefiles", "Police_Districts.shp")) %>%
    transmute(beat = BEAT) %>%
    st_transform(4269)
}

load_beats_tulsa <- function() {
  read_sf(path(ROOT, "data", "tulsa", "shapefiles", "TPD_Beats_2018.shp")) %>%
    transmute(beat = NAME) %>%
    group_by(beat) %>%
    summarize(.groups = "drop") %>%
    st_transform(4269)
}

load_beats_wichita <- function() {
  read_sf(path(ROOT, "data", "wichita", "shapefiles", "WichitaPoliceBeats.shp")) %>%
    transmute(beat = as.character(BEAT)) %>%
    st_transform(4269) %>%
    # Intersect with bounding box to eliminate erroneous polygon in southwest
    # quadrant.
    st_intersection(st_as_sfc(st_bbox(
      c(xmin = -97.55, ymin = 37.48, xmax = -97.15, ymax = 37.81),
      crs = 4269
    )))
}

load_beats_aurora <- function() {
  read_sf(path(ROOT, "data", "aurora", "shapefiles", "Police_Area_Representatives.shp")) %>%
    transmute(beat = BEATS) %>%
    st_transform(4269)
}

load_beats_plano <- function() {
  read_sf(path(ROOT, "data", "plano", "shapefiles", "City_of_Plano_Beats.shp")) %>%
    transmute(beat = BEAT) %>%
    st_transform(4269)
}

load_beats_madison <- function() {
  read_sf(path(ROOT, "data", "madison", "shapefiles", "shapes.shp")) %>%
    transmute(beat = as.character(Sector)) %>%
    st_transform(4269)
}

load_beats_mesa <- function() {
  read_sf(path(ROOT, "data", "mesa", "shapefiles", "MesaBeats.shp")) %>%
    transmute(beat = as.character(ESZ_NUM)) %>%
    group_by(beat) %>%
    summarize(.groups = "drop") %>%
    st_transform(4269)
}

################################## LOAD DRIVING ################################

load_driving <- function(city) {
  geoid_prefix <- pluck(CONFIG, city, "geoid_prefix")
  read_csv(path(ROOT, "data", city, "driving", "speeding_oct16.csv")) %>%
    # Filter class 1 highways
    filter(fc > 1) %>%
    # Filter rows with no observations
    filter(total_time != 0) %>%
    filter(speed_limit_kmh > 0) %>%
    rename(beat = geoid) %>%
    mutate(
      city = {{ city }},
      fc = factor(fc, levels = 2:4),
      beat = str_remove(beat, fixed(geoid_prefix))
    )
}

load_driving_tt <- function(city) {
   read_csv(path(ROOT, "data", city, "tomtom_driving",
                 str_c(city, "_driving.csv"))) %>%
    # Filter class 1 highways
    filter(fc > 1) %>%
    rename(beat = geoid) %>%
    mutate(city = {{ city }}, fc = factor(fc, levels = 2:4))
}

################################# LOAD EXPOSURE ################################

load_exposure <- function() {
  read_rds(path(ROOT, "data", "adj_matrix", "adj_matrix.rds")) %>%
    filter(fc > 1) %>%
    rename(tot_time = time_tot) %>%
    pivot_wider(
      id_cols = c(home_beat, transit_beat),
      names_from = fc,
      names_glue = "fc_{ fc }_{ .value }",
      values_from = c(avg_time, tot_time),
      values_fill = 0
    ) %>%
    mutate(
      avg_time = fc_2_avg_time + fc_3_avg_time + fc_4_avg_time,
      tot_time = fc_2_tot_time + fc_3_tot_time + fc_4_tot_time
    )
}

load_exposure_tt <- function(city) {
  read_csv(path(ROOT, "data", city, "tomtom_driving",
                str_c(city, "_driving.csv"))) %>%
    distinct(geoid, speed_limit_kmh, km_driven) %>%
    group_by(geoid) %>%
    summarize(exposure = sum(km_driven), .groups = "drop") %>%
    rename(beat = geoid)
}

################################## HOME BEATS ##################################

load_home_beats <- function() {
  read_rds(path(ROOT, "data", "adj_matrix", "home_beats.rds"))
}
