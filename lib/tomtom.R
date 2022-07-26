library(fs)

ROOT <- path(find_root(has_file(".gitignore")))

source(path(ROOT, "lib", "common.R"))
source(path(ROOT, "lib", "load.R"))

################################### GLOBALS ####################################
CONFIG <- read_yaml(path(ROOT, "lib", "config.yaml"))

################################### PROCESSING #################################
compute_driving_df <- function(segs_df, beats_df, speed_exposure_df, prefix) {
  speed_buckets <-
    c(
      "below_neg_10",
      "neg_10_neg_5",
      "neg_5_to_0",
      "pos_0_to_5",
      "pos_5_to_10",
      "pos_10_to_15",
      "pos_15_to_20",
      "pos_20_to_25",
      "pos_25_to_30",
      "above_pos_30"
    )  
  segs_df %>%
    rename(
      lat_start = lat1, 
      lat_end = lat2, 
      lon_start = lon1, 
      lon_end = lon2
    ) %>%
    pivot_longer(
      cols = c(lat_start, lat_end, lon_start, lon_end),
      names_to = c('.value', "location"),
      names_sep = "_"
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4269) %>%
    st_join(beats_df, join = st_within) %>%
    st_drop_geometry() %>% 
    drop_na(beat) %>%
    select(id, dist, beat) %>%
    group_by(id, beat) %>%
    # NOTE: Distance is split between the start and end beats of a segment.
    summarize(len = sum(dist / 2), .groups = "drop") %>%
    # Join on speeding + exposure data for each road
    left_join(speed_exposure_df, by = 'id') %>%
    # Within each beat, combine all roads of the same speed limit by:
    #   (1) Computing the average speeding behavior weighted by exposure,
    #   (2) Summing the total exposure of those roads.
    mutate(km_driven = tomtom_exposure * len) %>%
    select(-len, -tomtom_exposure, -id) %>%
    filter(km_driven != 0) %>%        
    mutate(across(speed_buckets, ~ . * km_driven)) %>%    
    group_by(beat, speed_limit) %>%
    summarize(across(everything(), sum)) %>%
    mutate(across(speed_buckets, ~ . / km_driven)) %>%
    # Match CMT dataframe specifications
    # NOTE: These fc are not real, they're imputed from the speed limit
    # NOTE: These are not speeding times: really, they're exposure.   
    mutate(
      fc = if_else(speed_limit > 72, 1, 2),
      beat = str_c(prefix, beat)
    )  %>%
    rename(speed_limit_kmh = speed_limit, geoid = beat) %>%
    pivot_longer(
      below_neg_10:above_pos_30,
      names_to = "sub_type",
      values_to = "speed_time"
    ) %>%
    mutate(speed_time = speed_time * km_driven) %>%
    mutate(
      sub_type = case_when(
        sub_type == 'neg_10_neg_5' ~ 'speeding -10 to -5',
        sub_type == 'neg_5_to_0' ~ 'speeding -5 to 0',
        sub_type == 'pos_0_to_5' ~ 'speeding 0 to 5',
        sub_type == 'pos_10_to_15' ~ 'speeding 10 to 15',
        sub_type == 'pos_15_to_20' ~ 'speeding 15 to 20',
        sub_type == 'pos_20_to_25' ~ 'speeding 20 to 25',
        sub_type == 'pos_25_to_30' ~ 'speeding 25 to 30',
        sub_type == 'above_pos_30' ~ 'speeding 30+',
        sub_type == 'pos_5_to_10' ~ 'speeding 5 to 10',
        sub_type == 'below_neg_10' ~ 'speeding below -10',
      )
    ) %>% 
    select(geoid, sub_type, fc, speed_limit_kmh, speed_time, km_driven)    
}

make_tomtom_driving_df <- function(city) {
  prefix <- pluck(CONFIG, city, 'geoid_prefix')
  beats_df <- load_beats(city) %>% clean_beats(city)
  segs_df <- read_csv(
    path(ROOT, 'data', city, 'segs', glue("{ city }_segs.csv"))
  )
  speed_exposure_df <- read_csv(
    path(ROOT, 'data', city, 'speeds', glue("{ city }_speed.csv"))
  )
  compute_driving_df(segs_df, beats_df, speed_exposure_df, prefix) %>%
    write_csv(
      path(ROOT, 'data', city, 'tomtom_driving', glue("{ city }_driving.csv"))
    )
}
