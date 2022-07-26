library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))

source(path(ROOT, "lib", "common.R"))

################################### PROCESSING #################################

adj_matrix <- function(use_cached = TRUE) {
  # Returns the adjacency matrix along with the home beats mapping used to
  # create it. The values in the adjacency matrix are the number of seconds a
  # driver from `home_beat` spends driving in `transit_beat` in an average year.
  # NOTE: Provides an optional `args` argument to prevent unnecessary reloading
  # of data and recomputation of home beat mappings.

  if (use_cached) {
    home_beats <- read_rds(path(ROOT, "data", "adj_matrix", "home_beats.rds"))
    args <- tibble(src = c("prod", "dssf")) %>%
      mutate(tmp = map(src, load_adj_matrix_data)) %>%
      unnest_wider(tmp) %>%
      left_join(nest(home_beats, home_beats = -src), by = "src") %>%
      mutate(home_beats = map2(home_beats, src, ~ mutate(.x, src = .y)))
  } else {
    args <- tibble(src = c("prod", "dssf")) %>%
      mutate(tmp = map(src, load_adj_matrix_data)) %>%
      unnest_wider(tmp) %>%
      mutate(
        home_beats = map(trips, impute_driver_info),
        # Annotate with source
        home_beats = map2(home_beats, src, ~ mutate(.x, src = .y))
      )
    # Cache the home beats
    args %>%
      pull(home_beats) %>%
      bind_rows() %>%
      write_rds(path(ROOT, "data", "adj_matrix", "home_beats.rds"))
  }

  # Annotate with home beat and then bind rows.
  total_time <- args %>%
    pmap(time_by_home_beat) %>%
    bind_rows() %>%
    group_by(home_beat, transit_beat, fc, src) %>%
    summarize(
      time_tot = sum(total_geo_time / active_yrs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = src,
      values_from = time_tot,
      names_prefix = "time_tot_",
      values_fill = 0
    ) %>%
    mutate(time_tot = time_tot_dssf + time_tot_prod)

  driver_pop <- args %>%
    pull(home_beats) %>%
    bind_rows() %>%
    count(home_beat, src, name = "pop_driver") %>%
    pivot_wider(
      names_from = src,
      values_from = pop_driver,
      names_prefix = "pop_driver_",
      values_fill = 0
    ) %>%
    mutate(pop_driver = pop_driver_dssf + pop_driver_prod)

  total_time %<>%
    left_join(driver_pop, by = "home_beat") %>%
    mutate(avg_time = time_tot / pop_driver)

  # Write the results
  write_rds(total_time, path(ROOT, "data", "adj_matrix", "adj_matrix.rds"))

  total_time
}

load_adj_matrix_data <- function(src) {
  # Loads data, and filters to records corresponding to active CMT users.

  trips_fn <- path(ROOT, "data", "adj_matrix", "start_end_{ src }.parquet") %>%
    glue()
  paths_fn <- path(ROOT, "data", "adj_matrix", "stanford_{ src }_trips.parquet") %>%
    glue()

  trips <- read_parquet(trips_fn) %>%
    mutate(src = {{ src }})
  paths <- read_parquet(paths_fn) %>%
    rename_with(~ "total_geo_time", any_of("total_fc_geo_time")) %>%
    mutate(
      src = {{ src }},
      total_geo_time = vctrs::vec_cast(total_geo_time, double())
    ) %>%
    select(geoid, fc, total_geo_time, drive_hash, src)

  valid_drivers <- trips %>%
    count(user_hash) %>%
    filter(n >= 10)

  valid_trips <- semi_join(trips, valid_drivers, by = "user_hash")
  valid_paths <- semi_join(paths, valid_trips, by = "drive_hash")

  #COMPUTE NUMBERS FOR PAPER
  n_valid_drivers <- valid_drivers %>% nrow()
  n_drivers <- trips %>% count(user_hash) %>% nrow()
  n_valid_trips <- valid_trips %>% nrow()
  n_trips <- trips %>% nrow()
  print(glue('{n_valid_drivers} valid drivers, {n_drivers} total drivers, {n_valid_trips} valid trips, and {n_trips} total trips'))
  #PROD: 24035 valid drivers, 51710 total drivers, 2819336 valid trips, and 2904266 total trips
  #DSSF: 115349 valid drivers, 218536 total drivers, 22349461 valid trips, and 22674501 total trips
  #TOTAL: 139384 valid drivers, 270246 total drivers, 25168797 valid trips, and 25578767 total trips
  #52% of drivers, 98% of trips
  
  list(trips = valid_trips, paths = valid_paths)
}

time_by_home_beat <- function(src, trips, paths, home_beats) {
  # Annotates paths with the home beat of the driver, as long as the length of
  # the driver's active period.

  trips %>%
    select(drive_hash, user_hash, src) %>%
    left_join(home_beats, by = c("user_hash", "src")) %>%
    left_join(paths, by = c("drive_hash", "src")) %>%
    rename(transit_beat = geoid) %>%
    mutate(src = {{ src }})
}

impute_driver_info <- function(trips) {
  # Imputes home beat as follows: for any given 24 hour period, starting at
  # noon, take the beat which bookends the longest break. We'll assume this is
  # where the individual parked their car while sleeping. Their home beat is the
  # beat where their car is most frequently parked while asleep.
  # NOTE: Also imputes length of active period.

  trips %>%
    group_by(user_hash) %>%
    group_modify(impute_parking_and_active) %>%
    ungroup()
}

impute_parking_and_active <- function(.x, .y) {
  .x %>%
    arrange(trip_start) %>%
    mutate(
      # Get parked start and end times.
      park_start = trip_end,
      park_end = lead(trip_start),
      # Impute beat where parked
      park_beat_start = beat_end,
      # Assuming the driver doesn't drive the next day, the last beat should be
      # the same as the beat where the car was last parked.
      park_beat_end = lead(beat_start),
      park_beat = if_else(
        condition = park_beat_start == park_beat_end,
        true = park_beat_start,
        false = na_chr
      ),
      # Get length of time parked at beat.
      parked_duration = as.duration(park_start %--% park_end),
      # Get duration of period during which the user was active.
      # NOTE: We rely on the trips being ordered to avoid an extra traversal.
      active_yrs = as.duration(last(trip_end) - first(trip_start)) / dyears(1)
    ) %>%
    group_by(home_beat = park_beat, active_yrs) %>%
    summarize(tot_duration = sum(parked_duration), .groups = "drop") %>%
    arrange(desc(tot_duration)) %>%
    # Get home beat and length of active period.
    summarize(across(c(home_beat, active_yrs), first))
}
