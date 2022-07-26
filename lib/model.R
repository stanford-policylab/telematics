library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))

source(path(ROOT, "lib", "common.R"))

################################### MODELING ###################################

join_stops_and_driving <- function(
  stops,
  driving,
  method = "relative"
) {
  # Joins stop data and telematics data into a dataframe suitable for modeling.
  # Params:
  #   `stops`: The dataframe of stops. Should contain at least the following
  #   columns: `beat`, `year`, `pop_tot`, `pct_nw`, `n_stops`.
  #   `driving`: The dataframe of driving behavior. Should contain at leasf the
  #   following columns: `geoid`, `sub_type`, `fc`, `speed_time`, `total_time`.
  #   `method`: Whether to compute absolute speeding buckes (KPH over limit) or
  #   relative speeding buckes (percent over limit).

  driving %<>%
    pivot_driving(method = method) %>%
    group_by(beat) %>%
    # Convert to proportion and standardize
    summarize(
      total = sum(total),
      across(starts_with("sp_"), ~ sum(.) / total),
      .groups = "drop"
    ) %>%
    mutate(across(starts_with("sp_"), ~ ., .names = "raw_{.col}")) %>%
    # Normalize speeding bins.
    mutate(
      across(starts_with("sp_"), ~ (. - mean(.)) / sd(.)),
      # Convert NaNs from constant columns, if there are any, to 0.
      across(starts_with("sp_"), ~ if_else(is.nan(.), 0, .))
    )

  stops %>%
    left_join(driving) %>%
    mutate(year = factor(year, levels = 2013:2019))
}

speeding_ccdf <- function(
  driving,
  method = "absolute",
  disaggregate = character(0)
) {
  # Generates the CCDF of speeding.
  # NOTE: Will disaggregate by column names passed to the disaggregate param.

  driving %>%
    mutate(
      sub_type = str_replace_all(sub_type, "[^a-z0-9\\-\\+]+", "_"),
      sub_type = str_replace(sub_type, ".+_", ""),
      sub_type = str_replace(sub_type, "0\\+", "5"),
      sub_type =  str_replace(sub_type, "neg_", "-"),
      speed = as.numeric(sub_type),
      speed = if_else(
        condition = rep_along(sub_type, {{ method }} == "absolute"),
      # Impute middle of buckets (subtract 2.5 KPH).
        true = 5 * round((speed + speed_limit_kmh - 2.5) / 5),
        false = speed - 2.5
      )
    ) %>%
    group_by(across(c(any_of({{ disaggregate }}), speed))) %>%
    summarize(density = sum(speed_time), .groups = "drop_last") %>%
    arrange(speed, .by_group = TRUE) %>%
    mutate(density = cumsum(density) / sum(density)) %>%
    ungroup()
}

pivot_driving <- function(driving, method = "absolute") {
  # NOTE: This recursive source is OK because of lazy evaluation and the fact
  # that `pivot_driving` is not recursive. It would be nice if `R` namespacing
  # didn't require this...
  source(path(ROOT, "lib", "model.R"), local = TRUE)

  f <- current_env()[[str_c("pivot_driving_", method)]]

  if (is_null(f)) {
    abort("Method must be 'absolute' or 'relative'")
  }

  f(driving)
}

pivot_driving_absolute <- function(drivers) {
  # Bin driving according to the absolute amount by which they speed.

  driving %>%
    mutate(sub_type = str_replace_all(sub_type, "[^a-z0-9\\-\\+]+", "_")) %>%
    pivot_wider(
      id_cols = c(beat, fc, speed_limit_kmh, any_of("city")),
      names_from = sub_type,
      values_from = speed_time,
      values_fill = 0
    ) %>%
    # Make speeding values cumulative.
    mutate(
      sp_over_30 = `speeding_30+`,
      sp_over_25 = sp_over_30 + `speeding_25_to_30`,
      sp_over_20 = sp_over_25 + `speeding_20_to_25`,
      sp_over_15 = sp_over_20 + `speeding_15_to_20`,
      sp_over_10 = sp_over_15 + `speeding_10_to_15`,
      sp_over_5 = sp_over_10 + `speeding_5_to_10`,
      sp_over_0 = sp_over_5 + `speeding_0_to_5`,
      sp_over_neg_5 = sp_over_0 + `speeding_-5_to_0`,
      sp_over_neg_10 = sp_over_neg_5 + `speeding_-10_to_-5`,
      total = sp_over_neg_10 + `speeding_below_-10`
    ) %>%
    select(-starts_with("speeding_"))
}

pivot_driving_relative <- function(drivers) {
  # Bin driving according to how much they speed *relative* to the speed limit.
  levels <- c(
    "total",
    str_replace(glue("sp_over_{ (-10):10 * 10 }_pct"), "-", "neg_")
  )

  # Pivot drives
  driving %<>%
    mutate(
      sub_type = str_replace_all(sub_type, "[^a-z0-9\\-\\+]+", "_"),
      sub_type = str_replace(sub_type, ".+_", ""),
      sub_type = str_replace(sub_type, "0\\+", "5"),
      # Approximate speed in bins by centerpoint.
      sub_type = as.integer(sub_type) - 2.5,
      # Calculate relative speed.
      sub_type = 100 * sub_type / speed_limit_kmh,
      # Group in 10% bins.
      sub_type = cut(
        sub_type,
        breaks = c(-Inf, (-10):10 * 10, Inf),
        labels = levels,
        include.lowest = TRUE
      )
    ) %>%
    group_by(across(c(beat, fc, speed_limit_kmh, sub_type, any_of("city")))) %>%
    summarize(speed_time = sum(speed_time), .groups = "drop") %>%
    pivot_wider(
      id_cols = c(beat, fc, speed_limit_kmh, any_of("city")),
      names_from = sub_type,
      values_from = speed_time,
      values_fill = 0
    )

    # Impute missing columns.
    missing_cols <- setdiff(levels, colnames(driving))
    missing_cols <- set_names(rep_along(missing_cols, 0), missing_cols)
    driving %<>% add_column(!!!missing_cols)

    driving %>%
      mutate(
        sp_over_100_pct = sp_over_100_pct,
        sp_over_90_pct = sp_over_90_pct + sp_over_100_pct,
        sp_over_80_pct = sp_over_80_pct + sp_over_90_pct,
        sp_over_70_pct = sp_over_70_pct + sp_over_80_pct,
        sp_over_60_pct = sp_over_60_pct + sp_over_70_pct,
        sp_over_50_pct = sp_over_50_pct + sp_over_60_pct,
        sp_over_40_pct = sp_over_40_pct + sp_over_50_pct,
        sp_over_30_pct = sp_over_30_pct + sp_over_40_pct,
        sp_over_20_pct = sp_over_20_pct + sp_over_30_pct,
        sp_over_10_pct = sp_over_10_pct + sp_over_20_pct,
        sp_over_0_pct = sp_over_0_pct + sp_over_10_pct,
        sp_over_neg_10_pct = sp_over_neg_10_pct + sp_over_0_pct,
        sp_over_neg_20_pct = sp_over_neg_20_pct + sp_over_neg_10_pct,
        sp_over_neg_30_pct = sp_over_neg_30_pct + sp_over_neg_20_pct,
        sp_over_neg_40_pct = sp_over_neg_40_pct + sp_over_neg_30_pct,
        sp_over_neg_50_pct = sp_over_neg_50_pct + sp_over_neg_40_pct,
        sp_over_neg_60_pct = sp_over_neg_60_pct + sp_over_neg_50_pct,
        sp_over_neg_70_pct = sp_over_neg_70_pct + sp_over_neg_60_pct,
        sp_over_neg_80_pct = sp_over_neg_80_pct + sp_over_neg_70_pct,
        sp_over_neg_90_pct = sp_over_neg_90_pct + sp_over_neg_80_pct,
        sp_over_neg_100_pct = sp_over_neg_100_pct + sp_over_neg_90_pct,
        total = total + sp_over_neg_100_pct
      ) %>%
      select(beat, fc, speed_limit_kmh, any_of("city"), any_of(levels))
}
