library(tibble)
library(dplyr)


compute_max_horizon_by_country <- function(drop_horizon_by_country, countries, base_horizon = 4, default_horizon = 5) {
  # Step 1: Compute max horizon for countries with drop info
  max_horizon_list <- lapply(drop_horizon_by_country, function(drop_range) {
    n_future_drops <- sum(drop_range < 0)
    base_horizon + n_future_drops
  })
  
  # Step 2: Add countries with no drop info using default_horizon
  missing_countries <- setdiff(countries, names(max_horizon_list))
  for (cty in missing_countries) {
    max_horizon_list[[cty]] <- default_horizon
  }
  
  return(max_horizon_list)
}

##############################################################################################
# Step 1: Manually enter Australia population
Australia_pop <- tibble(
  year = 2000:2025,
  population = c(
    19017963, 19248143, 19475844, 19698999, 19925056, 20171731, 20467030, 20830828,
    21247873, 21660892, 22019168, 22357034, 22729269, 23111782, 23469579, 23820236,
    24195701, 24590334, 24979230, 25357170, 25670051, 25921089, 26177413, 26439111,
    26699482, 26958054
  ),
  growth_rate = c(
    0.0115, 0.0121, 0.0118, 0.0115, 0.0115, 0.0124, 0.0146, 0.0178,
    0.0200, 0.0194, 0.0165, 0.0153, 0.0166, 0.0168, 0.0155, 0.0149,
    0.0158, 0.0163, 0.0158, 0.0151, 0.0123, 0.0098, 0.0099, 0.0100,
    0.0098, 0.0097
  ),
  country = "Australia"
)

# Step 2: Repeat for Brazil
Brazil_pop <- tibble(
  year = 2000:2025,
  population = c(
    175873720, 178211881, 180476685, 182629278, 184722043, 186797334, 188820682, 190779453,
    192672317, 194517549, 196353492, 198185302, 199977707, 201721767, 203459650, 205188205,
    206859578, 208504960, 210166592, 211782878, 213196304, 214326223, 215313498, 216422446,
    217637297, 218803058
  ),
  growth_rate = c(
    0.0138, 0.0133, 0.0127, 0.0119, 0.0115, 0.0112, 0.0108, 0.0104,
    0.0099, 0.0096, 0.0094, 0.0093, 0.0090, 0.0087, 0.0086, 0.0085,
    0.0081, 0.008, 0.008, 0.0077, 0.0067, 0.0053, 0.0046, 0.0052,
    0.0056, 0.0054
  ),
  country = "Brazil"
)

# Step 3: Chile
Chile_pop <- tibble(
  year = 2000:2025,
  population = c(
    15351799, 15523978, 15693790, 15859112, 16017966, 16175311, 16334575, 16495538,
    16661462, 16833447, 17004162, 17173573, 17341771, 17509925, 17687108, 17870124,
    18083879, 18368577, 18701450, 19039485, 19300315, 19493184, 19603733, 19629590,
    19658839, 19690323
  ),
  growth_rate = c(
    0.0116, 0.0112, 0.0109, 0.0105, 0.0100, 0.0098, 0.0098, 0.0099,
    0.0101, 0.0103, 0.0101, 0.0100, 0.0098, 0.0097, 0.0101, 0.0103,
    0.0120, 0.0157, 0.0181, 0.0181, 0.0137, 0.0100, 0.0057, 0.0013,
    0.0015, 0.0016
  ),
  country = "Chile"
)

# Step 4: South Africa
South_Africa_pop <- tibble(
  year = 2000:2025,
  population = c(
    46813266, 47229714, 47661514, 48104048, 48556071, 49017147, 49491756, 49996094,
    50565812, 51170779, 51784921, 52443325, 53145033, 53873616, 54729551, 55876504,
    56422274, 56641209, 57339635, 58087055, 58801927, 59392255, 59893885, 60414495,
    61020221, 61673081
  ),
  growth_rate = c(
    0.0097, 0.0089, 0.0091, 0.0093, 0.0094, 0.0095, 0.0097, 0.0102,
    0.0114, 0.0120, 0.0120, 0.0127, 0.0134, 0.0137, 0.0159, 0.0210,
    0.0098, 0.0039, 0.0123, 0.0130, 0.0123, 0.0100, 0.0084, 0.0087,
    0.0100, 0.0107
  ),
  country = "South Africa"
)

# Step 5: Thailand
Thailand_pop <- tibble(
  year = 2000:2025,
  population = c(
    63066603, 63649892, 64222580, 64776956, 65311166, 65821360, 66319525, 66826754,
    67328239, 67813654, 68270489, 68712846, 69157023, 69578602, 69960943, 70294397,
    70607037, 70898202, 71127802, 71307763, 71475664, 71601103, 71697030, 71801279,
    71885799, 71953054
  ),
  growth_rate = c(
    0.0100, 0.0092, 0.0090, 0.0086, 0.0082, 0.0078, 0.0076, 0.0076,
    0.0075, 0.0072, 0.0067, 0.0065, 0.0065, 0.0061, 0.0055, 0.0048,
    0.0044, 0.0041, 0.0032, 0.0025, 0.0024, 0.0018, 0.0013, 0.0015,
    0.0012, 0.0009
  ),
  country = "Thailand"
)

# Step 6: Combine all into a single master database
pop_db <- bind_rows(
  Australia_pop,
  Brazil_pop,
  Chile_pop,
  South_Africa_pop,
  Thailand_pop
)

# Step 7: Preview and Save
print(pop_db, n = 20)
#####################################################################
split_and_enrich_flunet_by_country <- function(df, pop_db) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  # Step 1: Filter COVID-affected years
  df_clean <- df %>%
    filter(!(
      (COUNTRY == "Australia" & ISO_YEAR %in% c(2020, 2021)) |
        (COUNTRY == "Brazil"    & ISO_YEAR %in% c(2020, 2021, 2022)) |
        (COUNTRY == "Chile"     & ISO_YEAR %in% c(2020, 2021)) |
        (COUNTRY == "Thailand"  & ISO_YEAR == 2021)
    ))
  
  # Step 2: Aggregate Brazil and Chile
  df_combined <- df_clean %>%
    filter(COUNTRY %in% c("Brazil", "Chile"), ISO_YEAR >= 2015) %>%
    group_by(COUNTRY, ISO_SDATE, ISO_YEAR, ISO_WEEK) %>%
    summarise(
      INF_ALL = sum(INF_ALL, na.rm = TRUE),
      TOTAL = sum(TOTAL, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ORIGIN_SOURCE = "Combined_Source")
  
  df_others <- df_clean %>%
    filter(!COUNTRY %in% c("Brazil", "Chile"), ISO_YEAR >= 2015)
  
  # Step 3: Final merged dataset
  final_df <- bind_rows(df_combined, df_others) %>%
    arrange(COUNTRY, ISO_SDATE) %>%
    drop_na()
  
  # Step 4: No normalization – preserve case as-is
  
  # Step 5: Join with population and format
  final_joined <- final_df %>%
    left_join(pop_db, by = c("COUNTRY" = "country", "ISO_YEAR" = "year")) %>%
    rename(
      target_end_date = ISO_SDATE,
      year = ISO_YEAR,
      week = ISO_WEEK,
      count = INF_ALL
    ) %>%
    mutate(
      region = COUNTRY,
      season = year,
      season_week = week
    ) %>%
    select(region, target_end_date, year, week, season, season_week,
           count, population, growth_rate)
  
  # Step 6: Split by region
  country_dfs <- split(final_joined, final_joined$region)
  
  return(country_dfs)
}

#######
load_and_process_country_data <- function(
    country,
    raw_data_dir = "raw-data",
    date_str = format(Sys.Date(), "%Y-%m-%d"),
    drop_horizon_range = NULL,
    ref_date = Sys.Date()  # Default fallback to today's date if not passed
) {
  library(dplyr)
  library(lubridate)
  
  country_key <- country  # preserve case
  
  if (!country_key %in% names(country_data_list)) {
    stop("❌ Country data not found in `country_data_list`: ", country)
  }
  
  dat <- country_data_list[[country_key]] %>%
    arrange(target_end_date) %>%
    rename(country = region) %>%
    select(-season, -season_week) %>%
    mutate(
      inc = count / (population / 100000),
      wk_end_date = as.Date(target_end_date),
      location = country,
      pop = population
    ) %>%
    select(wk_end_date, location, inc, pop)
  
  if (!is.null(drop_horizon_range)) {
    drop_dates <- as.Date(ref_date) + (7 * drop_horizon_range)
    dat <- dat %>% filter(!(wk_end_date %in% drop_dates))
    cat("🗑️ Dropped horizons:", paste(drop_horizon_range, collapse = ", "),
        "\n📅 Corresponding dates:", paste(format(drop_dates), collapse = ", "), "\n")
  }
  
  df <- dat %>%
    mutate(
      epiweek = isoweek(wk_end_date),
      year = year(wk_end_date),
      season = year(wk_end_date),
      season_week = epiweek
    )
  
  cat("📆 Latest wk_end_date for", country, ":", format(max(df$wk_end_date)), "\n")
  return(df)
}

####
####################################
transform_incidence_features <- function(df, in_season_weeks) {
  library(dplyr)
  
  if (!"location" %in% colnames(df)) stop("df must contain a 'location' column.")
  
  df <- df %>%
    mutate(
      log_pop = log(pop),
      inc_4rt = (inc + 0.01)^0.25
    )
  
  df <- df %>%
    rowwise() %>%
    mutate(
      in_season = {
        loc_weeks <- in_season_weeks[[location]]
        if (is.null(loc_weeks)) FALSE
        else any(sapply(loc_weeks, function(range) season_week >= range[1] & season_week <= range[2]))
      }
    ) %>%
    ungroup()
  
  df <- df %>%
    group_by(location) %>%
    mutate(
      inc_4rt_scale_factor = quantile(inc_4rt[in_season], probs = 0.95, na.rm = TRUE),
      inc_4rt_cs = inc_4rt / (inc_4rt_scale_factor + 0.01),
      inc_4rt_center_factor = mean(inc_4rt_cs[in_season], na.rm = TRUE),
      inc_4rt_cs = inc_4rt_cs - inc_4rt_center_factor
    ) %>%
    ungroup()
  
  return(df)
}

######################################

plot_country_incidence <- function(df, country_name) {
  library(ggplot2)
  library(dplyr)
  
  df %>%
    filter(location == country_name) %>%
    mutate(season_loc = paste0(season, "_", location)) %>%
    ggplot(aes(x = season_week, y = inc_4rt_cs, group = season_loc, color = season_loc)) +
    geom_line(alpha = 0.8) +
    labs(
      title = country_name,
      x = "Season Week",
      y = "Centered & Scaled Incidence"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10)
    ) +
    guides(color = guide_legend(nrow = 3))
}

##########################################

############################################################################
prepare_features_and_targets <- function(df, max_horizon, xmas_week, delta_offsets) {
  library(dplyr)
  library(slider)
  library(purrr)
  library(tibble)
  
  # Ensure 'location' is a factor
  df <- df %>% mutate(location = as.factor(location))
  
  # === Step 1: Assign offset value(s) per location as list-column ===
  df <- df %>%
    rowwise() %>%
    mutate(delta_offset_vals = list(delta_offsets[[as.character(location)]])) %>%
    ungroup()
  
  # === Step 2: Compute delta_xmas features ===
  if (length(xmas_week) == 1) {
    df <- df %>%
      mutate(
        delta_base_week = xmas_week - map_dbl(delta_offset_vals, 1),
        delta_xmas = season_week - delta_base_week
      ) %>%
      select(-delta_offset_vals)
    
    feat_names <- c("inc_4rt_cs", "season_week", "log_pop", "delta_xmas")
    
  } else if (length(xmas_week) == 2) {
    df <- df %>%
      mutate(
        delta_base_week_1 = xmas_week[1] - map_dbl(delta_offset_vals, 1),
        delta_base_week_2 = xmas_week[2] - map_dbl(delta_offset_vals, 2),
        delta_xmas_first_peak  = season_week - delta_base_week_1,
        delta_xmas_second_peak = season_week - delta_base_week_2
      ) %>%
      select(-delta_offset_vals, -delta_base_week_1, -delta_base_week_2)
    
    feat_names <- c("inc_4rt_cs", "season_week", "log_pop",
                    "delta_xmas_first_peak", "delta_xmas_second_peak")
  }
  
  # === Helper: Add Taylor features ===
  add_taylor_features <- function(df, degree, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      df <- df %>%
        group_by(location) %>%
        arrange(wk_end_date, .by_group = TRUE) %>%
        mutate(
          taylor_list = slide(
            .x = .data[[var]],
            .f = ~ {
              if (length(.x) < degree + 1 || all(is.na(.x))) return(rep(NA_real_, degree + 1))
              X <- outer(0:(length(.x) - 1), 0:degree, `^`)
              tryCatch(qr.solve(X, .x), error = function(e) rep(NA_real_, degree + 1))
            },
            .before = w - 1,
            .complete = TRUE
          )
        ) %>%
        ungroup()
      
      coef_cols <- paste0(var, "_taylor_d", degree, "_c", 0:degree, "_w", w, "t_sNone")
      coef_mat <- do.call(rbind, lapply(df$taylor_list, function(x) if (length(x) == degree + 1) x else rep(NA_real_, degree + 1)))
      df <- df %>% select(-taylor_list)
      df[coef_cols] <- as_tibble(coef_mat)
      feat_names <- c(feat_names, coef_cols)
    }
    list(df = df, feat_names = feat_names)
  }
  
  # === Helper: Add rolling mean features ===
  add_rollmean_features <- function(df, window_sizes, var = "inc_4rt_cs", feat_names) {
    for (w in window_sizes) {
      name <- paste0(var, "_rollmean_w", w)
      df <- df %>%
        group_by(location) %>%
        arrange(wk_end_date, .by_group = TRUE) %>%
        mutate(!!name := slide_dbl(.data[[var]], mean, .before = w - 1, .complete = TRUE))
      feat_names <- c(feat_names, name)
    }
    list(df = df, feat_names = feat_names)
  }
  
  # === Helper: Add lag features ===
  add_lag_features <- function(df, lag_vars, lags, feat_names) {
    for (v in lag_vars) {
      for (l in lags) {
        name <- paste0(v, "_lag", l)
        df <- df %>%
          group_by(location) %>%
          arrange(wk_end_date, .by_group = TRUE) %>%
          mutate(!!name := lag(.data[[v]], n = l))
        feat_names <- c(feat_names, name)
      }
    }
    list(df = df, feat_names = feat_names)
  }
  
  # === Step 3: Apply feature engineering ===
  res <- add_taylor_features(df, degree = 2, window_sizes = c(4, 6), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names
  
  res <- add_taylor_features(df, degree = 1, window_sizes = c(3, 5), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names
  
  res <- add_rollmean_features(df, window_sizes = c(2, 4), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names
  
  intermediate_feats <- setdiff(feat_names, c("inc_4rt_cs", "season_week", "log_pop", 
                                              "delta_xmas", 
                                              "delta_xmas_first_peak", "delta_xmas_second_peak"))
  res <- add_lag_features(df, lag_vars = c("inc_4rt_cs", intermediate_feats), lags = c(1, 2), feat_names = feat_names)
  df <- res$df; feat_names <- res$feat_names
  
  # === Step 4: Create long-format multi-horizon targets ===
  horizons <- 1:max_horizon
  df_targets_long <- map_dfr(horizons, function(h) {
    df %>%
      group_by(location) %>%
      arrange(wk_end_date, .by_group = TRUE) %>%
      mutate(
        horizon = h,
        inc_4rt_cs_target = lead(inc_4rt_cs, h),
        delta_target = inc_4rt_cs_target - inc_4rt_cs
      ) %>%
      ungroup()
  })
  
  feat_names <- c(feat_names, "horizon")
  
  return(list(
    df_with_features = df,
    target_long = df_targets_long,
    feature_names = feat_names
  ))
}


######################################
filter_targets_for_training <- function(
    target_df,
    ref_date,
    in_season_weeks,
    drop_missing_targets = TRUE
) {
  library(dplyr)
  library(lubridate)
  library(purrr)
  
  df <- target_df
  
  # Step 1: Drop rows with NA targets if requested
  if (drop_missing_targets) {
    df <- df %>% filter(!is.na(inc_4rt_cs_target))
  }
  
  # Step 2: Filter by in-season weeks per location
  df <- df %>%
    rowwise() %>%
    filter({
      loc <- as.character(location)
      week <- season_week
      week_ranges <- in_season_weeks[[loc]]
      any(sapply(week_ranges, function(rng) week >= rng[1] & week <= rng[2]))
    }) %>%
    ungroup()
  
  # Step 3: Drop 'epiweek' column if present
  df <- df %>% select(-any_of("epiweek"))
  
  # Step 4: Filter by reference date
  df <- df %>% filter(wk_end_date < as.Date(ref_date))
  
  cat("✅ Filtered target dataframe using in_season_weeks\n")
  cat("  • Rows:", nrow(df), "Columns:", ncol(df), "\n")
  
  return(df)
}

######################################
split_train_test <- function(
    df_with_pred_targets,
    feat_names,
    ref_date,
    season_week_windows
) {
  library(dplyr)
  
  # Step 1: Filter by all valid season_week windows
  df_filtered <- bind_rows(lapply(season_week_windows, function(w) {
    df_with_pred_targets %>%
      filter(
        season_week >= w[1],
        season_week <= w[2],
        wk_end_date < as.Date(ref_date)
      )
  }))
  
  # Step 2: Get the test set (latest date)
  df_test <- df_filtered %>%
    filter(wk_end_date == max(wk_end_date, na.rm = TRUE))
  
  x_test <- df_test %>% select(all_of(feat_names))
  
  # Step 3: Get training set (non-NA delta_target)
  df_train <- df_filtered %>% filter(!is.na(delta_target))
  x_train <- df_train %>% select(all_of(feat_names))
  y_train <- df_train$delta_target
  
  cat("✅ Train size:", nrow(df_train), "| Test size:", nrow(df_test), "\n")
  
  return(list(
    df_train = df_train,
    x_train = x_train,
    y_train = y_train,
    df_test = df_test,
    x_test = x_test
  ))
}

############################################################################
run_quantile_lgb_bagging <- function(x_train, y_train, x_test, df_train, feat_names, ref_date,
                                     num_bags,
                                     q_levels ,
                                     bag_frac_samples,
                                     nrounds) {
  library(lightgbm)
  library(dplyr)
  library(lubridate)
  library(matrixStats)
  
  # Set reproducible seed from reference date
  rng_seed <- as.numeric(as.POSIXct(ref_date))
  set.seed(rng_seed)
  
  # Generate seeds for bagging and quantile levels
  lgb_seeds <- matrix(
    floor(runif(num_bags * length(q_levels), min = 1, max = 1e8)),
    nrow = num_bags,
    ncol = length(q_levels)
  )
  
  # Allocate prediction arrays
  oob_preds_by_bag <- array(NA_real_, dim = c(nrow(x_train), num_bags, length(q_levels)))
  test_preds_by_bag <- array(NA_real_, dim = c(nrow(x_test), num_bags, length(q_levels)))
  
  # Identify training seasons
  train_seasons <- unique(df_train$season)
  
  # Track feature importance
  feature_importance_df <- matrix(0, nrow = length(feat_names), ncol = num_bags * length(q_levels))
  rownames(feature_importance_df) <- feat_names
  
  for (b in seq_len(num_bags)) {
    cat("Bag number", b, "\n")
    
    # Sample a subset of seasons
    bag_seasons <- sample(train_seasons, size = floor(length(train_seasons) * bag_frac_samples), replace = FALSE)
    bag_obs_inds <- df_train$season %in% bag_seasons
    
    for (q_ind in seq_along(q_levels)) {
      q_level <- q_levels[q_ind]
      col_index <- (b - 1) * length(q_levels) + q_ind
      
      # Prepare LightGBM dataset
      dtrain <- lgb.Dataset(data = as.matrix(x_train[bag_obs_inds, ]), label = y_train[bag_obs_inds])
      
      # Train LightGBM model
      model <- lgb.train(
        params = list(
          objective = "quantile",
          alpha = q_level,
          verbosity = -1,
          seed = lgb_seeds[b, q_ind]
        ),
        data = dtrain,
        nrounds = nrounds
      )
      
      # Predict on test set
      test_preds_by_bag[, b, q_ind] <- predict(model, newdata = as.matrix(x_test))
      
      # Feature importance (Gain-based)
      importance <- lgb.importance(model)
      matched <- match(feat_names, importance$Feature)
      feature_importance_df[, col_index] <- ifelse(!is.na(matched), importance$Gain[matched], 0)
    }
  }
  
  return(list(
    test_preds_by_bag = test_preds_by_bag,
    oob_preds_by_bag = oob_preds_by_bag,  # currently unused but reserved
    feature_importance = feature_importance_df,
    lgb_seeds = lgb_seeds
  ))
}

plot_top_feature_importance <- function(feature_importance_df, top_n = 20) {
  library(dplyr)
  library(ggplot2)
  
  # Move rownames to a column if necessary
  if (is.null(feature_importance_df$feature)) {
    feature_importance_df <- feature_importance_df %>%
      mutate(feature = rownames(.))
  }
  
  # Compute average importance across all numeric columns
  feature_importance_df <- feature_importance_df %>%
    rowwise() %>%
    mutate(average_importance = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup()
  
  # Get top N features by average importance
  top_features <- feature_importance_df %>%
    arrange(desc(average_importance)) %>%
    slice_head(n = top_n)
  
  # Plot
  ggplot(top_features, aes(x = average_importance, y = reorder(feature, average_importance))) +
    geom_bar(stat = "identity") +
    labs(
      title = paste0("Top ", top_n, " Most Important Features"),
      x = "Average Gain",
      y = "Feature"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold")
    )
}
#######
process_gbqr_forecasts_to_df <- function(
    test_preds_by_bag,
    df_test,
    q_labels,
    country_code,
    ref_date
) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  
  # Step 1: Take median across bags [obs x quantiles]
  test_pred_qs <- apply(test_preds_by_bag, c(1, 3), median, na.rm = TRUE)
  
  # Step 2: Sort quantile predictions (ensure monotonicity)
  test_pred_qs_sorted <- t(apply(test_pred_qs, 1, sort))
  
  # Step 3: Convert to data frame and label quantiles
  test_pred_qs_df <- as.data.frame(test_pred_qs_sorted)
  colnames(test_pred_qs_df) <- q_labels
  
  # Step 4: Bind predictions to test set
  df_test <- df_test %>% mutate(row_id = row_number())
  df_test_w_preds <- bind_cols(df_test, test_pred_qs_df)
  
  # Step 5: Reshape to long format
  cols_to_keep <- c("wk_end_date", "location", "pop", "inc_4rt_cs", "horizon", 
                    "inc_4rt_center_factor", "inc_4rt_scale_factor")
  
  preds_df <- df_test_w_preds %>%
    select(all_of(c(cols_to_keep, q_labels))) %>%
    pivot_longer(cols = all_of(q_labels), names_to = "quantile", values_to = "delta_hat")
  
  # Step 6: Reverse transform to raw incidence
  preds_df <- preds_df %>%
    mutate(
      inc_4rt_cs_target_hat = inc_4rt_cs + delta_hat,
      inc_4rt_target_hat = (inc_4rt_cs_target_hat + inc_4rt_center_factor) * (inc_4rt_scale_factor + 0.01),
      value = ((pmax(inc_4rt_target_hat, 0) ^ 4 - 0.01) * pop) / 100000,
      value = pmax(value, 0)
    )
  
  # Step 7: Final formatting
  preds_df <- preds_df %>%
    select(wk_end_date, location, horizon, quantile, value) %>%
    rename(output_type_id = quantile) %>%
    rename(country = location)
  
  # Step 8: Remap most recent 5 horizons to -1:3
  last5_horizons <- preds_df %>%
    distinct(horizon) %>%
    arrange(desc(horizon)) %>%
    slice(1:5) %>%
    arrange(horizon) %>%
    pull(horizon)
  
  horizon_map <- tibble(
    original = last5_horizons,
    new = -1:3
  )
  
  preds_df <- preds_df %>%
    filter(horizon %in% last5_horizons) %>%
    left_join(horizon_map, by = c("horizon" = "original")) %>%
    mutate(
      horizon = new,
      reference_date = ref_date,
      target_end_date = ref_date + weeks(horizon),
      target = "inc flu case",
      output_type = "quantile"
    ) %>%
    select(reference_date, target, horizon, target_end_date,
           country, output_type, output_type_id, value)
  
  cat("✅ Processed", nrow(preds_df), "rows for", country_code, "\n")
  
  return(preds_df)
}

run_gbqr_pipeline <- function(country, date_str, ref_date) {
  # Load and process data
  df <- load_and_process_country_data(
    country = country,
    date_str = date_str,
    ref_date = ref_date,
    drop_horizon_range = drop_horizon_by_country[[country]]
  )
  
  # Extract config
  xmas_week <- xmas_week_by_country[[country]]
  max_horizon <- max_horizon_by_country[[country]]
  delta_offset <- setNames(list(delta_offsets[[country]]), country)
  
  # Transform incidence features
  df <- transform_incidence_features(df, in_season_weeks)
  plot_country_incidence(df, country)
  
  # Prepare features and targets
  res <- prepare_features_and_targets(
    df = df,
    max_horizon = max_horizon,
    xmas_week = xmas_week,
    delta_offsets = delta_offset
  )
  
  df_feat <- res$df_with_features
  target_df <- res$target_long
  feat_names <- res$feature_names
  
  df_filtered <- filter_targets_for_training(
    target_df = target_df,
    ref_date = ref_date,
    in_season_weeks = in_season_weeks,
    drop_missing_targets = FALSE
  )
  
  season_week_windows <- in_season_weeks[[country]]
  
  # Train/test split
  split_data <- split_train_test(
    df_with_pred_targets = target_df,
    feat_names = feat_names,
    ref_date = ref_date,
    season_week_windows = season_week_windows
  )
  
  # Set bagging + quantile settings
  num_bags <- 100
  q_levels <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  q_labels <- as.character(q_levels)
  bag_frac_samples <- 0.7
  nrounds <- 100
  
  # Run model
  results <- run_quantile_lgb_bagging(
    x_train = split_data$x_train,
    y_train = split_data$y_train,
    x_test = split_data$x_test,
    df_train = split_data$df_train,
    feat_names = feat_names,
    ref_date = ref_date,
    num_bags = num_bags,
    q_levels = q_levels,
    bag_frac_samples = bag_frac_samples,
    nrounds = nrounds
  )
  
  # Feature importance
  feature_importance_df <- as.data.frame(results$feature_importance)
  plot_top_feature_importance(feature_importance_df, top_n = 20)
  
  # Format forecast
  forecast_df <- process_gbqr_forecasts_to_df(
    test_preds_by_bag = results$test_preds_by_bag,
    df_test = split_data$df_test,
    q_labels = q_labels,
    country_code = country,
    ref_date = ref_date
  )
  
  return(forecast_df)
}
