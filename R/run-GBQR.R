library(tibble)
library(dplyr)
library(readr)

# === Load FluNet data ===
df <- read_csv("../NCIRD-GIB-FluNET-Forecasting/target-data/flunet-preferred.csv")

# === Load forecasting functions ===
source("R/GBQR-function.R")

# === Derive parameters from weekly_forecast.R ===
if (!exists("forecast_date") || !exists("country_parm_table")) {
  stop("❌ Required global objects (`forecast_date`, `country_parm_table`) not found. Make sure to run from weekly_forecast.R.")
}

ref_date <- forecast_date + 3
date_str <- format(ref_date, "%Y-%m-%d")
countries <- country_parm_table$country

drop_horizon_by_country <- setNames(
  lapply(country_parm_table$horizons_dropped, function(n) {
    if (is.na(n) || n == 0) NULL else seq(-n, -1)
  }),
  country_parm_table$country
)

# Create max_horizon vector where 0 → 5, all others → n + 4
max_horizon_vec <- ifelse(
  is.na(country_parm_table$horizons_dropped),
  NA_integer_,
  ifelse(country_parm_table$horizons_dropped == 0, 5, country_parm_table$horizons_dropped + 4)
)

max_horizon_by_country <- setNames(
  max_horizon_vec,
  country_parm_table$country
)


in_season_weeks <- list(
  "Australia"    = list(c(10, 40)),
  "Brazil"       = list(c(8, 30)),
  "South Africa" = list(c(12, 45)),
  "Chile"        = list(c(12, 28), c(35, 52)),
  "Thailand"     = list(c(1, 52))
)

xmas_week_by_country <- list(
  "Australia"    = 25,
  "Brazil"       = 25,
  "South Africa" = 25,
  "Chile"        = c(22, 43),
  "Thailand"     = c(22, 43)
)

delta_offsets <- list(
  "Australia"    = 3,
  "Brazil"       = 7,
  "South Africa" = 5,
  "Chile"        = c(5, 5),
  "Thailand"     = c(18, 11)
)

# === Preprocess data
pop_db <- bind_rows(Australia_pop, Brazil_pop, Chile_pop, South_Africa_pop, Thailand_pop)
country_data_list <- split_and_enrich_flunet_by_country(df, pop_db)

# === Run forecasts ===
forecast_results <- lapply(countries, function(cty) {
  message("📦 Running forecast for: ", cty)
  run_gbqr_pipeline(
    country = cty,
    date_str = date_str,
    ref_date = ref_date
  )
})

names(forecast_results) <- countries
forecast_results <- Map(function(df, cty) {
  df$country <- cty
  df
}, forecast_results, names(forecast_results))

combined_forecast_df <- bind_rows(forecast_results)

# === Save output ===
output_dir <- "processed-data/rt-forecasts"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_path <- file.path(output_dir, paste0(format(ref_date, "%Y-%m-%d"), "-UGA_flucast-GBQR.csv"))
write_csv(combined_forecast_df, file_path)

message("✅ Forecast saved to: ", file_path)
