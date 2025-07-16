library(tidyverse)
library(INLA)
library(lubridate)
library(cowplot)
library(glue)

inla.setOption(inla.mode="classic")

###############
#Define Parameters 
###############
# country_parm_table <- tibble(
#   country           = c('Australia','Brazil','Chile','South Africa','Thailand'),
#   horizons_dropped  = c(1,           3,       2,      2,             0),
#   min_fcast_horizon = c(-1,          -1,      -1,     -1,            -1),
#   max_fcast_horizon = c(3,           3,       3,      3,             3)
# )

horizon_0 <- forecast_date + days(3)
horizon_neg_1 <- forecast_date - days(4) 

global_min <- min(country_parm_table$min_fcast_horizon)
global_max <- max(country_parm_table$max_fcast_horizon)
forecast_horizon_global <- global_max - global_min + 1

#############
#Read in and clean data 
#############
df <- read_csv('../NCIRD-GIB-FluNET-Forecasting/target-data/flunet-preferred.csv')

#df <- read.csv("/Users/maya/Desktop/2025_cdc/Script/flunet-preferred.csv")

####### Remove Covid Years
df <- df %>%
  filter(!(
    (COUNTRY == "Australia" & ISO_YEAR %in% c(2020, 2021)) |
      (COUNTRY == "Brazil"   & ISO_YEAR %in% c(2020, 2021, 2022)) |
      (COUNTRY == "Chile"     & ISO_YEAR %in% c(2020, 2021)) |
      (COUNTRY == "Thailand"  & ISO_YEAR == 2021)
  ))

####### Make combined column 

df_Brazil_Chile <- df %>%
  filter(COUNTRY %in% c("Brazil", "Chile"), ISO_YEAR >= 2015) %>%
  group_by(COUNTRY, ISO_SDATE, ISO_YEAR, ISO_WEEK) %>%
  summarise(
    INF_ALL = sum(INF_ALL),
    TOTAL = sum(TOTAL),
    .groups = "drop"
  ) %>%
  mutate(ORIGIN_SOURCE = "Combined_Source")

df_others <- df %>%
  filter(!COUNTRY %in% c("Brazil", "Chile"), ISO_YEAR >= 2015)

final_df <- bind_rows(df_Brazil_Chile, df_others) %>%
  arrange(COUNTRY, ISO_SDATE)

latest_dates <- final_df %>%
  group_by(COUNTRY) %>%
  summarise(latest_date = max(ISO_SDATE, na.rm = TRUE)) %>%
  ungroup()

#################
#Run Models
#################

########### #define model/fit data/ fit/ sample/ summarize
model <- model_formula("iid", temporal="ar1", spatial="iid") #normal

fit_df <- final_df %>%
  left_join(country_parm_table,
            by = c("COUNTRY" = "country")) %>%
  filter(ISO_SDATE < (horizon_0 - weeks(horizons_dropped))) %>%
  prep_fit_data(forecast_horizon_global)
  
fit <- fit_inla_model(fit_df, horizon_neg_1, model, pc_prior_u=c(1, 1))

pred_summ <- sample_count_predictions(fit_df, fit, forecast_date = horizon_neg_1, nsamp=5000) |>
  summarize_quantiles()

result <- hub_formatting(pred_summ) %>%
  mutate(horizon        = as.integer(horizon - 1),
         reference_date = reference_date + days(7))

# 8) Now join back your table and prune country‐by‐country
result_final <- result %>%
  left_join(country_parm_table %>% 
              select(country, min_fcast_horizon, max_fcast_horizon),
            by = "country") %>%
  filter(horizon >= min_fcast_horizon,
         horizon <= max_fcast_horizon) %>%
  select(-min_fcast_horizon, -max_fcast_horizon)


########### # Save files 

result |> 
  write_csv(paste0("processed-data/rt-forecasts/", horizon_0, "-UGA_flucast-INFLAenza.csv"))

