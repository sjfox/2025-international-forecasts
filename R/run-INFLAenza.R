library(tidyverse)
library(INLA)
library(lubridate)
library(cowplot)
library(glue)

inla.setOption(inla.mode="classic")

###############
#Define Parameters 
###############

horizon_neg_1 <- earliest_expected_data_date

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

#################
#Run Models
#################

########### #define model/fit data/ fit/ sample/ summarize
model <- model_formula("iid", temporal="ar1", spatial="iid") #normal

fit_df <- final_df |> 
  filter(ISO_SDATE < horizon_neg_1) |> 
  prep_fit_data(forecast_horizon) 

fit_df <- drop_weeks(fit_df, weeks_to_drop, date_col = "ISO_SDATE")
  
fit <- fit_inla_model(fit_df, horizon_neg_1, model, pc_prior_u=c(1, 1))

pred_summ <- sample_count_predictions(fit_df, fit, forecast_date = horizon_neg_1, nsamp=5000) |>
  summarize_quantiles()

result <- hub_formatting(pred_summ)

result <- result |>
  mutate(horizon = as.integer(horizon - 1),
         reference_date = reference_date + days(7))

########### # Save files 

result |> 
  write_csv(paste0("processed-data/rt-forecasts/", forecast_date + days(3), "-UGA_flucast-INFLAenza.csv"))

