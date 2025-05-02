library(tidyverse)
library(lubridate)
library(gam)



# Setup parameters --------------------------------------------------------
curr_resp_season <- year(forecast_date)


# Read-in recent  data --------------------------------------------
## Read and clean the data
df <- read_csv('../NCIRD-GIB-FluNET-Forecasting/target-data/flunet-all.csv')

data_selection_for_forecasting <- tibble(COUNTRY = c('Australia', 'Brazil', 'Brazil', 
                                                     'Chile', 'Chile', 'South Africa', 'Thailand'),
                                         ORIGIN_SOURCE = c('NOTDEFINED', 'NONSENTINEL', 'SENTINEL', 
                                                           'NONSENTINEL', 'SENTINEL', 'SENTINEL', 'NOTDEFINED'))

df |> 
  inner_join(data_selection_for_forecasting, by = c('COUNTRY', 'ORIGIN_SOURCE')) |> 
  group_by(COUNTRY, ISO_YEAR, ISO_WEEK, ISO_SDATE) |> 
  summarize(INF_ALL = sum(INF_ALL, na.rm = T)) |> 
  select(year = ISO_YEAR,
         week = ISO_WEEK,
         date = ISO_SDATE,
         country = COUNTRY, value = INF_ALL) -> fcast_df_clean
  
## Current seasonal data used for fitting/forecasting
fcast_df_clean |> 
  filter(year == curr_resp_season,
         week >= 1,
         date <= forecast_date) |> 
  group_by(country) |> 
  arrange(week) -> curr_fcast_df 
  
curr_fcast_df |> ungroup() |> filter(date == max(date)) |> distinct(week) |> pull(week) -> forecast_week

# curr_fcast_df |> 
#   group_by(country) |> 
#   filter(date == max(date))

## Build historical dataset used to create the trajectory database
df |> 
  group_by(COUNTRY, ORIGIN_SOURCE, ISO_YEAR, ISO_WEEK, ISO_SDATE) |> 
  summarize(INF_ALL = sum(INF_ALL, na.rm = T)) |> 
  select(year = ISO_YEAR,
         week = ISO_WEEK,
         date = ISO_SDATE,
         origin = ORIGIN_SOURCE,
         country = COUNTRY, value = INF_ALL) |> 
  mutate(keeping = case_when(country == 'Australia' & origin == 'NOTDEFINED' & year >= 2020 ~ F,
                             country == 'Australia' & origin == 'NOTDEFINED' & year >= 1999 ~ T,
                             country == 'Brazil' & origin == 'NOTDEFINED' & year >= 2009 & year < 2020 ~ T,
                             country == 'Chile' & origin == 'NOTDEFINED' & year >= 2006 & year < 2020 ~ T,
                             country == 'South Africa' & origin == 'NOTDEFINED' & year >= 2000 & year < 2020 ~ T,
                             country == 'Thailand' & origin == 'NOTDEFINED' & year >= 2020 ~ F,
                             country == 'Thailand' & origin == 'NOTDEFINED' & year >= 2003 ~ T,
                             T ~ F
                             )) |> 
  filter(keeping) |> 
  bind_rows(fcast_df_clean |> 
              filter((year > 2021 & country !='Brazil') |
                     (year > 2022 & country == 'Brazil'),
                     year < curr_resp_season)) |> 
  ungroup() |> 
  select(year, week, date, country, value) -> historical_fcast_df



# historical_fcast_df |>
#   filter(country == 'Brazil') |>
#   ggplot(aes(date, value)) + geom_line()
# 
# historical_fcast_df |> 
#   filter(country == 'Thailand') |> 
#   ggplot(aes(week, value, color = as.factor(year))) + geom_line()



# Create seasonal trajectory splines -------------------------------------
get_seasonal_spline_vals <- function(season_weeks, value){
  # browser()
  padding <- 5
  new_value <- c(rep(head(value, 1),padding), value, rep(tail(value, 1),padding))
  new_season_weeks <- c(rev(min(season_weeks) - 1:padding), season_weeks, max(season_weeks)+1:padding)
  weekly_change <- lead(new_value+1)/(new_value+1)
  weekly_change <- ifelse(is.na(weekly_change), 1, weekly_change)
  df <- tibble(new_season_weeks, weekly_change) 
  
  mod <- gam(log(weekly_change) ~ s(new_season_weeks, length(season_weeks)/10), data = df)
  
  df |>
    ggplot(aes(new_season_weeks, log(weekly_change))) +
    geom_point() +
    geom_line(aes(x = new_season_weeks, y = mod$fitted.values)) 
  
  
  tibble(weeks=new_season_weeks,
         pred = mod$fitted.values,
         pred_se = as.numeric(predict(mod, se = T)$se.fit)) |> 
    filter(weeks %in% season_weeks) 
}
augment_annual_data <- function(year, country, df){
  ## Need a function to add 10 weeks or so on to every year
  ## So that you can make forecasts on week 52 into next year basically
  # browser()
  # if(year == 2022 & country == 'Australia'){browser()}
  df |> 
    filter(country == .env$country,
           year == .env$year | (year == .env$year+1 & week <10)) |> 
    arrange(date) |> 
    mutate(year = .env$year,
           week = seq_along(week)) 
}

historical_fcast_df |> 
  distinct(year,country) |> 
  pmap(augment_annual_data, df = historical_fcast_df) |> ## This makes it so you can make a forecast on week 50, 5 weeks ahead
  bind_rows() |>
  group_by(country, year) |> 
  filter(n()>50) |> ## This minus 10 is what is the actual number of weeks needed
  arrange(week) |> 
  mutate(get_seasonal_spline_vals(week, value)) |> 
  ungroup() |> 
  select(country, year, week, pred, pred_se) -> traj_db


# traj_db |>
#   ggplot(aes(week, exp(pred), group = interaction(country, year),
#              color = as.factor(year))) +
#   geom_line(alpha = .8) +
#   facet_wrap(~country)

save(traj_db, historical_fcast_df, 
     file = 'processed-data/copycat-spline-traj.rda')


# Make forecasts ----------------------------------------------------------
# browser()
countries <- unique(curr_fcast_df$country)
country_forecasts <- vector('list', length = length(countries))
for(curr_country in countries){
  # curr_country <- 'Chile'
  ## Subset data to correct time period and location
  curr_fcast_df |>
    ungroup() |>
    filter(country == curr_country,
           year == curr_resp_season,
           date < earliest_expected_data_date - days(weeks_to_drop*7)) |> ## Removes most recent data point
    mutate(value = value+1) |> ## Makes sure no zeroes, need to correct later
    mutate(curr_weekly_change = log(lead(value)/value)) |> 
    select(week, value, curr_weekly_change) |>
    international_copycat(db = traj_db |> 
                            filter(country == curr_country),
                     recent_weeks_touse = 100,
                     resp_week_range = 0,
                     forecast_horizon = desired_horizon + weeks_to_drop) |>
    mutate(forecast = forecast-1) |>
    mutate(forecast = ifelse(forecast < 0, 0, forecast)) -> forecast_trajectories
  
  
  forecast_trajectories |>
    group_by(week) |>
    summarize(qs = list(value = quantile(forecast, probs = quantiles_needed))) |>
    mutate(horizon = seq_along(week)-weeks_to_drop-2) |>
    unnest_wider(qs) |>
    gather(quantile, value, -week, -horizon) |>
    ungroup() |>
    mutate(quantile = as.numeric(gsub("[\\%,]", "", quantile))/100) |>
    mutate(country = curr_country,
           target = paste0("inc flu case"),
           reference_date = forecast_date + days(3),
           target_end_date = forecast_date + days(3) + horizon*7,
           output_type_id = quantile,
           output_type = 'quantile',
           value = round(value)) %>%
    arrange(country, horizon, quantile) |>
    dplyr::select(reference_date, target, horizon, target_end_date, country, output_type, output_type_id, value) -> cleaned_forecasts_quantiles
  
  
  country_forecasts[[match(curr_country, countries)]] <- cleaned_forecasts_quantiles |>
    mutate(output_type_id = as.character(output_type_id)) 
}
country_forecasts |>
  bind_rows() -> country_forecasts
country_forecasts |> 
  filter(horizon >= -1) |>
  mutate(horizon = horizon,
         target_end_date = target_end_date) -> country_forecasts

country_forecasts |> 
  write_csv(paste0("processed-data/rt-forecasts/", forecast_date + days(3), "-UGA_flucast-Copycat.csv"))


# country_forecasts |>
#   filter(output_type == 'quantile') |> 
#   filter(output_type_id %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |> 
#   spread(output_type_id, value) -> forecast_df
# 
# forecast_df |> 
#   ggplot(aes(target_end_date, `0.5`)) +
#   geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), alpha = .2) +
#   geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = .2) +
#   geom_line() +
#   facet_wrap(~country, scales= 'free_y') +
#   geom_point(data = curr_fcast_df, aes(date, value))
# 
# 
# 
# # Plot forecasts ----------------------------------------------------------
# make_individual_plot <- function(curr_country,
#                                  curr_season_data, 
#                                  prev_season_data,
#                                  forecast_df){
#   # browser()
#   curr_df <- curr_season_data |> 
#     filter(country == curr_country)
#   
#   # curr_df |> 
#   #   filter(resp_season_week == 1) |> pull(date) -> week1_date
#   prev_df <- prev_season_data |> 
#     filter(country == curr_country)
#   
#   forecast_df |> 
#     filter(country == curr_country) -> forecast_df
#   
#   forecast_df |> 
#     mutate(week = horizon + (curr_df |> pull(week) |> max()) + 1) |> 
#     ggplot(aes(week, `0.5`)) +
#     geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), alpha = .2) +
#     geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = .2) +
#     geom_line() +
#     geom_point(data = curr_df, aes(week, value)) +
#     geom_point(data = prev_df, aes(week, value), color = 'red',alpha = .6) +
#     labs(title = curr_country, x = NULL, y ='Admits') +
#     background_grid(major = 'xy', minor = 'y') +
#     coord_cartesian(ylim = c(0, max(c(curr_df$value, forecast_df$`0.75`))))
# }
# 
# 
# 
# 
# 
# 
# curr_season_data <- curr_fcast_df |>
#   ungroup() |>
#   filter(week <= forecast_week, year == curr_resp_season)
# 
# # curr_season_data |> 
# #   filter(location_name == 'Washington')
# rec_resp_season_week <- forecast_week
# prev_season_data <- historical_fcast_df |>
#   ungroup() |>
#   filter(year == curr_resp_season-1, 
#          week >= 1,
#          week < rec_resp_season_week+7)
# 
# 
# 
# 
# countries |> 
#   map(make_individual_plot, 
#       curr_season_data = curr_season_data, 
#       prev_season_data = prev_season_data,
#       forecast_df = forecast_df) -> plots
# 
# plot_grid(plotlist = plots) |> 
#   save_plot(filename = paste0('figures/international-rt/', forecast_date + 3, '_rt-forecast.png'), base_height = 12, base_asp = 1.6, bg = 'white')
# 
# 
# # Double check files ------------------------------------------------------
# library(hubValidations)
# 
# file.copy(from=paste0("processed-data/international-rt-forecasts/", 
#                       forecast_date + 4, 
#                       "-UGA_flucast-Copycat.csv"), 
#           to=paste0("../NCIRD-GIB-FluNET-Forecasting/model-output/UGA_flucast-Copycat/", 
#                     forecast_date + 4, 
#                     "-UGA_flucast-Copycat.csv"), copy.mode = TRUE, overwrite = T)
# 
# hubValidations::validate_submission(hub_path = '~/projects/NCIRD-GIB-FluNET-Forecasting',
#                                     file_path = paste0('UGA_flucast-Copycat/', forecast_date + 4, '-UGA_flucast-Copycat.csv')) -> sub_validation
# 
# # hubValidations::validate_submission(hub_path = '~/projects/FluSight-forecast-hub/',
# # file_path = 'UGA_flucast-INFLAenza/2024-02-10-UGA_flucast-INFLAenza.csv') -> sub_validation
# 
# 
# # Want all \green checkmarks
# sub_validation
# 
# ## Want to make sure there are no missing required values
# sub_validation$req_vals$missing

