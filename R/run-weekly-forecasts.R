library(tidyverse)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())

# Setup the global parameters --------------------------------------------
forecast_date <- Sys.Date() 
## Switch to the date you are making the forecast (should be a wednesday) for testing
# forecast_date <- ymd('2025-04-30')

earliest_expected_data_date <- forecast_date - days(4)

country_parm_table <- tibble(country = c('Australia', 'Brazil', 'Chile', 'South Africa', 'Thailand'),
                             horizons_dropped = c(1, 3, 2, 2, 0),  ## Most weeks Thailand should be 0, but if they missing data should be -1
                             min_fcast_horizon = c(-1, -1, -1, -1, 0),
                             max_fcast_horizon = c(3, 3, 3, 3, 3)) 
## horizons_dropped definition:
## If you drop `1` horizon you are dropping data from anything equal to or more recent than horizon -1. 
## If you drop `0`, you are dropping anything from horizon 0 and from the future.
## Horizon 0 is a half week of data so you always want to drop at least zero

## Specify the min and max horizon for each country


# desired_horizon <- 5
#weeks_to_drop <- 0

quantiles_needed <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

# Run copycat -------------------------------------------------------------
source('R/copycat-function.R')
source('R/run-copycat.R')

# Run INFLAenza -------------------------------------------------------------
source('R/INFLAenza-function.R')
source('R/run-INFLAenza.R')


# Run GBQR -------------------------------------------------------------
source('R/GBQR-function.R')
source('R/run-GBQR.R')


# Make the plots! ---------------------------------------------------------

## Uses the tibble copycat creates to make data for plotting -- can change later
fcast_df_clean |> 
  filter(year == year(forecast_date),
         week >= 1,
         date < forecast_date) |> 
  group_by(country) |> 
  arrange(week) |> 
  left_join(country_parm_table, by = 'country') |> 
  mutate(dropped_data = date >= earliest_expected_data_date + days(7) - days(horizons_dropped*7)) -> curr_yr_df 


fcast_df_clean |> 
  filter(year == year(forecast_date)-1,
         week >= 1,
         week <= max(curr_yr_df$week) + 5) |> 
  group_by(country) |> 
  arrange(week) -> last_yr_df 




make_country_plot <- function(curr_country,
                              fcast_df,
                              curr_yr_df,
                              last_yr_df){
  # browser()
  
  fcast_df <- fcast_df |> 
    filter(country == curr_country) |> 
    filter(output_type_id %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |> 
    pivot_wider(names_from = output_type_id,
                values_from = value) |> 
    mutate(week = horizon + (curr_yr_df |> pull(week) |> max()) + 1)
    
  curr_yr_df <- curr_yr_df |> 
    filter(country == curr_country)
  last_yr_df <- last_yr_df |> 
    filter(country == curr_country)
  
    fcast_df |> 
      ggplot(aes(week, `0.5`)) +
      geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), alpha = .2) +
      geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = .2) +
      geom_line() +
      geom_point(data = curr_yr_df, aes(week, value)) +
      geom_point(data = curr_yr_df |> 
                   filter(dropped_data), aes(week, value), color = 'blue') +
      geom_point(data = last_yr_df, aes(week, value), color = 'darkred', alpha = .5) +
      labs(title = curr_country, x = NULL, y ='Admits') +
      background_grid(major = 'xy', minor = 'y') +
      coord_cartesian(ylim = c(0, max(c(curr_yr_df$value*1.25, fcast_df$`0.75`))))
}

make_fcast_model_plot <- function(file_path,
                                  curr_yr_df,
                                  last_yr_df){
  fcast_df <- read_csv(file_path)
  
  country_plots <- map(sort(unique(fcast_df$country)), make_country_plot,
               fcast_df = fcast_df, 
               curr_yr_df = curr_yr_df,
               last_yr_df = last_yr_df)
  
  plot_path <- file_path |> 
    str_replace(pattern = 'processed-data', replacement = 'figs') |> 
    str_replace(pattern = 'csv', replacement = 'png')
  
  save_plot(filename = plot_path,
              plot_grid(plotlist = country_plots),
              base_height = 7, base_asp = 1.8, bg = 'white')
}

## Run through all data files produced this week and make resepective forecast plots
fcast_files <- list.files('processed-data/rt-forecasts', full.names = T)
curr_week_fcasts <- fcast_files[grepl(forecast_date+days(3), fcast_files)]

curr_week_fcasts |> 
  map(make_fcast_model_plot,
      curr_yr_df = curr_yr_df,
      last_yr_df = last_yr_df)


# Copy files to the submission location -----------------------------------
## You need to add your model folder here if it's not already accounted for
model_folder_loc <- case_when(grepl(pattern = 'Copycat', curr_week_fcasts) ~ 
                             'UGA_flucast-Copycat/',
                           grepl(pattern = 'INFLAenza', curr_week_fcasts) ~ 
                             'UGA_flucast-INFLAenza/',
                           grepl(pattern = 'GBQR', curr_week_fcasts) ~ 
                             'UGA_flucast-GBQR/',
                           grepl(pattern = 'seasonal_baseline', curr_week_fcasts) ~ 
                             'UGA_flucast-seasonal_baseline/',
                          )

## Concatenate to get all file paths
new_file_locs <- paste0("../NCIRD-GIB-FluNET-Forecasting/model-output/", 
                        model_folder_loc, 
                        str_remove(curr_week_fcasts, pattern = 'processed-data/rt-forecasts/'))

## Copy files over based on the relative file path
file.copy(from = curr_week_fcasts, 
          to = new_file_locs, 
          copy.mode = TRUE, 
          overwrite = T)


# Validate the files ------------------------------------------------------
library(hubValidations)

validate_and_missing <- function(fcast_file, hub_rel_path){
  validate_submission(hub_path = hub_rel_path,
                      file_path = fcast_file) -> sub_validation
  
  ## Looking for all green checkmarks
  print(sub_validation)
  
  ## Looking for any missing required values
  print(sub_validation$req_vals$missing)
  
  return(sub_validation)
}

str_remove(new_file_locs, pattern = '../NCIRD-GIB-FluNET-Forecasting/model-output/') |> 
  map(validate_and_missing, hub_rel_path = '../NCIRD-GIB-FluNET-Forecasting') -> validation_objects
 


