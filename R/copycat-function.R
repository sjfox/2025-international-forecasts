international_copycat <- function(curr_data,
                                  forecast_horizon = 5, ## How many weeks forecast and plotted?
                                  recent_weeks_touse = 5, ## 100 means all data from season are used
                                  nsamps = 1000,
                                  resp_week_range = 0,
                                  db = traj_db){
  # browser()
  
  most_recent_week <- max(curr_data$week)
  most_recent_value <- tail(curr_data$value,1)
  
  cleaned_data <- curr_data |> 
    select(week, curr_weekly_change) |> 
    filter(!is.na(curr_weekly_change)) |> 
    tail(recent_weeks_touse)  
  
  if(resp_week_range != 0){  
    matching_data <- cleaned_data |> 
      mutate(week_change = list(c(-(1:resp_week_range), 0, (1:resp_week_range)))) |> 
      unnest(week_change) |> 
      mutate(week = week + week_change) |> 
      filter(week>0)
  } else {
    matching_data <- cleaned_data |> 
      mutate(week_change = 0) |> 
      filter(week>0)
  }
  
  db |> 
    inner_join(matching_data, 
               by = 'week', relationship = "many-to-many") |> 
    group_by(week_change, country, year) |> 
    filter(n() == nrow(cleaned_data) | n() >= 4) |> ## Makes sure you've matched as many as the cleaned data or at least a full month
    # filter(metric == 'flusurv') |>  ## If you want to limit to specific database
    summarize(weight = sum((pred - curr_weekly_change)^2)/n(), .groups = "drop") |> 
    ungroup() |> 
    filter(!is.na(weight)) -> traj_temp
  
  min_allowed_weight <- 0.02
  
  traj_temp |>   
    mutate(weight = ifelse(weight < min_allowed_weight, min_allowed_weight, weight)) |>
    arrange(weight) |>
    slice(1:20) |> 
    sample_n(size = nsamps, replace = T, weight = 1/weight^2) |> 
    mutate(id = seq_along(weight)) |> 
    select(id, country, year, week_change) -> trajectories
  
  
  # trajectories |>
  #   left_join(db |>
  #               nest(data = c('week', 'pred', 'pred_se')),
  #             by = c('country', 'year')) |>
  #   unnest(data) |>
  #   mutate(week = week - week_change) |>
  #   # mutate(pred = rnorm(n(), pred, pred_se/2)) |>
  #   ggplot(aes(week, pred, group = interaction(country, year, week_change))) +
  #   geom_point(alpha = .1) +
  #   geom_hline(yintercept=0, lty = 2, color = 'gray') +
  #   geom_point(data = cleaned_data, aes(week, curr_weekly_change), color = 'red', inherit.aes=F)
  # 
  # browser()
  
  trajectories |> 
    left_join(db |> 
                nest(data = c('week', 'pred', 'pred_se')),
              by = c('country', 'year')) |> 
    unnest(data) |> 
    mutate(week = week - week_change) |> 
    filter(week %in% most_recent_week:(most_recent_week+forecast_horizon-1)) |>
    mutate(weekly_change = exp(rnorm(n(), pred, pred_se))) |>
    group_by(id) |> 
    arrange(week) |> 
    mutate(mult_factor = cumprod(weekly_change)) |> 
    ungroup() |> 
    mutate(forecast = most_recent_value*mult_factor) |> 
    mutate(forecast = rnbinom(n(), mu = most_recent_value*mult_factor, size = 5)) |> ##Want more dispersion than poisson distribution
    # mutate(forecast = rpois(n = n(), lambda = forecast)) |> ##Want poisson dispersion
    mutate(week = week + 1) |> 
    select(id, week,forecast) 
  
}
