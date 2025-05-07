library(tidyverse)
library(INLA)
library(lubridate)
library(cowplot)

inla.setOption(inla.mode="classic")

######################
#Model Formula
###Note that its intended to be run only as "iid, ar1, iid"
######################
model_formula <- function(
    seasonal=c("none", "shared", "iid"),
    temporal=c("none", "ar1", "rw2"),
    spatial=c("none", "iid", "exchangeable", "besag", "besagproper") 
) {
  seasonal <- case_match(
    seasonal,
    "none" ~ "",
    "shared" ~ '+ f(ISO_WEEK, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)',
    "iid" ~ '+ f(ISO_WEEK, model="rw2", scale.model=TRUE, cyclic=TRUE, 
            hyper=hyper_epwk, group=iloc, control.group=list(model="iid"))'
  )
  
  scaling <- case_match(
    temporal,
    c("rw1", "rw2") ~ "scale.model=TRUE, ",
    "ar1" ~ ""
  )
  
  weekly_main <- case_match(
    temporal,
    "none" ~ "",
    c("ar1", "rw2") ~ glue('+ f(t, model="{temporal}", {scaling}hyper=hyper_wk)')
  )
  
  weekly_interaction <- case_match(
    spatial,
    "none" ~ "",
    "iid" ~ glue('+ f(t2, model="{temporal}", hyper=hyper_wk, 
                     group=iloc, {scaling}control.group=list(model="iid"))'),
    "exchangeable" ~ glue('+ f(t2, model="{temporal}", hyper=hyper_wk, 
                     group=iloc, {scaling}control.group=list(model="exchangeable"))'),
    "besag" ~ glue('+ f(iloc, model="besag", hyper=hyper_wk, graph=graph, scale.model=TRUE,
                     group=t2, control.group=list(model="{temporal}"))'), # note the group model automatically scaled
    "besagproper" ~ glue('+ f(iloc, model="besagproper", hyper=hyper_wk, graph=graph,
                     group=t2, control.group=list(model="{temporal}"))')
  )
  
  glue("INF_ALL ~ 1 + COUNTRY {seasonal} {weekly_main} {weekly_interaction}")
}

####################
#Function for fitting model
#####################

fit_inla_model <- function(
    fit_df, forecast_date, model,
    pc_prior_u=c(1, 1),
    joint_forecast=TRUE,
    pred_idx=NULL,
    q=c(0.025, 0.25, 0.5, 0.75, 0.975),
    graph=NULL, dic=FALSE
) {
  # the PC priors c(u, a) give the probability a that the standard deviation between weeks exceeds u
  # increasing u increases prior beliefs that there will be large jumps between weeks
  hyper_epwk <- list(prec=list(prior="pc.prec", param=c(pc_prior_u[1], 0.01)))
  hyper_wk <- list(prec=list(prior="pc.prec", param=c(pc_prior_u[2], 0.01)))
  
  mod <- as.formula(model)
  
  if (joint_forecast)
    pred_idx <- which(fit_df$ISO_SDATE >= forecast_date)
  
  fit <- inla(
    mod, family="poisson", data=fit_df, # poisson regression link
    #E=fit_df$ex_lam,
    quantiles=q,
    selection=if (is.null(pred_idx)) NULL else list(Predictor=pred_idx),
    control.compute=list(dic=dic, mlik=FALSE, return.marginals.predictor=TRUE),
    control.predictor=list(link=1) # produce marginal fitted values with default (log) link function
  )
  
  return(fit)
}

############
#Function to drop weeks/ set horizon 
############
forecast_horizon = desired_horizon + weeks_to_drop

drop_weeks <- function(df, 
                       weeks_to_drop = 1, 
                       date_col = "ISO_SDATE") {
  
  df <- df %>% mutate(!!sym(date_col) := as.Date(.data[[date_col]]))
  # Get the max date and compute forecast reference
  max_date <- max(df[[date_col]], na.rm = TRUE)
  forecast_ref <- max_date - lubridate::weeks(weeks_to_drop)
  
  df_dropped <- df %>% 
    filter(.data[[date_col]] <= forecast_ref)
  
  return(df_dropped)
}


#####################
#prepare the data for model 
######################

prep_fit_data <- function(disease_df, forecast_horizon) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(forcats)
  
  # Convert ISO_SDATE to Date, filter for dates from 2015 onwards,
  # and calculate ISO_WEEK from the date using epiweek
  disease_df <- disease_df %>%
    mutate(ISO_SDATE = as.Date(ISO_SDATE)) %>%
    filter(ISO_SDATE >= as.Date("2015-01-01")) %>%
    mutate(ISO_WEEK = epiweek(ISO_SDATE))
  
  ret <- disease_df %>%
    group_by(ISO_SDATE) %>%
    mutate(t = cur_group_id(), .after = ISO_SDATE) %>%
    ungroup() %>%
    mutate(iloc = as.numeric(fct_inorder(COUNTRY))#,
           #ex_lam={{ex_lam}}
    )
  
  if (forecast_horizon > 0) {
    # Get the latest date in the data and its epiweek (base_week)
    base_date <- max(ret$ISO_SDATE)
    base_week <- epiweek(base_date)
    
    # Create prediction rows using the base_date and base_week
    pred_df <- expand_grid(
      tibble(
        ISO_SDATE = base_date + weeks(1:forecast_horizon),
        t         = max(ret$t) + (1:forecast_horizon),
        ISO_WEEK  = base_week + (1:forecast_horizon)
      ),
      distinct(ret, COUNTRY, iloc)
    )
    
    # Ensure that predictions are added only for the existing locations
    location_data <- ret %>%
      filter(ISO_SDATE == base_date) %>%
      #distinct(COUNTRY, ex_lam)
      distinct(COUNTRY)
    
    pred_df <- left_join(
      pred_df, 
      location_data, 
      by = "COUNTRY", 
      unmatched = "error"
    )
    
    ret <- bind_rows(ret, pred_df)
  }
  
  ret %>%
    mutate(t2 = t) %>%
    arrange(t, iloc)
}


##############
#Sample 
#############
sample_count_predictions <- function(fit_df, fit, forecast_date, nsamp=1000) {
  nloc <- length(unique(fit_df$iloc))
  
  ret_df <- fit_df |>
    filter(ISO_SDATE >= forecast_date)
  
  jsamp_fvals <- exp(inla.rjmarginal(nsamp, fit$selection)$samples)
  
  
  count_samp <- list_transpose(map(1:nsamp, \(samp) { # invert list so have sampled counts for each row
    lambda <- jsamp_fvals[,samp] 
    rpois(length(lambda), lambda) # indep. Poisson for each spacetime series
  }))
  
  ret_df |> 
    mutate(count_samp=count_samp) |> 
    select(where(~!any(is.na(.x))))
}

###############
#Summarize Quantiles 
###############
summarize_quantiles <- function(pred_samples, q=c(0.01, 0.025, seq(0.05, 0.95, by=0.05), 0.975, 0.99)) {
  pred_samples |> 
    unnest(count_samp) |> 
    group_by(ISO_SDATE, COUNTRY) |> 
    summarize(
      mean=mean(count_samp),
      qs = list(value = quantile(count_samp, probs=q)), 
      .groups="drop"
    ) |> 
    unnest_wider(qs) |>
    pivot_longer(contains("%"), names_to="quantile") |> 
    mutate(quantile=parse_number(quantile)/100)
}

####################
#Hub formatting 
#####################
hub_formatting <- function(pred_samples) { 
  
  pred_samples$date <- ymd(pred_samples$ISO_SDATE)
  
  pred_samples$reference_date <- ymd(pred_samples$ISO_SDATE[1]) 
  
  pred_samples %>%
    group_by(COUNTRY) %>%
    arrange(ISO_SDATE, .by_group = TRUE) %>%
    mutate(
      output_type_id = quantile,
      target = "inc flu case",
      #horizon = as.integer(date - min(date)) + 1,  
      horizon=as.numeric(as.factor(ISO_SDATE)) -1,
      output_type = "quantile",
      value = round(value, digits = 0)
    ) %>%
    rename(target_end_date = ISO_SDATE,
           country = COUNTRY) %>%
    select(reference_date, target, horizon, target_end_date, country, output_type, output_type_id, value)
}
