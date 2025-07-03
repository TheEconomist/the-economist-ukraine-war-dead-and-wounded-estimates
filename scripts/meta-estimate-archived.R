# This scripts reads in estimates of casualties and dead in the Russia-Ukraine war and uses them to construct a meta-estimate of those fallen in the conflict. There is currently not enough data on Ukrainian casualties or deaths to produce a live-updating estimate.

setwd('/Users/sondresolstad/Github/ukraine-war-dead-and-wounded-estimates')

# 1. Load packages ------------------------------------------------------------
library(tidyverse)
library(lubridate)

# 2. Load source data ------------------------------------------------------------
all <- read_csv('source-data/Soldier deaths_casualties in Ukraine June 1st 2025 - estimates.csv') %>% 
  filter(is.na(`ignore for chart, data from warring parties`)) %>%
  mutate(date = as.Date(date, format = '%d/%m/%Y'),
         source = ifelse(source == 'UK Ministry of Defence', 'UK MoD', source)) %>%
  mutate(source = ifelse(source == 'Pentagon leak', 'US DoD', source))
all$source_id <- paste0('all_df_', all$source, '_', 1:nrow(all))

# Reading in separately collected UK MoD estimates, removing our own estimates for very recent dates
casualties_via_mod <- read_csv('source-data/Soldier deaths_casualties in Ukraine June 1st 2025 - uk mod month-by-month estimates.csv') %>% 
  mutate(date = as.Date(as.character(`date (at beginning of next month)`), format = "%d/%m/%Y")) %>%
  filter(date <= as.Date('2025-05-01'))
casualties_via_mod$source_id <- paste0('casualties_via_mod_', 1:nrow(casualties_via_mod))

# Test: If no recent estimate, decline update
if(max(casualties_via_mod$date, na.rm = T) < Sys.Date()-30*3 &
   max(all$date, na.rm = T) < Sys.Date()-30*3){
  stop("No recent enough data available, declining update.")
}

# Source: https://meduza.io/en/feature/2025/02/24/three-years-of-death - filtering out incomplete recent weeks
deaths_via_meduza <- read_csv('source-data/data-9LLji.csv') %>% filter(Date <= as.Date('2024-11-18'))
deaths_via_meduza$source_id <- paste0('deaths_via_meduza_', 1:nrow(deaths_via_meduza))

# Define invasion start date:
invasion_start <- as.Date('2022-02-24')

# 3. Construct two data sources, each being daily deaths, based on all this data. 
# All estimates are weighted equally. This means that e.g. estimates of cumulative deaths at T=100 becomes 100 observations, each with weight 1/100.

# We first discard estimates which only give a lower or upper bound. If they have both, they enter separately as two estimates. Also subset to Russian data only
all <- all %>% filter(!(is.na(estimate_low) & !is.na(estimate_high)), 
                      !(!is.na(estimate_low) & is.na(estimate_high)),
                      country == 'russia') %>%
  mutate(estimate_raw = estimate,
         estimate_high_raw = estimate_high,
         estimate_low_raw = estimate_low,
         estimate_start = invasion_start)

# We then extract the data for deaths and casualties (for Russia)
deaths_cumulative <- all %>% filter(type == 'deaths')
casualties_cumulative <- all %>% filter(type == 'casualties')

# Because we are loading MoD estimates separately, we discard those duplicated in here.
casualties_cumulative <- casualties_cumulative %>% filter(!source %in% c('UK Ministry of Defence', 'UK MoD'))

# Same with Meduza; Mediazona
deaths_cumulative <- deaths_cumulative %>% filter(!source %in% c('Mediazona; Meduza'))

# For some other sources, we also have repeated estimates. This means these imply different death rates at different times. 
# While it is also possible that methodologies and data availability change over time, leveraging this information is better 
# than assuming that all estimate imply a flat rate over the course of the war. 

# Function to process repeated cumulative estimates from the same source
process_repeated_estimates <- function(data, sources_to_process) {
  # Order data by date to ensure proper chronological processing
  data <- data[order(data$date), ]
  
  for(source_name in sources_to_process) {
    source_indices <- which(data$source == source_name)
    
    if(length(source_indices) > 1) {
      # Process from most recent to oldest (reverse chronological order)
      for(j in length(source_indices):2) {
        idx_current <- source_indices[j]
        idx_previous <- source_indices[j-1]
        
        # Check if previous had central estimate, else use data from range:
        prev_estimate <- ifelse(!is.na(data$estimate[idx_previous]),  data$estimate[idx_previous], 
                                mean(c(data$estimate_high[idx_previous],data$estimate_low[idx_previous]), na.rm = T))
        
        if(!is.na(prev_estimate)){
          
          # Take the difference from the preceding estimate
          if(!is.na(data$estimate[idx_current])){
            data$estimate[idx_current] <- data$estimate[idx_current] - prev_estimate
          }

          # Handle estimate_low
          if(!is.na(data$estimate_low[idx_current])){ 
            if(!is.na(data$estimate_low[idx_previous])) {
              data$estimate_low[idx_current] <- data$estimate_low[idx_current] - data$estimate_low[idx_previous]
            } else {
              data$estimate_low[idx_current] <- data$estimate_low[idx_current] - prev_estimate
            }
          }
          
          # Handle estimate_high
          if(!is.na(data$estimate_high[idx_current])){ 
            if(!is.na(data$estimate_high[idx_previous])) {
              data$estimate_high[idx_current] <- data$estimate_high[idx_current] - data$estimate_high[idx_previous]
            } else {
              data$estimate_high[idx_current] <- data$estimate_high[idx_current] - prev_estimate
            }
          }
          
          # Update the start date to the day after the previous estimate
          data$estimate_start[idx_current] <- data$date[idx_previous] + 1
        }
      }
    }
  }
  
  return(data)
}

# We then cycle through sources which have repeated (cumulative) estimates:
casualties_cumulative <- process_repeated_estimates(data = casualties_cumulative, 
                                                    sources_to_process = c("CIA", "US DoD"))
deaths_cumulative <- process_repeated_estimates(data = deaths_cumulative, 
                                                sources_to_process = c('Excess deaths estimate (D. Kobak)', 'BBC Russia', 'UK MoD'))

# Power-law scale parameter to weight estimates taken over longer periods as less informative
# Transform cumulative casualty estimates into daily ones
casualties_daily <- data.frame()
for(i in 1:nrow(casualties_cumulative)){
  for(j in c('estimate_low', 'estimate_high', 'estimate')){
    if(!is.na(as.numeric(casualties_cumulative[i, j]))){
      date_sequence <- seq(casualties_cumulative$estimate_start[i], casualties_cumulative$date[i], by="day")
      daily_rate <- as.numeric(casualties_cumulative[i, j])/length(date_sequence)
      weight_value <- 1/length(date_sequence)
      
      casualties_daily <- rbind(casualties_daily, 
                                data.frame(date = date_sequence,
                                           casualties = daily_rate,
                                           weight = weight_value,
                                           source = casualties_cumulative$source[i],
                                           source_id = casualties_cumulative$source_id[i]))
    }
  }
}
casualties_daily$date <- as.Date(casualties_daily$date)

# Transform cumulative death estimates into daily ones
deaths_daily <- data.frame()
for(i in 1:nrow(deaths_cumulative)){
  for(j in c('estimate_low', 'estimate_high', 'estimate')){
    if(!is.na(as.numeric(deaths_cumulative[i, j]))){
      date_sequence <- seq(deaths_cumulative$estimate_start[i], deaths_cumulative$date[i], by="day")
      daily_rate <- as.numeric(deaths_cumulative[i, j])/length(date_sequence)
      weight_value <- 1/length(date_sequence)
      
      deaths_daily <- rbind(deaths_daily, 
                            data.frame(date = date_sequence,
                                       deaths = daily_rate,
                                       weight = weight_value,
                                       source = deaths_cumulative$source[i],
                                       source_id = deaths_cumulative$source_id[i]))
    }
  }
}
deaths_daily$date <- as.Date(deaths_daily$date)

# Summary statistics
cat("Data processing complete.\n")
cat("Casualties daily observations:", nrow(casualties_daily), "\n")
cat("Deaths daily observations:", nrow(deaths_daily), "\n")
cat("Date range - Casualties:", min(casualties_daily$date, na.rm = T), "to", max(casualties_daily$date, na.rm = T), "\n")
cat("Date range - Deaths:", min(deaths_daily$date, na.rm = T), "to", max(deaths_daily$date, na.rm = T), "\n")


# Get monthly data from MoD on casualties into format:
casualties_daily_mod <- casualties_via_mod %>%
  # rename and compute weight & source
  mutate(
    casualties = `casualties (average daily losses)`,
    weight           = 1 / `days in month`,
    source           = "UK MoD"
  ) %>%
  # repeat each row once per day in that month
  uncount(`days in month`, .id = "day") %>%
  # assemble the actual date from year, month name and day
  mutate(
    date = as.Date(paste(year, month, day),
                   format = "%Y %B %d")
  ) %>%
  # pick only the columns you asked for
  select(date, casualties, weight, source, source_id)

# inspect
head(casualties_daily_mod)
# Visualize the MoD data
ggplot(casualties_daily_mod, aes(x=date, y=casualties))+geom_point()

# Get weekly deaths data from Meduza/Mediazona into format:
deaths_daily_meduza <- deaths_via_meduza %>%
  # gather the three weekly estimates into one column
  pivot_longer(
    cols = c(
      `Estimate based on probate data`,
      ED_prediction_low,
      ED_prediction_high
    ),
    names_to  = NULL,       # we don’t need to keep a “type” column
    values_to = "weekly"
  ) %>%
  # convert to per-day, add weight & source
  mutate(
    deaths = weekly / 7,
    weight       = 1 / 7,
    source       = "Meduza"
  ) %>%
  # expand each week into 7 days
  uncount(7, .id = "day") %>%
  # build the real date
  mutate(
    date = Date + days(day - 1)
  ) %>%
  # pick only your final four columns
  select(date, deaths, weight, source, source_id)

# Visualize the Meduza data
ggplot(deaths_daily_meduza, aes(x=date, y=deaths))+geom_point()

# Combine all data into two data frames
deaths_daily <- na.omit(rbind(deaths_daily, deaths_daily_meduza))
casualties_daily <- na.omit(rbind(casualties_daily, casualties_daily_mod))

# Visualize the Meduza data
ggplot(deaths_daily, aes(x=date, y=deaths))+geom_point()

# 4. Load covariates
fires <- read_csv('source-data/strikes_by_location_and_day.csv')
control <- read_csv('source-data/area_assessed_as_controlled_by_russia.csv')

covars <- fires %>% left_join(control) #%>% select(war_fires_per_day_non_cloud_days_7dma, change_in_area_assessed_as_russia_controlled, date)
  
# 5. Model probably daily deaths and casualties using poisson, predict to present  ------------------------------------------------------------

# turn dates into “days since invasion” for a numeric predictor
deaths_daily     <- deaths_daily     %>% mutate(day = as.numeric(date - invasion_start))
casualties_daily <- casualties_daily %>% mutate(day = as.numeric(date - invasion_start))

# merge in covariates:

casualties_daily <- casualties_daily %>% left_join(covars)

# fit weighted Poisson log‐linear trends
death_fit   <- glm(deaths   ~ day + as.factor(week(date))*year(date),
                   family = poisson(link = "log"),
                   data   = deaths_daily,
                   weights= weight)

casualty_fit <- glm(casualties ~  day + as.factor(week(date))*year(date) + war_fires_per_day_non_cloud_days_7dma + change_in_area_assessed_as_russia_controlled + I(day^2),
                    family = poisson(link = "log"),
                    data   = casualties_daily,
                    weights= weight)

# predict for each day from last obs to today
last_obs <- max(max(deaths_daily$date), max(casualties_daily$date))
future    <- tibble(date = seq(last_obs + 1, Sys.Date(), by = "day")) %>%
  mutate(day = as.numeric(date - invasion_start)) %>% left_join(covars)

predictions <- future %>% transmute(
  date                 = date,
  pred_deaths          = predict(death_fit,      newdata = ., type = "response"),
  pred_casualties      = predict(casualty_fit,  newdata = ., type = "response")
)

# Plot observed vs predicted
if(F){
  ggplot() +
    geom_point(data = deaths_daily,     aes(date, deaths),      alpha = .3) +
    geom_line (data = predictions,      aes(date, pred_deaths),  color = "red") +
    labs(title = "Daily Deaths: observed (gray) vs predicted (red)")
  
  ggplot() +
    geom_point(data = casualties_daily, aes(date, casualties),   alpha = .3) +
    geom_line (data = predictions,      aes(date, pred_casualties), color = "blue") +
    labs(title = "Daily Casualties: observed (gray) vs predicted (blue)")
}

# We finally build one dataframe of fitted+predicted values with 95% CI ----------------

# full date‐grid from invasion start to today
all_dates <- tibble(
  date = seq.Date(invasion_start, Sys.Date(), by = "day")
) %>% mutate(day = as.numeric(date - invasion_start))%>% left_join(covars)

# predict on the link scale (log) for deaths
pred_deaths_link <- predict(death_fit,
                            newdata = all_dates,
                            type    = "link",
                            se.fit  = TRUE)
# predict on the link scale (log) for casualties
pred_casualty_link <- predict(casualty_fit,
                              newdata = all_dates,
                              type    = "link",
                              se.fit  = TRUE)

# assemble into one tibble with 95% CIs
all_results <- all_dates %>%
  transmute(
    date,
    deaths_fit      = exp(pred_deaths_link$fit),
    deaths_lower    = exp(pred_deaths_link$fit - 1.96 * pred_deaths_link$se.fit),
    deaths_upper    = exp(pred_deaths_link$fit + 1.96 * pred_deaths_link$se.fit),
    casualties_fit  = exp(pred_casualty_link$fit),
    casualties_lower= exp(pred_casualty_link$fit - 1.96 * pred_casualty_link$se.fit),
    casualties_upper= exp(pred_casualty_link$fit + 1.96 * pred_casualty_link$se.fit)
  )

all_results <- all_results %>%
  mutate(
    # Prediction interval for an observed daily death count:
    deaths_pred_lo = qpois(0.025, deaths_fit),
    deaths_pred_hi = qpois(0.975, deaths_fit),
    # And similarly for casualties:
    casualties_pred_lo = qpois(0.025, casualties_fit),
    casualties_pred_hi = qpois(0.975, casualties_fit)
  )

all_results <- all_results[order(all_results$date), ]
for(i in c('deaths_fit',
           'deaths_lower',
           'deaths_upper',
           "deaths_pred_lo",
           "deaths_pred_hi",
           'casualties_fit',
           'casualties_lower',
           "casualties_pred_lo",
           "casualties_pred_hi",
           'casualties_upper')){
  all_results[, paste0('cumulative_', i)] <- cumsum(all_results[, i])
}

# Plot predictions:
ggplot(all_results, aes(x=date))+
  geom_line(aes(y=cumulative_deaths_fit))+
  geom_ribbon(aes(ymin=cumulative_deaths_pred_lo, ymax=cumulative_deaths_pred_hi), alpha = 0.9)+
  geom_line(aes(y=cumulative_casualties_fit))+
  geom_ribbon(aes(ymin=cumulative_casualties_pred_lo, ymax=cumulative_casualties_pred_hi), alpha = 0.9)+
  geom_point(data = all, aes(x=date, y=estimate, col=type))+
  geom_linerange(data = all, aes(x=date, ymin=estimate_low, ymax=estimate_high, col=type))+theme_minimal()

# 5. Xgboost + bootstrapped 95% CIs ---------------------------------------

library(xgboost)
library(dplyr)
library(purrr)
library(lubridate)

#–– 5.1 helper: impute & flag missing on any numeric covariates
prep_with_covars <- function(df, response) {
  df %>%
    left_join(covars, by="date") %>%
    # find all numeric covariates except date,response,weight,source
    mutate(across(
      .cols = where(is.numeric) & 
        !all_of(c("date", response, "weight")),
      .fns = list(
        impute = ~replace_na(.x, min(.x, na.rm=TRUE) - 1),
        miss   = ~as.integer(is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    ))
}

deaths_daily <- deaths_daily[, c('date', setdiff(colnames(deaths_daily), colnames(covars)))]
casualties_daily <- casualties_daily[, c('date', setdiff(colnames(casualties_daily), colnames(covars)))]

deaths_p <- na.omit(prep_with_covars(deaths_daily %>% select(-source),     "deaths"))
casual_p <- na.omit(prep_with_covars(casualties_daily %>% select(-source), "casualties"))

#–– 5.2 hyperparameter tuning function -------------------------------
library(ParBayesianOptimization)
library(xgboost)
library(dplyr)

tune_xgb_params_bayes <- function(df, response, nfolds = 5, seed = 11235, init_points = 10, n_iter = 2) {
  #  tune_xgb_params_bayes <- function(df, response, nfolds = 5, seed = 11235, init_points = 35, n_iter = 70) {
    set.seed(seed)
  
  y <- df[[response]]
  X <- df %>%
    select(-date, -all_of(response), -weight, -source_id) %>%
    as.matrix()
  
  # Group-aware CV folds
  unique_ids <- unique(df$source_id)
  id2fold <- sample(rep(1:nfolds, length.out = length(unique_ids)))
  names(id2fold) <- unique_ids
  fold_list <- lapply(1:nfolds, function(k) {
    which(id2fold[as.character(df$source_id)] == k)
  })
  
  dtrain <- xgb.DMatrix(data = X, label = y, weight = df$weight)
  
  nround_tracker <- list()
  
  scoringFunction <- function(eta, max_depth, subsample, colsample_bytree, min_child_weight, gamma, tweedie_variance_power) {
    params <- list(
      objective = "reg:tweedie",
      eval_metric = "poisson-nloglik",
      eta = eta,
      max_depth = as.integer(max_depth),
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      gamma = gamma,
      tweedie_variance_power = tweedie_variance_power,
      subsample = 1
    )
    
    cv <- xgb.cv(
      params = params,
      data = dtrain,
      folds = fold_list,
      nrounds = 1000,
      early_stopping_rounds = 30,
      verbose = 0
    )
    
    best_iter <- cv$best_iteration
    best_score <- cv$evaluation_log$test_poisson_nloglik_mean[best_iter]
    
    # Minimize score (so return -best_score)
    nround_tracker[[as.character(Sys.time())]] <<- list(score = best_score, nrounds = best_iter)
    
    return(list(
      Score = as.numeric(-best_score)))
  }
  
  bounds <- list(
    eta = c(0.01, 0.2),
    max_depth = c(3L, 7L),
    colsample_bytree = c(0.7, 1),
    min_child_weight = c(1, 6),
    gamma = c(0, 5),
    tweedie_variance_power = c(1, 2)
  )
  
  cat("Running Bayesian optimization for", response, "...\n")
  optObj <- bayesOpt(
    FUN = scoringFunction,
    bounds = bounds,
    initPoints = init_points,
    iters.n = n_iter,
    acq = "ei",
    verbose = 10,
    parallel = FALSE
  )
  
  #optObj <<- optObj
  
  best_row <- data.frame(optObj$scoreSummary[which.max(optObj$scoreSummary$Score), ])
  best_params <- best_row[ , names(bounds), drop = FALSE] %>% as.list()
  best_match <- nround_tracker[[which.min(sapply(nround_tracker, function(x) abs(x$score + best_row$Score)))]]
  best_params$nrounds <- best_match$nrounds
  cat("Best CV score:", round(-best_row$Score, 6), "\n")
  print(best_params)
  
  return(best_params)
}

#–– 5.3 fit xgboost with tuned parameters -------------------------------
fit_xgb <- function(df, response, params = NULL) {
  # extract y and X
  y <- df[[response]]
  X <- df %>%
    select(-date, -all_of(response), -weight, -source_id) %>%
    as.matrix()
  
  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X, label = y, weight = df$weight)
  
  # Use provided params or tune new ones
  if(is.null(params)) {
    params <- tune_xgb_params_bayes(df, response)
    
    saveRDS(params, paste0('best_params_temp', sample(1:100000, 1), '.RDS'))
  }
  
  # Train final model
  if(!is.na(params$nrounds)){
    nrounds <- params$nrounds
    params$nrounds <- NULL
  } else {
    nrounds <- 500
  }

  xgb_params <- c(
    list(
      objective              = "reg:tweedie",
      eval_metric            = "poisson-nloglik"
    ),
    params
  )

  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
  
  return(list(model = model, params = params, nrounds = nrounds))
}

# Tune and fit models
set.seed(112358)
cat("Tuning deaths model...\n")
death_mod <- fit_xgb(deaths_p[sample(1:nrow(deaths_p), 300), ], "deaths")
saveRDS(death_mod, 'output-data/death_mod.RDS')

cat("Tuning casualties model...\n")
casual_mod <- fit_xgb(casual_p, "casualties")
saveRDS(casual_mod, 'output-data/casualties_mod.RDS')

#–– 5.4 bootstrap for 95% CI on fit+forecast -----------------------------
boot_preds_group <- function(model_info, train_df, pred_df, response, B = 200, seed = 11238) {
  library(dplyr)
  library(purrr)
  library(xgboost)
  
  set.seed(seed)
  unique_ids <- unique(train_df$source_id)
  n_ids <- length(unique_ids)
  
  cat("Running cluster bootstrap with", B, "iterations for", response, "...\n")
  
  # Prepare prediction data once outside loop
  X_pred <- pred_df %>% 
    select(-date, -weight) %>% 
    as.matrix()
  dtest_pred <- xgb.DMatrix(X_pred)
  
  results <- map_dfr(seq_len(B), ~{
    i <- .x
    if (i %% 10 == 0) cat(" Bootstrap iteration", i, "of", B, "\n")
    
    sampled_ids <- sample(unique_ids, size = n_ids, replace = TRUE)
    samp <- bind_rows(lapply(sampled_ids, function(id) train_df %>% filter(source_id == id)))
    
    y_samp <- samp[[response]]
    X_samp <- samp %>%
      select(-date, -all_of(response), -weight, -source_id) %>%
      as.matrix()
    
    dtrain_boot <- xgb.DMatrix(data = X_samp, label = y_samp, weight = samp$weight)
    
    xgb_params <- c(
      list(
        objective              = "reg:tweedie",
        eval_metric            = "poisson-nloglik"
      ),
      model_info$params
    )
    
    mod_b <- xgb.train(
      params  = xgb_params,
      data    = dtrain_boot,
      nrounds = model_info$nrounds,
      verbose = 0
    )
    
    tibble(
      iteration = i,
      date = pred_df$date,
      pred = predict(mod_b, dtest_pred)
    )
  })
  
  return(results)
}



#–– 5.5 prepare full date grid and get predictions -----------------------
# build full grid (train + future) & prep the same way
all_dates <- tibble(date = seq(invasion_start, Sys.Date(), by="day")) %>%
  mutate(day = as.numeric(date - invasion_start),
         weight = 1) %>%
  left_join(covars, by="date") %>%
  mutate(across(
    .cols = where(is.numeric) & !all_of(c("date","weight")),
    .fns = list(
      impute = ~replace_na(.x, min(.x, na.rm=TRUE) - 1),
      miss   = ~as.integer(is.na(.x))
    ),
    .names = "{.col}_{.fn}"
  ))

# Extract column names from the training datasets
train_cols_deaths <- deaths_p %>%
  select(-date, -deaths, -weight, -source_id) %>%
  colnames()

train_cols_casualties <- casual_p %>%
  select(-date, -casualties, -weight, -source_id) %>%
  colnames()

# Ensure X_all has the exact same columns as training data
X_all_deaths <- all_dates %>%
  select(all_of(train_cols_deaths)) %>%
  as.matrix()

X_all_casualties <- all_dates %>%
  select(all_of(train_cols_casualties)) %>%
  as.matrix()

# Convert to xgb.DMatrix
dtest_all_deaths <- xgb.DMatrix(data = X_all_deaths)
dtest_all_casualties <- xgb.DMatrix(data = X_all_casualties)

# Predict using the matched data
all_dates$deaths_fit <- predict(death_mod$model, dtest_all_deaths)
all_dates$casualties_fit <- predict(casual_mod$model, dtest_all_casualties)

# Run bootstraps
deaths_boot_raw <- boot_preds_group(death_mod, deaths_p, all_dates %>% select(date, weight, all_of(train_cols_deaths)), "deaths", B = 20)
casual_boot_raw <- boot_preds_group(casual_mod, casual_p, all_dates %>% select(date, weight, all_of(train_cols_casualties)), "casualties", B = 200)

  # Function to compute intervals
compute_intervals <- function(boot_data, confidence_levels = c(0.90, 0.95)) {
  daily_intervals <- boot_data %>%
    group_by(date) %>%
    summarise(
      fit = mean(pred),
      lo_95 = quantile(pred, 0.025),
      hi_95 = quantile(pred, 0.975),
      lo_90 = quantile(pred, 0.05),
      hi_90 = quantile(pred, 0.95)
    ) %>%
    arrange(date)
  
  cumulative_boot <- boot_data %>%
    arrange(iteration, date) %>%
    group_by(iteration) %>%
    mutate(cum_pred = cumsum(pred)) %>%
    ungroup()
  
  cumulative_intervals <- cumulative_boot %>%
    group_by(date) %>%
    summarise(
      cumulative_fit = mean(cum_pred),
      cumulative_lo_95 = quantile(cum_pred, 0.025),
      cumulative_hi_95 = quantile(cum_pred, 0.975),
      cumulative_lo_90 = quantile(cum_pred, 0.05),
      cumulative_hi_90 = quantile(cum_pred, 0.95)
    ) %>%
    arrange(date)
  
  list(daily = daily_intervals, cumulative = cumulative_intervals)
}

# Get intervals
deaths_intervals <- compute_intervals(deaths_boot_raw)
casualties_intervals <- compute_intervals(casual_boot_raw)

# Plot predictions:
ggplot() +
  geom_line(data = deaths_intervals$cumulative, aes(x = date, y = cumulative_fit), color = "red") +
  geom_ribbon(data = deaths_intervals$cumulative, aes(x = date, ymin = cumulative_lo_95, ymax = cumulative_hi_95), fill = "red", alpha = 0.2) +
  geom_ribbon(data = deaths_intervals$cumulative, aes(x = date, ymin = cumulative_lo_90, ymax = cumulative_hi_90), fill = "red", alpha = 0.4) +
  geom_line(data = casualties_intervals$cumulative, aes(x = date, y = cumulative_fit), color = "blue") +
  geom_ribbon(data = casualties_intervals$cumulative, aes(x = date, ymin = cumulative_lo_95, ymax = cumulative_hi_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = casualties_intervals$cumulative, aes(x = date, ymin = cumulative_lo_90, ymax = cumulative_hi_90), fill = "blue", alpha = 0.4) +
  theme_minimal() +
  labs(title = "Cumulative Predictions: Deaths (Red) and Casualties (Blue)",
       y = "Cumulative Count",
       x = "Date")+
  geom_point(data = all, aes(x=date, y=estimate, col=type))+
  geom_linerange(data = all, aes(x=date, ymin=estimate_low, ymax=estimate_high, col=type))


# Plot daily predictions
ggplot() +
  geom_line(data = deaths_intervals$daily, aes(x = date, y = fit), color = "red") +
  geom_ribbon(data = deaths_intervals$daily, aes(x = date, ymin = lo_95, ymax = hi_95), fill = "red", alpha = 0.2) +
  geom_ribbon(data = deaths_intervals$daily, aes(x = date, ymin = lo_90, ymax = hi_90), fill = "red", alpha = 0.4) +
  geom_line(data = casualties_intervals$daily, aes(x = date, y = fit), color = "blue") +
  geom_ribbon(data = casualties_intervals$daily, aes(x = date, ymin = lo_95, ymax = hi_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = casualties_intervals$daily, aes(x = date, ymin = lo_90, ymax = hi_90), fill = "blue", alpha = 0.4) +
  theme_minimal() +
  labs(title = "Daily Predictions: Deaths (Red) and Casualties (Blue)",
       y = "Daily Count",
       x = "Date")
  
#–– 5.7 Feature importance analysis ---------------------------------------
# Get feature importance for both models
deaths_importance <- xgb.importance(model = death_mod$model)
casualties_importance <- xgb.importance(model = casual_mod$model)

print("Deaths Model - Top 10 Features:")
print(head(deaths_importance, 10))

print("Casualties Model - Top 10 Features:")
print(head(casualties_importance, 10))

# Plot feature importance
if(require(ggplot2)) {
  p1 <- xgb.ggplot.importance(deaths_importance[1:10,]) + 
    ggtitle("Deaths Model: Feature Importance")
  
  p2 <- xgb.ggplot.importance(casualties_importance[1:10,]) + 
    ggtitle("Casualties Model: Feature Importance")
  
  print(p1)
  print(p2)
}
