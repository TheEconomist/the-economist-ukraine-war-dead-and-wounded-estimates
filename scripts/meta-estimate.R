# This scripts reads in estimates of casualties and dead in the Russia-Ukraine war and uses them to construct a meta-estimate of those fallen in the conflict. There is currently not enough data on Ukrainian casualties or deaths to produce a live-updating estimate.

# 1. Load packages ------------------------------------------------------------
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# 2. Load source data ------------------------------------------------------------
all <- read_csv('source-data/deaths-and-casualties-data/Soldier deaths_casualties in Ukraine - estimates.csv') %>% 
  filter(is.na(`ignore for chart, data from warring parties`)) %>%
  mutate(date = as.Date(date, format = '%d/%m/%Y'),
         source = ifelse(source == 'UK Ministry of Defence', 'UK MoD', source)) %>%
  mutate(source = ifelse(source == 'Pentagon leak', 'US DoD', source))
all$source_id <- paste0('all_df_', all$source, '_', seq_len(nrow(all)))

casualties_via_mod <- read_csv('source-data/deaths-and-casualties-data/Soldier deaths_casualties in Ukraine - uk mod month-by-month estimates.csv') %>% 
  mutate(date = as.Date(as.character(`date (at beginning of next month)`), format = '%d/%m/%Y')) %>%
  filter(date <= as.Date('2025-05-01')) %>%
  mutate(estimate_low = NA,
         estimate_high = NA,
         estimate = `cumulative casualties`,
         country = 'russia',
         source = 'UK MoD')
casualties_via_mod$source_id <- paste0('casualties_via_mod_', seq_len(nrow(casualties_via_mod)))

# Define invasion start date:
invasion_start <- as.Date('2022-02-24')

# Test for recent data availability. This is coupled with a "date updated" as part of the exports of this script:
if (max(casualties_via_mod$date, na.rm = TRUE) < Sys.Date() - 30*3 &&
    max(all$date, na.rm = TRUE)        < Sys.Date() - 30*3) {
  stop("No recent enough data available, declining update.")
}

# Source: https://meduza.io/en/feature/2024/07/05/a-new-estimate-from-meduza-and-mediazona-shows-the-rate-of-russian-military-deaths-in-ukraine-is-only-growing N
deaths_via_meduza <- read_csv('source-data/deaths-and-casualties-data/meduza_2024.csv') %>%
  # This chunk ensures that format stays consistent
  rename(date = Date,
         "Estimate based on probate data" = `Оценка на основе данных РНД`,
         "Flash estimate (by date of discovery)" = `Оперативная оценка, прогноз`) %>%                    
  mutate(estimate = ifelse(!is.na(`Estimate based on probate data`), 
                           `Estimate based on probate data`, `Flash estimate (by date of discovery)`),
         estimate_low = ifelse(!is.na(ED_low), 
                               ED_low, ED_onlyprediction_low),
         estimate_high = ifelse(!is.na(ED_high), 
                                ED_high, ED_onlyprediction_high),
         country = 'russia') %>%
  arrange(date) %>%
  mutate(date = if_else(date == as.Date('2022-02-21'), invasion_start, date)) # Fix for invasion starting mid-week

deaths_via_meduza$source_id <- paste0('deaths_via_meduza_', seq_len(nrow(deaths_via_meduza)))
deaths_via_meduza$source <- 'Meduza'

# We add on estimated deaths since: 
deaths_via_meduza_weekly <- read_csv('source-data/deaths-and-casualties-data/meduza_2025_weekly.csv') %>% 
  rename(date = Date) %>%
  filter(date <= as.Date('2024-11-18')) %>% # Removes some estimates marked as affected by reporting lags
  mutate(estimate = ifelse(!is.na(`Estimate based on probate data`), 
                           `Estimate based on probate data`, `Flash estimate (by date of discovery)`),
         estimate_low = ifelse(!is.na(ED_low), 
                               ED_low, ED_prediction_low),
         estimate_high = ifelse(!is.na(ED_high), 
                                ED_high, ED_prediction_high),
         country = 'russia') %>%
  arrange(date) %>%
  mutate(date = if_else(date == as.Date('2022-02-21'), invasion_start, date)) %>%
  filter(date > max(deaths_via_meduza$date)) %>%
  mutate(estimate = cumsum(estimate) + deaths_via_meduza$estimate[deaths_via_meduza$date == max(deaths_via_meduza$date)],
         estimate_low = cumsum(estimate_low) + deaths_via_meduza$estimate_low[deaths_via_meduza$date == max(deaths_via_meduza$date)],
         estimate_high = cumsum(estimate_high) + deaths_via_meduza$estimate_high[deaths_via_meduza$date == max(deaths_via_meduza$date)]) 

deaths_via_meduza_weekly$source_id <- paste0('deaths_via_meduza_weekly', seq_len(nrow(deaths_via_meduza_weekly)))
deaths_via_meduza_weekly$source <- 'Meduza'

deaths_via_meduza <- rbind(deaths_via_meduza[, intersect(names(deaths_via_meduza), names(deaths_via_meduza_weekly))], 
                           deaths_via_meduza_weekly[, intersect(names(deaths_via_meduza), names(deaths_via_meduza_weekly))])

# Inspect that this works
ggplot(deaths_via_meduza, aes(x=date, y=estimate))+geom_line()+geom_line(aes(y=estimate_low))+geom_line(aes(y=estimate_high))+
  geom_line(data=deaths_via_meduza_weekly, aes(x=date, y=estimate, col='2025-weekly'), size = 2, alpha = 0.5)+
  geom_line(data=deaths_via_meduza_weekly, aes(x=date, y=estimate_low, col='2025-weekly'), size = 2, alpha = 0.5)+
  geom_line(data=deaths_via_meduza_weekly, aes(x=date, y=estimate_high, col='2025-weekly'), size = 2, alpha = 0.5)+labs(title = 'Check of Meduza data merge (cumulative + recent weekly observations)', x='', y='')

# 3. Construct two data sources -----------------------------------------------
all <- all %>%
  filter(
    !(is.na(estimate_low)  & !is.na(estimate_high)),
    !(!is.na(estimate_low) &  is.na(estimate_high)),
    country == 'russia'
  )

deaths_cumulative     <- all %>% filter(type == 'deaths') %>%
  select(-source_url)

casualties_cumulative <- all %>% filter(type == 'casualties') %>%
  select(-source_url)


# Drop only the recoded UK MoD rows here
casualties_cumulative <- casualties_cumulative %>%
  filter(!source %in% 'UK MoD')

# Merge in UK MoD data
casualties_cumulative <- rbind(casualties_cumulative %>% 
                                 select(-type, -`ignore for chart, data from warring parties`),
                               casualties_via_mod[, colnames(casualties_via_mod) %in% colnames(casualties_cumulative)]) 
# Drop the "Mediazona; Meduza" data already in the main dataset
deaths_cumulative <- deaths_cumulative %>%
  filter(!source %in% c('Mediazona; Meduza'))

# Merge in Meduza data:
deaths_cumulative <- rbind(deaths_cumulative[, intersect(colnames(deaths_cumulative), 
                                                         colnames(deaths_via_meduza))], 
                           deaths_via_meduza[, intersect(colnames(deaths_cumulative), 
                                                         colnames(deaths_via_meduza))])

# Finally, transfer both datasets to long format: we take lower, upper, and central estimates as our prediction target. This is not perfect: for a small number of sources we only have a lower bound.
casualties_cumulative <- casualties_cumulative %>% 
  pivot_longer(cols=c(estimate, estimate_low, estimate_high), 
               names_to = 'estimate_type', 
               values_to = 'estimate')

deaths_cumulative <- deaths_cumulative %>% 
  pivot_longer(cols=c(estimate, estimate_low, estimate_high), 
               names_to = 'estimate_type', 
               values_to = 'estimate')

# 4. Load covariates and merge these in ---------------------------------------
fires   <- read_csv('source-data/deaths-and-casualties-data/strikes_by_location_and_day.csv')
control <- read_csv('source-data/deaths-and-casualties-data/area_assessed_as_controlled_by_russia.csv')

covars <- fires %>% left_join(control) %>% mutate(days_since_invasion = as.numeric(date) - as.numeric(invasion_start)) %>% filter(date >= invasion_start)

# Process covariates and convert them to cumulative format:
covars_cumulative <- covars %>%
  arrange(date) %>%
  mutate(
    # Cumulative exposure measures based on the sum of daily fires by area, logged
    total_daily_log_war_fires_to_date = cumsum(ifelse(is.na(war_fires_per_day), 0, ifelse(war_fires_per_day > 0, log(war_fires_per_day), 0))),
    total_daily_log_war_fires_to_date_in_ukraine_held_area = cumsum(ifelse(is.na(war_fires_per_day_in_ukraine_held_area), 0, ifelse(war_fires_per_day_in_ukraine_held_area > 0, log(war_fires_per_day_in_ukraine_held_area), 0))),
    total_daily_log_war_fires_to_date_in_russia_held_area = cumsum(ifelse(is.na(war_fires_per_day_in_russia_held_area), 0, ifelse(war_fires_per_day_in_russia_held_area > 0, log(war_fires_per_day_in_russia_held_area), 0))),
    total_cloud_cover_in_east_of_country_to_date = cumsum(ifelse(is.na(cloud_cover_in_country), 0, cloud_cover_in_country)),
    total_change_in_area_assessed_as_russia_controlled = cumsum(abs(change_in_area_assessed_as_russia_controlled))) %>%
  
  # Select applicable covariates
  select(
    date,
    total_daily_log_war_fires_to_date,
    total_daily_log_war_fires_to_date_in_russia_held_area,
    total_daily_log_war_fires_to_date_in_ukraine_held_area,
    total_cloud_cover_in_east_of_country_to_date,
    total_change_in_area_assessed_as_russia_controlled,
  )

# Ensure we have all dates since the war began:
war <- tibble(date = seq.Date(from = invasion_start, to = Sys.Date(), by = "day")) %>%
  mutate(days_since_invasion = as.numeric(date - min(date)))
covars <- covars %>% full_join(war, by = "date") %>% filter(date >= invasion_start)
covars_cumulative <- covars_cumulative %>% full_join(war, by = "date") %>% filter(date >= invasion_start)

# Merge in covariates:
casualties_cumulative <- casualties_cumulative %>% full_join(covars_cumulative, by='date')
deaths_cumulative <- deaths_cumulative %>% full_join(covars_cumulative, by='date')

# Compute weights (each estimate given the same weight, even if upper + lower bound given, for instance):
casualties_cumulative <- casualties_cumulative %>%
  group_by(source_id) %>%
  mutate(weight = 1 / sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(days_since_invasion = as.numeric(date - min(date)))

deaths_cumulative <- deaths_cumulative %>%
  group_by(source_id) %>%
  mutate(weight = 1 / sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(days_since_invasion = as.numeric(date - min(date)))

# 5. Generate GAM prediction for both casualties and deaths based on day of the war alone. We will model the residuals of this using our covariates. This is important because in this way can project forward in time rigorously  ---------------------------------------
generate_gam_prediction <- function(estimate_df = casualties_cumulative,
                                    pred_df = covars_cumulative,
                                    gtitle = "Casualties",
                                    gam_formula = estimate ~ 
                                      
                                      # baseline trend using days since invasion
                                      s(days_since_invasion, k = 10) + 
                                      days_since_invasion +
                                      
                                      # war-fires and visibility
                                      te(total_daily_log_war_fires_to_date,          
                                         total_cloud_cover_in_east_of_country_to_date, k = c(3, 3)) +
                                      te(total_daily_log_war_fires_to_date_in_ukraine_held_area,          
                                         total_cloud_cover_in_east_of_country_to_date, k = c(3, 3)) +
                                      te(total_daily_log_war_fires_to_date_in_russia_held_area,          
                                         total_cloud_cover_in_east_of_country_to_date, k = c(3, 3)) +
                                      
                                      # change in territory over time
                                      s(total_change_in_area_assessed_as_russia_controlled, k = 3)){
  
  library(mgcv)
  gam_model <- gam(
    formula = gam_formula,
    data    = estimate_df,
    weights = weight,
    family  = tw(link = "log"),  
    method  = "REML",
    select  = TRUE
  )
  
  # Generate predictions:
  casualty_preds <- predict(
    gam_model,
    newdata = pred_df,
    type = "link",
    se.fit = TRUE
  )
  
  # convert link-scale fits to response-scale means
  predictions <- tibble(date = seq.Date(from = invasion_start, to = Sys.Date(), by = "day")) %>%
    mutate(days_since_invasion = as.numeric(date - min(date))) %>% 
    mutate(
      fit_link = casualty_preds$fit,
      se_link  = casualty_preds$se.fit,
      fit      = exp(fit_link),
      ci_lower = exp(fit_link - 1.96 * se_link),
      ci_upper = exp(fit_link + 1.96 * se_link)
    )
  
  # retrieve Tweedie parameters
  phi   <- summary(gam_model)$dispersion
  p     <- gam_model$family$getTheta(TRUE)
  mu    <- predictions$fit  # on response scale
  var_y <- phi * mu^p
  
  # on link scale, approximate pi-se:
  se_pi_link <- sqrt(predictions$se_link^2 + (var_y / mu^2))
  
  predictions <- predictions %>%
    mutate(
      pi_low  = exp(fit_link - 1.96*se_pi_link),
      pi_high = exp(fit_link + 1.96*se_pi_link)
    )
  
  # Plot
  p <- ggplot(predictions, aes(x = date)) +
    geom_ribbon(aes(ymin = pi_low, ymax = pi_high), alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.3) +
    geom_line(aes(y = fit), color = "steelblue", size = 1) +
    labs(
      title = paste0("Predicted ", gtitle, ": mean ±95% CI (darker) and 95% PI (lighter)"),
      subtitle = 'Dots = data. Narrow band is confidence interval, wide is prediction interval',
      y     = "Casualties",
      x     = "Date"
    ) +
    theme_minimal()+
    geom_point(data=estimate_df, aes(x=date, y=estimate))
  print(p)
  
  # Smooth to lowest level of the estimates (7-day), ensure monotonically increasing
  predictions <- predictions %>% arrange(date)
  if(sum(duplicated(predictions$date))> 0){
    stop('Duplicated dates!')
  }
  
  
  
  # Set a rolling average for the specified columns with dynamic window size
  library(zoo)
  for(i in c('fit', 'ci_lower', 'ci_upper', 'pi_low', 'pi_high')) {
    predictions[, i] <- rollapply(predictions[, i], width = 7, FUN = function(x) mean(x, na.rm = T), fill = NA, align = 'right', partial = TRUE)
  }
  
  # Ensure all monotonically increasing
  for(i in c('fit', 'ci_lower', 'ci_upper', 'pi_low', 'pi_high')){
    predictions[, i] <- cummax(predictions[, i])
  }
  
  
  p <- ggplot(predictions, aes(x = date)) +
    geom_ribbon(aes(ymin = pi_low, ymax = pi_high), alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.3) +
    geom_line(aes(y = fit), color = "steelblue", size = 1) +
    labs(
      title = paste0("Predicted ", gtitle, ": mean ±95% CI (darker) and 95% PI (lighter)"),
      subtitle = 'Dots = data. Narrow band is confidence interval, wide is prediction interval',
      y     = "Casualties",
      x     = "Date"
    ) +
    theme_minimal()+
    geom_point(data=estimate_df, aes(x=date, y=estimate))
  print(p)
  
  predictions <- predictions %>% rename(estimate = fit) %>% select(-fit_link, -se_link)
  
  return(predictions)}

# Casualties:
gam_casualties <- generate_gam_prediction(estimate_df = casualties_cumulative, gtitle="Casualties") %>%
  mutate(type = 'casualties',
         country = 'russia')

# Deaths:
gam_deaths <- generate_gam_prediction(estimate_df = deaths_cumulative, gtitle='Deaths') %>%
  mutate(type = 'deaths',
         country = 'russia')

# Export data
write_csv(gam_casualties %>% arrange(desc(date)), 'output-data/tracker/meta-estimate-casualties.csv')
write_csv(gam_deaths %>% arrange(desc(date)), 'output-data/tracker/meta-estimate-deaths.csv')

# Export chart
library(scales)
ggplot(gam_casualties, aes(x = date)) +
  geom_ribbon(aes(ymin = pi_low, ymax = pi_high), alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = estimate), color = "steelblue", size = 1) +
  geom_point(data=casualties_cumulative, aes(x=date, y=estimate), color='steelblue', alpha = 0.5)+
  
  geom_ribbon(data = gam_deaths, aes(ymin = pi_low, ymax = pi_high), alpha = 0.2) +
  geom_ribbon(data = gam_deaths, aes(ymin = ci_lower, ymax = ci_upper), fill = "darkblue", alpha = 0.3) +
  geom_line(data = gam_deaths, aes(y = estimate), color = "darkblue", size = 1) +
  geom_point(data=deaths_cumulative, aes(x=date, y=estimate), color='darkblue', alpha = 0.5) + 
  
  labs(
    title = paste0("Predicted Russian casualties and deaths: mean ±95% CI (darker) and 95% PI (lighter)"),
    subtitle = 'Dots = data. Dark blue = deaths, light blue = casualties. Narrow band is confidence interval, wide is prediction interval',
    y     = "",
    x     = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # White background for the plot panel
    plot.background = element_rect(fill = "white")   # White background for the entire plot
  )+
  scale_y_continuous(labels = label_comma())
ggsave('plots/meta-estimate.png', width = 12, height = 8)

# Number of estimates used:

# Add a column to identify the source of the data (deaths or casualties)
deaths_cumulative <- deaths_cumulative %>%
  mutate(source_type = "deaths")
casualties_cumulative <- casualties_cumulative %>%
  mutate(source_type = "casualties")

# Combine the data, filter out NAs in 'estimate', and select 'source' and 'source_type'
combined_data <- rbind(deaths_cumulative, casualties_cumulative) %>%
  filter(!is.na(estimate)) %>%
  select(source, source_type, weight)

# Create a table counting the occurrences for each source
source_count <- combined_data %>%
  group_by(source, source_type) %>%
  summarise(count = sum(weight)) %>%
  spread(key = source_type, value = count, fill = 0)

# Create the overall count for each source
overall_count <- combined_data %>%
  group_by(source) %>%
  summarise(Overall = sum(weight))

# Merge overall count with deaths and casualties counts
final_table <- source_count %>%
  left_join(overall_count, by = "source")

# Add a final row with the sum across all sources
final_row <- tibble(
  source = "Total",
  deaths = sum(final_table$deaths, na.rm = TRUE),
  casualties = sum(final_table$casualties, na.rm = TRUE),
  Overall = sum(final_table$Overall, na.rm = TRUE)
)

# Combine the final table with the summary row
final_table_with_total <- bind_rows(final_table, final_row)

# Write the final table with the total row to CSV
write_csv(final_table_with_total, "output-data/tracker/meta_estimate_sources_summary.csv")
