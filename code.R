# Load libraries
library(dplyr)
library(readr)
library(tidyr)

# Read in the csv files
files <- dir(pattern = '_raw_inputs.csv')
out_list <- list()
for(i in 1:length(files)){
  this_file <- files[i]
  this_data <- read_csv(this_file)
  this_data <- this_data %>%
    dplyr::select(date = ds,
                  value = y) %>%
    mutate(date = lubridate::floor_date(date, 'month')) %>%
    mutate(year = lubridate::floor_date(date, 'year')) %>%
    mutate(month = date) %>%
    filter(!is.na(month))

  # Identify whether the data is yearly or not
  # If yearly, interpolate for month
  left <- tibble(month = seq(min(this_data$month, na.rm = TRUE),
                             max(this_data$month, na.rm = TRUE),
                             by = 'month'))
  joined <- left_join(left, this_data)
  # Forward fill
  joined <- joined %>% 
    arrange(month) %>%
    tidyr::fill(date, value, year, .direction = 'down')
  df <- joined
  
  this_data$key <- gsub('_raw_inputs.csv', '', this_file)
  out_list[[i]] <- this_data
}

df <- bind_rows(out_list)

# Get average monthly values
df <- df %>%
  group_by(month, key) %>%
  summarise(value = mean(value))

# Structure in "wide" rather than "long" data for better modeling
df <- df %>%
  pivot_wider(names_from = 'key',
              values_from = 'value')

# Combine all exports / imports to get trade balance
df$exports <- 
  df$trade_export_global +
  df$trade_export_eu 
df$imports <- 
  df$trade_import_eu +
  df$trade_import_global
df$trade_balance <- df$exports - df$imports

# Define the formula for the ordinary least squares regression
the_formula <- as.formula('consumer_price ~ population + exports + trade_balance')

# Build the model
fit <-lm(the_formula, data = df)

# Examine results
summary(fit)

# Predict on data
predictions <- predict(fit, newdata = df,
                       interval = 'prediction')
