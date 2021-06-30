# Load libraries
library(dplyr)
library(readr)
library(tidyr)

# Read in the data from the csv in this repo
df <- read_csv('forecast.csv')

# Restructure the data so that there is one observation per row
# and all of the relevant variables are columns
price_df <- df %>% filter(category == 'consumer_price') %>%
  dplyr::select(date = predict_time)

# Define the formula for the ordinary least squares regression
# 


the_formula <- as.formula('')