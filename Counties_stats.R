# Install packages if needed
# install.packages("tidyverse")

library(tidyverse)

# Read CSV file
df <- read_csv("65_plus_deaths_by_counties.csv")

# Compute deaths per 100,000 population
df <- df %>%
  mutate(deaths_per_100k = (deaths / population) * 100000)

#    For each county_code, fits a linear model for 2015–2020
#    to predict deaths_per_100k from year.
trend_models <- df %>%
  filter(year >= 2015, year <= 2020) %>%
  group_by(county_code) %>%
  # Nests the data so we can fit a separate LM for each county
  nest() %>%
  mutate(
    model = map(data, ~ lm(deaths_per_100k ~ year, data = .x)),
    # Extracts the coefficients from each model: intercept, slope
    intercept = map_dbl(model, ~ coef(.x)[1]),
    slope     = map_dbl(model, ~ coef(.x)[2])
  ) %>%
  select(county_code, intercept, slope)

#    Joins these slope/intercept estimates back to the full data
#    so we can calculate predicted deaths_per_100k for all years.
df_with_preds <- df %>%
  inner_join(trend_models, by = "county_code") %>%
  # For each row, compute the predicted value from the 2015–2020 trend.
  mutate(predicted_deaths_per_100k = intercept + slope * year)

#    Calculates excess mortality (%) for 2021–2024
#    Excess (%) = 100 * (actual - predicted) / predicted
excess_mortality <- df_with_preds %>%
  filter(year >= 2021, year <= 2024) %>%
  mutate(
    excess_pct = 100 * (deaths_per_100k - predicted_deaths_per_100k) / 
      predicted_deaths_per_100k
  ) %>%
  select(
    county_code, 
    year, 
    population,
    deaths, 
    rate, 
    deaths_per_100k, 
    predicted_deaths_per_100k, 
    excess_pct
  )

# Views the results
excess_mortality
write.csv(excess_mortality, file = "65_plus_excess_mortality.csv", row.names = FALSE)
