# Install packages if needed
# install.packages("tidyverse")

library(tidyverse)

# Read CSV file
df <- read_csv("65_plus_deaths_by_counties.csv")

# Function to create buckets based on "rate" for a specific year
create_buckets <- function(data, n_buckets = 50) {
  data <- data %>%
    arrange(rate) %>% # Sort counties by rate
    mutate(
      cumulative_population = cumsum(population),
      total_population = sum(population),
      bucket = ntile(cumulative_population / total_population, n_buckets)
    )
  return(data)
}

# Assign counties to buckets for each year
df_buckets <- df %>%
  group_by(year) %>%
  group_modify(~ create_buckets(.x)) %>%
  ungroup()

# Aggregate data at the bucket level
bucket_data <- df_buckets %>%
  group_by(year, bucket) %>%
  summarize(
    total_population = sum(population),
    total_deaths = sum(deaths),
    rate_avg = mean(rate, na.rm = TRUE),
    deaths_per_100k = (total_deaths / total_population) * 100000,
    .groups = "drop"
  )

# Fit linear models for 2015–2020 trends, grouped by bucket
trend_models <- bucket_data %>%
  filter(year >= 2015, year <= 2020) %>%
  group_by(bucket) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(deaths_per_100k ~ year, data = .x)),
    intercept = map_dbl(model, ~ coef(.x)[1]),
    slope = map_dbl(model, ~ coef(.x)[2])
  ) %>%
  select(bucket, intercept, slope)

# Join trend estimates back to the bucket-level data
bucket_data_with_preds <- bucket_data %>%
  left_join(trend_models, by = "bucket") %>%
  mutate(predicted_deaths_per_100k = intercept + slope * year)

# Calculate excess mortality for 2021–2024 at the bucket level
excess_mortality <- bucket_data_with_preds %>%
  filter(year >= 2021, year <= 2024) %>%
  mutate(
    excess_pct = 100 * (deaths_per_100k - predicted_deaths_per_100k) / 
      predicted_deaths_per_100k
  ) %>%
  select(
    year,
    bucket,
    total_population,
    total_deaths,
    rate_avg,
    deaths_per_100k,
    predicted_deaths_per_100k,
    excess_pct
  )

# View and save results
excess_mortality
write.csv(excess_mortality, file = "65_plus_excess_mortality_by_buckets.csv", row.names = FALSE)
