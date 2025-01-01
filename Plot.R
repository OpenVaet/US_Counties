# Loads necessary libraries
library(ggplot2)
library(dplyr)

# Reads data
df <- read.csv("65_plus_excess_mortality_by_buckets.csv")

# Computes min and max values
min_rate_avg <- min(df$rate_avg, na.rm = TRUE)
max_rate_avg <- max(df$rate_avg, na.rm = TRUE)
min_excess_pct <- min(df$excess_pct, na.rm = TRUE)
max_excess_pct <- max(df$excess_pct, na.rm = TRUE)

# Prints the values
cat("Min Vaccination Rate:", min_rate_avg, "\n")
cat("Max Vaccination Rate:", max_rate_avg, "\n")
cat("Min Excess Mortality:", min_excess_pct, "\n")
cat("Max Excess Mortality:", max_excess_pct, "\n")

# Modifies excess_pct values
df$excess_pct <- ifelse(df$excess_pct > 100, 100, df$excess_pct)
df$excess_pct <- ifelse(df$excess_pct < -50, -50, df$excess_pct)

# Function to fit a linear model and return a summary for each year
fit_model_by_year <- function(data) {
  lm_model <- lm(excess_pct ~ rate_avg, data = data)
  summary(lm_model)
}

# Splits data by year and apply the function
models_by_year <- df %>%
  group_by(year) %>%
  group_split() %>%
  lapply(fit_model_by_year)

# Prints results for each year
for (i in seq_along(models_by_year)) {
  cat("\nYear:", unique(df$year)[i], "\n")
  print(models_by_year[[i]])
}

# Creates the plot with regression line
ggplot(data = df, aes(x = rate_avg, y = excess_pct)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1) + # Use linewidth for line size
  facet_wrap(~ year) +
  labs(
    title = "US Counties - Excess Mortality vs. Vaccination Rate by Year",
    x = "Vaccination Rate (%)",
    y = "Excess Mortality (%)"
  ) +
  scale_y_continuous(limits = c(-50, 100)) + # Set Y-axis limits
  scale_x_continuous(limits = c(0, 100)) +  # Set X-axis limits
  geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 1) + # Use linewidth for dashed line
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # Increase title size
    axis.title = element_text(size = 14),               # Increase axis title size
    axis.text = element_text(size = 12),                # Increase axis text size
    legend.title = element_text(size = 14),             # Increase legend title size
    legend.text = element_text(size = 12),              # Increase legend text size
    strip.text = element_text(size = 14)                # Increase facet text size
  )

