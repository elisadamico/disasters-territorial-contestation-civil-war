# Detailed temporal patterns analysis - specific examples
# To illustrate the contestation status instability patterns

library(dplyr)
library(readr)

# Read the data
data <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

# Read our analysis results
status_changes <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/status_changes_analysis.csv")

# Get country thresholds
country_means <- data %>%
  group_by(iso3c) %>%
  summarise(country_grid_risk_mean = mean(grid_risk_mean, na.rm = TRUE), .groups = 'drop')

# Select units with highest status changes for detailed examination
high_change_units <- status_changes %>%
  filter(has_status_change) %>%
  arrange(desc(total_changes)) %>%
  head(5)

cat("=== DETAILED TEMPORAL PATTERNS FOR MOST UNSTABLE UNITS ===\n\n")

for(i in 1:nrow(high_change_units)) {
  unit_info <- high_change_units[i,]

  cat("UNIT", i, ":", unit_info$country_name, "-", unit_info$adm1_name, "\n")
  cat("Total status changes:", unit_info$total_changes, "over 36 months\n")

  # Get detailed monthly data for this unit
  unit_data <- data %>%
    filter(iso3c == unit_info$iso3c, iso3c_adm1 == unit_info$iso3c_adm1) %>%
    left_join(country_means, by = "iso3c") %>%
    mutate(
      contestation_status = ifelse(grid_risk_mean > country_grid_risk_mean, "High", "Low"),
      above_threshold = grid_risk_mean > country_grid_risk_mean
    ) %>%
    arrange(year, month) %>%
    select(year, month, grid_risk_mean, country_grid_risk_mean, contestation_status, above_threshold)

  cat("Country threshold:", round(unit_data$country_grid_risk_mean[1], 4), "\n")
  cat("Unit's grid_risk_mean range:", round(min(unit_data$grid_risk_mean), 4), "to", round(max(unit_data$grid_risk_mean), 4), "\n")

  # Show monthly pattern
  cat("\nMonthly pattern (showing first 18 months):\n")
  cat("Year-Month | grid_risk | Threshold | Status\n")
  cat("-----------|-----------|-----------|-------\n")
  for(j in 1:min(18, nrow(unit_data))) {
    row <- unit_data[j,]
    cat(sprintf("%4d-%02d    | %9.4f | %9.4f | %s\n",
               row$year, row$month, row$grid_risk_mean, row$country_grid_risk_mean, row$contestation_status))
  }

  # Status change summary
  status_summary <- unit_data %>%
    group_by(contestation_status) %>%
    summarise(months = n(), .groups = 'drop')

  cat("\nStatus summary:\n")
  print(status_summary)
  cat("\n", rep("=", 60), "\n\n")
}

# Summary statistics on instability patterns
cat("=== OVERALL TEMPORAL INSTABILITY SUMMARY ===\n\n")

# Calculate within-country variability metrics
unit_variability <- data %>%
  filter(!is.na(grid_risk_mean)) %>%
  group_by(iso3c, iso3c_adm1, country_name, adm1_name) %>%
  summarise(
    n_months = n(),
    mean_risk = mean(grid_risk_mean),
    sd_risk = sd(grid_risk_mean),
    cv_risk = sd_risk / mean_risk,
    min_risk = min(grid_risk_mean),
    max_risk = max(grid_risk_mean),
    range_risk = max_risk - min_risk,
    .groups = 'drop'
  ) %>%
  left_join(country_means, by = "iso3c")

# Add threshold crossing info
unit_variability <- unit_variability %>%
  mutate(
    crosses_threshold = (min_risk <= country_grid_risk_mean & max_risk > country_grid_risk_mean),
    always_high = min_risk > country_grid_risk_mean,
    always_low = max_risk <= country_grid_risk_mean
  )

stability_summary <- unit_variability %>%
  summarise(
    total_units = n(),
    always_high_units = sum(always_high, na.rm = TRUE),
    always_low_units = sum(always_low, na.rm = TRUE),
    crossing_units = sum(crosses_threshold, na.rm = TRUE),
    pct_always_high = always_high_units / total_units * 100,
    pct_always_low = always_low_units / total_units * 100,
    pct_crossing = crossing_units / total_units * 100
  )

cat("Temporal stability classification:\n")
cat("Always High Contestation:", stability_summary$always_high_units, "units (", round(stability_summary$pct_always_high, 1), "%)\n")
cat("Always Low Contestation:", stability_summary$always_low_units, "units (", round(stability_summary$pct_always_low, 1), "%)\n")
cat("Cross Threshold:", stability_summary$crossing_units, "units (", round(stability_summary$pct_crossing, 1), "%)\n")

cat("\nIMPLICATIONS FOR RESEARCH DESIGN:\n")
cat("Only", round(stability_summary$pct_always_high + stability_summary$pct_always_low, 1), "% of units maintain stable contestation status.\n")
cat("This temporal instability could severely compromise DiD identification strategy.\n")