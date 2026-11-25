# Temporal Stability Analysis of Contestation Status
# Analysis of grid_risk_mean stability over time

library(dplyr)
library(ggplot2)
library(readr)

# Read the data
cat("Reading data...\n")
data <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("Data dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Time period: 2022-2024 (36 months)\n")
cat("Countries:", length(unique(data$iso3c)), "\n")
cat("Admin units:", length(unique(data$iso3c_adm1)), "\n")

# Remove rows with missing grid_risk_mean
data_clean <- data %>%
  filter(!is.na(grid_risk_mean))

cat("Rows with valid grid_risk_mean:", nrow(data_clean), "\n")

# ============================================================================
# 1. ANALYSIS: Calculate within-country means for contestation thresholds
# ============================================================================
cat("\n=== ANALYSIS 1: COUNTRY-LEVEL THRESHOLDS ===\n")

country_means <- data_clean %>%
  group_by(iso3c) %>%
  summarise(
    country_grid_risk_mean = mean(grid_risk_mean, na.rm = TRUE),
    country_grid_risk_median = median(grid_risk_mean, na.rm = TRUE),
    country_grid_risk_sd = sd(grid_risk_mean, na.rm = TRUE),
    n_observations = n(),
    n_admin_units = n_distinct(iso3c_adm1),
    n_months = n_distinct(year_month),
    .groups = 'drop'
  )

print(country_means)

# ============================================================================
# 2. ANALYSIS: Unit-level temporal stability
# ============================================================================
cat("\n=== ANALYSIS 2: UNIT-LEVEL TEMPORAL STABILITY ===\n")

unit_stability <- data_clean %>%
  group_by(iso3c, iso3c_adm1, country_name, adm1_name) %>%
  summarise(
    mean_grid_risk = mean(grid_risk_mean, na.rm = TRUE),
    median_grid_risk = median(grid_risk_mean, na.rm = TRUE),
    sd_grid_risk = sd(grid_risk_mean, na.rm = TRUE),
    cv_grid_risk = ifelse(mean_grid_risk > 0, sd_grid_risk / mean_grid_risk, NA),
    min_grid_risk = min(grid_risk_mean, na.rm = TRUE),
    max_grid_risk = max(grid_risk_mean, na.rm = TRUE),
    range_grid_risk = max_grid_risk - min_grid_risk,
    n_observations = n(),
    n_zero_months = sum(grid_risk_mean == 0, na.rm = TRUE),
    pct_zero_months = n_zero_months / n_observations * 100,
    .groups = 'drop'
  )

# Add country means to unit data for threshold comparison
unit_stability <- unit_stability %>%
  left_join(country_means %>% select(iso3c, country_grid_risk_mean), by = "iso3c")

# Classify units as high/low contestation based on country mean
unit_stability <- unit_stability %>%
  mutate(
    contestation_status = ifelse(mean_grid_risk > country_grid_risk_mean, "High", "Low")
  )

cat("Summary of temporal stability metrics:\n")
print(summary(unit_stability %>% select(mean_grid_risk, sd_grid_risk, cv_grid_risk, range_grid_risk)))

cat("\nUnits with highest temporal variability (CV > 1):\n")
high_variability <- unit_stability %>%
  filter(!is.na(cv_grid_risk) & cv_grid_risk > 1) %>%
  arrange(desc(cv_grid_risk))
print(head(high_variability, 10))

cat("\nDistribution of contestation status:\n")
print(table(unit_stability$contestation_status))

# ============================================================================
# 3. ANALYSIS: Threshold crossing analysis
# ============================================================================
cat("\n=== ANALYSIS 3: THRESHOLD CROSSING ANALYSIS ===\n")

# For each unit, check if it crosses the country threshold over time
threshold_crossing <- data_clean %>%
  left_join(country_means %>% select(iso3c, country_grid_risk_mean), by = "iso3c") %>%
  group_by(iso3c, iso3c_adm1, country_name, adm1_name) %>%
  summarise(
    country_threshold = first(country_grid_risk_mean),
    months_above_threshold = sum(grid_risk_mean > country_grid_risk_mean, na.rm = TRUE),
    months_below_threshold = sum(grid_risk_mean <= country_grid_risk_mean, na.rm = TRUE),
    total_months = n(),
    crosses_threshold = (months_above_threshold > 0 & months_below_threshold > 0),
    pct_months_high = months_above_threshold / total_months * 100,
    .groups = 'drop'
  )

cat("Threshold crossing summary:\n")
cat("Units that cross their country threshold:", sum(threshold_crossing$crosses_threshold), "out of", nrow(threshold_crossing), "\n")
cat("Percentage of units that cross threshold:", round(mean(threshold_crossing$crosses_threshold) * 100, 1), "%\n")

# Show units that cross thresholds
crossing_units <- threshold_crossing %>%
  filter(crosses_threshold) %>%
  arrange(desc(pct_months_high))

cat("\nUnits that cross their country's contestation threshold:\n")
print(crossing_units)

# ============================================================================
# 4. ANALYSIS: Month-by-month contestation status changes
# ============================================================================
cat("\n=== ANALYSIS 4: MONTH-BY-MONTH STATUS CHANGES ===\n")

# Calculate contestation status for each unit-month
monthly_status <- data_clean %>%
  left_join(country_means %>% select(iso3c, country_grid_risk_mean), by = "iso3c") %>%
  mutate(
    contestation_status = ifelse(grid_risk_mean > country_grid_risk_mean, "High", "Low")
  ) %>%
  arrange(iso3c, iso3c_adm1, year, month)

# Track status changes for each unit
status_changes <- monthly_status %>%
  group_by(iso3c, iso3c_adm1, country_name, adm1_name) %>%
  mutate(
    status_lag = lag(contestation_status),
    status_change = (contestation_status != status_lag & !is.na(status_lag))
  ) %>%
  summarise(
    total_months = n(),
    total_changes = sum(status_change, na.rm = TRUE),
    has_status_change = total_changes > 0,
    .groups = 'drop'
  )

cat("Status change summary:\n")
cat("Units with status changes:", sum(status_changes$has_status_change), "out of", nrow(status_changes), "\n")
cat("Percentage with status changes:", round(mean(status_changes$has_status_change) * 100, 1), "%\n")

# Show units with most status changes
changing_units <- status_changes %>%
  filter(has_status_change) %>%
  arrange(desc(total_changes))

cat("\nUnits with most status changes:\n")
print(head(changing_units, 10))

# ============================================================================
# 5. ANALYSIS: Country-specific patterns
# ============================================================================
cat("\n=== ANALYSIS 5: COUNTRY-SPECIFIC PATTERNS ===\n")

country_stability <- threshold_crossing %>%
  group_by(iso3c, country_name) %>%
  summarise(
    n_units = n(),
    units_crossing_threshold = sum(crosses_threshold),
    pct_units_crossing = units_crossing_threshold / n_units * 100,
    mean_threshold = first(country_threshold),
    .groups = 'drop'
  ) %>%
  arrange(desc(pct_units_crossing))

cat("Countries with highest percentage of units crossing thresholds:\n")
print(head(country_stability, 15))

# ============================================================================
# 6. SAVE RESULTS
# ============================================================================
cat("\n=== SAVING RESULTS ===\n")

# Save detailed results
write_csv(unit_stability, "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/unit_temporal_stability.csv")
write_csv(threshold_crossing, "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/threshold_crossing_analysis.csv")
write_csv(status_changes, "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/status_changes_analysis.csv")
write_csv(country_stability, "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/country_stability_summary.csv")

cat("Analysis complete! Results saved to CSV files.\n")

# ============================================================================
# 7. RESEARCH DESIGN IMPLICATIONS
# ============================================================================
cat("\n=== RESEARCH DESIGN IMPLICATIONS ===\n")

total_units <- nrow(unit_stability)
stable_units <- nrow(unit_stability) - sum(threshold_crossing$crosses_threshold)
unstable_units <- sum(threshold_crossing$crosses_threshold)

cat("SUMMARY FOR DIFFERENCE-IN-DIFFERENCES IDENTIFICATION:\n")
cat("============================================\n")
cat("Total admin units:", total_units, "\n")
cat("Stable units (never cross threshold):", stable_units, "(", round(stable_units/total_units*100, 1), "%)\n")
cat("Unstable units (cross threshold):", unstable_units, "(", round(unstable_units/total_units*100, 1), "%)\n")
cat("\nIMPLICATION: If", round(unstable_units/total_units*100, 1), "% of units change contestation status,")
cat(" this could violate the stable treatment group assumption for DiD.\n")

# Check distribution of high vs low contestation
high_contestation <- sum(unit_stability$contestation_status == "High")
low_contestation <- sum(unit_stability$contestation_status == "Low")

cat("\nTreatment group distribution:\n")
cat("High contestation units:", high_contestation, "(", round(high_contestation/total_units*100, 1), "%)\n")
cat("Low contestation units:", low_contestation, "(", round(low_contestation/total_units*100, 1), "%)\n")