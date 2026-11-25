#!/usr/bin/env Rscript

# Disaster Treatment Variables Analysis for Severity Splitting
# Purpose: Analyze treatment variables to identify best options for mild vs severe disaster categories

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read the data
cat("Loading data...\n")
data <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("Dataset dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("Year range:", min(data$year), "-", max(data$year), "\n\n")

# 1. DISASTER OCCURRENCE VARIABLES ANALYSIS
cat("=== 1. DISASTER OCCURRENCE VARIABLES ===\n")

disaster_vars <- c("disasters_all", "disasters_w_reconstruction", "disasters_w_damage", "disasters_flood", "disasters_earthquake")

occurrence_stats <- data %>%
  summarise(
    across(all_of(disaster_vars), list(
      total_events = sum,
      pct_nonzero = ~mean(. > 0) * 100,
      max_events = max,
      mean_events = mean,
      admin_units_affected = ~sum(. > 0)
    ))
  )

# Reshape for better display
occurrence_long <- occurrence_stats %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("disaster_type", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value)

print(occurrence_long)
cat("\n")

# 2. SEVERITY MEASURES ANALYSIS
cat("=== 2. SEVERITY MEASURES ANALYSIS ===\n")

severity_vars <- c("total_deaths_sum", "no_affected_sum", "no_homeless_sum",
                  "total_affected_sum", "total_damage_sum", "total_damage_adjusted_sum",
                  "reconstruction_costs_sum", "no_injured_sum")

severity_stats <- data %>%
  summarise(
    across(all_of(severity_vars), list(
      total = sum,
      pct_nonzero = ~mean(. > 0, na.rm = TRUE) * 100,
      mean_nonzero = ~mean(.[. > 0], na.rm = TRUE),
      median_nonzero = ~median(.[. > 0], na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      observations_nonzero = ~sum(. > 0, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )

# Reshape for better display
severity_long <- severity_stats %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("severity_measure", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value)

print(severity_long)
cat("\n")

# 3. DATA COMPLETENESS ANALYSIS
cat("=== 3. DATA COMPLETENESS ANALYSIS ===\n")

# Check disasters with severity data vs just occurrence
completeness_analysis <- data %>%
  filter(disasters_all > 0) %>%
  summarise(
    total_disaster_months = n(),
    has_deaths = sum(total_deaths_sum > 0, na.rm = TRUE),
    has_affected = sum(total_affected_sum > 0, na.rm = TRUE),
    has_homeless = sum(no_homeless_sum > 0, na.rm = TRUE),
    has_damage = sum(total_damage_sum > 0, na.rm = TRUE),
    has_reconstruction = sum(reconstruction_costs_sum > 0, na.rm = TRUE),
    has_injured = sum(no_injured_sum > 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    pct_has_deaths = has_deaths / total_disaster_months * 100,
    pct_has_affected = has_affected / total_disaster_months * 100,
    pct_has_homeless = has_homeless / total_disaster_months * 100,
    pct_has_damage = has_damage / total_disaster_months * 100,
    pct_has_reconstruction = has_reconstruction / total_disaster_months * 100,
    pct_has_injured = has_injured / total_disaster_months * 100
  )

print(completeness_analysis)
cat("\n")

# 4. TREATMENT TIMING VARIABLES ANALYSIS
cat("=== 4. TREATMENT TIMING VARIABLES ANALYSIS ===\n")

treatment_vars <- c("disasters_all_treat", "disasters_all_bin",
                   "disasters_w_reconstruction_treat", "disasters_w_reconstruction_bin",
                   "disasters_w_damage_treat", "disasters_w_damage_bin",
                   "disasters_flood_treat", "disasters_flood_bin")

treatment_stats <- data %>%
  summarise(
    across(all_of(treatment_vars), list(
      unique_values = ~length(unique(.)),
      min = min,
      max = max,
      mean = mean
    ))
  )

treatment_long <- treatment_stats %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("treatment_var", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value)

print(treatment_long)
cat("\n")

# 5. DISTRIBUTION ANALYSIS FOR SPLITTING
cat("=== 5. SEVERITY DISTRIBUTION ANALYSIS (for splitting options) ===\n")

# Focus on observations with disasters
disaster_data <- data %>% filter(disasters_all > 0)

cat("Disaster observations:", nrow(disaster_data), "out of", nrow(data), "total observations\n\n")

# Distribution of key severity measures
severity_distributions <- disaster_data %>%
  summarise(
    across(all_of(severity_vars), list(
      p25 = ~quantile(.[. > 0], 0.25, na.rm = TRUE),
      p50 = ~quantile(.[. > 0], 0.50, na.rm = TRUE),
      p75 = ~quantile(.[. > 0], 0.75, na.rm = TRUE),
      p90 = ~quantile(.[. > 0], 0.90, na.rm = TRUE),
      p95 = ~quantile(.[. > 0], 0.95, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )

# Reshape for display
distribution_long <- severity_distributions %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("severity_measure", "percentile"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = percentile, values_from = value)

print(distribution_long)
cat("\n")

# 6. RECOMMENDED SPLITTING STRATEGIES
cat("=== 6. RECOMMENDED SPLITTING STRATEGIES ===\n")

# Calculate potential splits based on different measures
splitting_recommendations <- disaster_data %>%
  summarise(
    # Deaths-based splits (above median deaths where deaths > 0)
    deaths_available = sum(total_deaths_sum > 0, na.rm = TRUE),
    deaths_median = median(total_deaths_sum[total_deaths_sum > 0], na.rm = TRUE),

    # Affected people splits (above median where affected > 0)
    affected_available = sum(total_affected_sum > 0, na.rm = TRUE),
    affected_median = median(total_affected_sum[total_affected_sum > 0], na.rm = TRUE),

    # Damage-based splits
    damage_available = sum(total_damage_sum > 0, na.rm = TRUE),
    damage_median = median(total_damage_sum[total_damage_sum > 0], na.rm = TRUE),

    # Combined severity indicator
    any_severity = sum(total_deaths_sum > 0 | total_affected_sum > 0 | total_damage_sum > 0, na.rm = TRUE)
  )

print(splitting_recommendations)
cat("\n")

# 7. SPECIFIC RECOMMENDATIONS
cat("=== 7. SPECIFIC RECOMMENDATIONS FOR REVIEWER 2 ===\n")

# Test different splitting approaches
test_splits <- disaster_data %>%
  mutate(
    # Approach 1: Deaths-based (if deaths > 0, use median split; otherwise mild)
    severe_deaths = ifelse(total_deaths_sum > 0,
                          total_deaths_sum >= median(total_deaths_sum[total_deaths_sum > 0], na.rm = TRUE),
                          FALSE),

    # Approach 2: Total affected (if affected > 0, use median split; otherwise mild)
    severe_affected = ifelse(total_affected_sum > 0,
                            total_affected_sum >= median(total_affected_sum[total_affected_sum > 0], na.rm = TRUE),
                            FALSE),

    # Approach 3: Any severe indicator (deaths OR high affected OR high damage)
    severe_any = (total_deaths_sum > 0) |
                 (total_affected_sum >= quantile(total_affected_sum[total_affected_sum > 0], 0.75, na.rm = TRUE)) |
                 (total_damage_sum >= quantile(total_damage_sum[total_damage_sum > 0], 0.75, na.rm = TRUE)),

    # Approach 4: Multiple criteria (deaths + affected)
    severe_multi = (total_deaths_sum > 0 & total_affected_sum > median(total_affected_sum[total_affected_sum > 0], na.rm = TRUE))
  )

split_summary <- test_splits %>%
  summarise(
    total_disasters = n(),
    severe_deaths_count = sum(severe_deaths, na.rm = TRUE),
    severe_affected_count = sum(severe_affected, na.rm = TRUE),
    severe_any_count = sum(severe_any, na.rm = TRUE),
    severe_multi_count = sum(severe_multi, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    severe_deaths_pct = severe_deaths_count / total_disasters * 100,
    severe_affected_pct = severe_affected_count / total_disasters * 100,
    severe_any_pct = severe_any_count / total_disasters * 100,
    severe_multi_pct = severe_multi_count / total_disasters * 100
  )

print(split_summary)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key files for further analysis:\n")
cat("- Dataset:", "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv\n")
cat("- Analysis script:", "/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/PSRM Rejection Revision test/code/disaster_treatment_analysis.R\n")