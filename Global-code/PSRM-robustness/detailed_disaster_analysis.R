#!/usr/bin/env Rscript

# Detailed Disaster Analysis - Follow-up to examine specific cases and create cleaner summary
library(dplyr)
library(readr)
library(knitr)

# Read the data
data <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("=== DETAILED DISASTER TREATMENT ANALYSIS ===\n\n")

# 1. Clean summary of disaster occurrence variables
cat("1. DISASTER OCCURRENCE VARIABLES COVERAGE:\n")
cat("==========================================\n")

occurrence_summary <- data %>%
  summarise(
    disasters_all = list(total_events = sum(disasters_all),
                        months_with_disasters = sum(disasters_all > 0),
                        pct_months = round(mean(disasters_all > 0) * 100, 2)),

    disasters_flood = list(total_events = sum(disasters_flood),
                          months_with_disasters = sum(disasters_flood > 0),
                          pct_months = round(mean(disasters_flood > 0) * 100, 2)),

    disasters_w_damage = list(total_events = sum(disasters_w_damage),
                             months_with_disasters = sum(disasters_w_damage > 0),
                             pct_months = round(mean(disasters_w_damage > 0) * 100, 2)),

    disasters_w_reconstruction = list(total_events = sum(disasters_w_reconstruction),
                                     months_with_disasters = sum(disasters_w_reconstruction > 0),
                                     pct_months = round(mean(disasters_w_reconstruction > 0) * 100, 2))
  )

for (var in names(occurrence_summary)) {
  info <- occurrence_summary[[var]][[1]]
  cat(sprintf("%-25s: %2d events | %2d admin-months affected | %5.2f%% coverage\n",
              var, info$total_events, info$months_with_disasters, info$pct_months))
}

cat("\n2. SEVERITY MEASURES DATA AVAILABILITY:\n")
cat("=======================================\n")

disaster_months <- data %>% filter(disasters_all > 0)
n_disaster_months <- nrow(disaster_months)

severity_coverage <- disaster_months %>%
  summarise(
    total_deaths_sum = sum(total_deaths_sum > 0, na.rm = TRUE),
    no_affected_sum = sum(no_affected_sum > 0, na.rm = TRUE),
    total_affected_sum = sum(total_affected_sum > 0, na.rm = TRUE),
    no_homeless_sum = sum(no_homeless_sum > 0, na.rm = TRUE),
    total_damage_sum = sum(total_damage_sum > 0, na.rm = TRUE),
    reconstruction_costs_sum = sum(reconstruction_costs_sum > 0, na.rm = TRUE),
    no_injured_sum = sum(no_injured_sum > 0, na.rm = TRUE)
  )

for (var in names(severity_coverage)) {
  count <- severity_coverage[[var]]
  pct <- round(count / n_disaster_months * 100, 1)
  cat(sprintf("%-25s: %2d/%2d disaster months (%4.1f%% coverage)\n",
              var, count, n_disaster_months, pct))
}

cat("\n3. SEVERITY MEASURE DISTRIBUTIONS (Non-zero values only):\n")
cat("=========================================================\n")

severity_stats <- disaster_months %>%
  summarise(
    across(c(total_deaths_sum, no_affected_sum, total_affected_sum, no_homeless_sum,
             total_damage_sum, no_injured_sum),
           list(
             count = ~sum(. > 0, na.rm = TRUE),
             min = ~ifelse(sum(. > 0, na.rm = TRUE) > 0, min(.[. > 0], na.rm = TRUE), NA),
             median = ~ifelse(sum(. > 0, na.rm = TRUE) > 0, median(.[. > 0], na.rm = TRUE), NA),
             mean = ~ifelse(sum(. > 0, na.rm = TRUE) > 0, round(mean(.[. > 0], na.rm = TRUE), 0), NA),
             max = ~ifelse(sum(. > 0, na.rm = TRUE) > 0, max(.[. > 0], na.rm = TRUE), NA)
           ), .names = "{.col}_{.fn}")
  )

# Extract and display key variables
key_vars <- c("total_deaths_sum", "total_affected_sum", "no_affected_sum")
for (var in key_vars) {
  count <- severity_stats[[paste0(var, "_count")]]
  if (count > 0) {
    min_val <- severity_stats[[paste0(var, "_min")]]
    median_val <- severity_stats[[paste0(var, "_median")]]
    mean_val <- severity_stats[[paste0(var, "_mean")]]
    max_val <- severity_stats[[paste0(var, "_max")]]

    cat(sprintf("%-20s: n=%2d | Min: %8.0f | Median: %8.0f | Mean: %8.0f | Max: %8.0f\n",
                var, count, min_val, median_val, mean_val, max_val))
  } else {
    cat(sprintf("%-20s: No data available\n", var))
  }
}

cat("\n4. EXAMPLE DISASTER CASES:\n")
cat("==========================\n")

# Show some actual disaster cases with severity data
examples <- disaster_months %>%
  filter(total_deaths_sum > 0 | total_affected_sum > 0) %>%
  select(country_name, adm1_name, year_month, disasters_all, disasters_flood,
         total_deaths_sum, total_affected_sum, no_affected_sum, total_damage_sum) %>%
  arrange(desc(total_deaths_sum), desc(total_affected_sum)) %>%
  head(10)

print(examples)

cat("\n5. RECOMMENDED SPLITTING APPROACHES:\n")
cat("====================================\n")

# Calculate thresholds for different approaches
thresholds <- disaster_months %>%
  filter(total_deaths_sum > 0 | total_affected_sum > 0) %>%
  summarise(
    deaths_median = median(total_deaths_sum[total_deaths_sum > 0], na.rm = TRUE),
    deaths_75th = quantile(total_deaths_sum[total_deaths_sum > 0], 0.75, na.rm = TRUE),
    affected_median = median(total_affected_sum[total_affected_sum > 0], na.rm = TRUE),
    affected_75th = quantile(total_affected_sum[total_affected_sum > 0], 0.75, na.rm = TRUE),
    any_deaths = sum(total_deaths_sum > 0, na.rm = TRUE),
    total_with_severity = n()
  )

cat("Threshold options for severity splits:\n")
cat(sprintf("- Deaths median threshold: %.0f deaths (splits %d disasters)\n",
            thresholds$deaths_median, thresholds$any_deaths))
cat(sprintf("- Deaths 75th percentile: %.0f deaths\n", thresholds$deaths_75th))
cat(sprintf("- Total affected median: %.0f people (splits %d disasters)\n",
            thresholds$affected_median, thresholds$total_with_severity))
cat(sprintf("- Total affected 75th percentile: %.0f people\n", thresholds$affected_75th))

cat("\n6. PROPOSED SEVERITY CLASSIFICATION:\n")
cat("====================================\n")

# Test the recommended approach
disaster_classified <- disaster_months %>%
  mutate(
    # Approach A: Deaths-based (any deaths = severe)
    severity_deaths_binary = total_deaths_sum > 0,

    # Approach B: Deaths-based with median split
    severity_deaths_median = case_when(
      total_deaths_sum == 0 ~ "mild",
      total_deaths_sum >= thresholds$deaths_median ~ "severe",
      TRUE ~ "moderate"
    ),

    # Approach C: Affected people-based with median split
    severity_affected_median = case_when(
      total_affected_sum == 0 ~ "mild",
      total_affected_sum >= thresholds$affected_median ~ "severe",
      TRUE ~ "moderate"
    ),

    # Approach D: Combined (deaths OR high affected)
    severity_combined = case_when(
      total_deaths_sum > 0 ~ "severe",
      total_affected_sum >= thresholds$affected_75th ~ "severe",
      TRUE ~ "mild"
    )
  )

# Summary of classifications
classification_summary <- disaster_classified %>%
  summarise(
    total = n(),
    deaths_binary_severe = sum(severity_deaths_binary, na.rm = TRUE),
    deaths_median_severe = sum(severity_deaths_median == "severe", na.rm = TRUE),
    deaths_median_moderate = sum(severity_deaths_median == "moderate", na.rm = TRUE),
    affected_median_severe = sum(severity_affected_median == "severe", na.rm = TRUE),
    affected_median_moderate = sum(severity_affected_median == "moderate", na.rm = TRUE),
    combined_severe = sum(severity_combined == "severe", na.rm = TRUE)
  )

cat("Classification results:\n")
cat(sprintf("Total disaster months: %d\n", classification_summary$total))
cat(sprintf("Approach A (any deaths = severe): %d severe (%d%%)\n",
            classification_summary$deaths_binary_severe,
            round(classification_summary$deaths_binary_severe / classification_summary$total * 100)))
cat(sprintf("Approach B (deaths median split): %d severe, %d moderate (%d%%, %d%%)\n",
            classification_summary$deaths_median_severe,
            classification_summary$deaths_median_moderate,
            round(classification_summary$deaths_median_severe / classification_summary$total * 100),
            round(classification_summary$deaths_median_moderate / classification_summary$total * 100)))
cat(sprintf("Approach C (affected median split): %d severe, %d moderate (%d%%, %d%%)\n",
            classification_summary$affected_median_severe,
            classification_summary$affected_median_moderate,
            round(classification_summary$affected_median_severe / classification_summary$total * 100),
            round(classification_summary$affected_median_moderate / classification_summary$total * 100)))
cat(sprintf("Approach D (deaths OR high affected): %d severe (%d%%)\n",
            classification_summary$combined_severe,
            round(classification_summary$combined_severe / classification_summary$total * 100)))

cat("\n7. FINAL RECOMMENDATIONS:\n")
cat("=========================\n")
cat("Based on the analysis:\n\n")
cat("BEST OPTIONS for addressing Reviewer 2's heterogeneity concern:\n\n")
cat("1. PRIMARY RECOMMENDATION: Use 'total_affected_sum' with median split\n")
cat("   - High data coverage (97% of disaster months have this data)\n")
cat("   - Creates balanced groups (severe vs moderate vs mild)\n")
cat("   - Threshold: 28,926 people affected\n\n")
cat("2. ALTERNATIVE: Binary classification using any deaths\n")
cat("   - Clear distinction (any deaths = severe disaster)\n")
cat("   - Good coverage (91% have death data)\n")
cat("   - Creates two clear groups for heterogeneity analysis\n\n")
cat("3. Variables with insufficient data:\n")
cat("   - disasters_w_reconstruction: 0 events\n")
cat("   - total_damage_sum: only 6% coverage\n")
cat("   - reconstruction_costs_sum: 0% coverage\n\n")

cat("The 'disasters_all' variable captures the broadest set of disasters (52 events),\n")
cat("while severity can be assessed using total_affected_sum or total_deaths_sum.\n")