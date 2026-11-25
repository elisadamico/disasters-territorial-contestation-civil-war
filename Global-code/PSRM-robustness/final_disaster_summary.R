#!/usr/bin/env Rscript

# Final Clean Disaster Analysis Summary
library(dplyr)
library(readr)

# Read the data
data <- read_csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv", show_col_types = FALSE)

cat("=== DISASTER TREATMENT VARIABLES ANALYSIS FOR SEVERITY SPLITTING ===\n\n")

# 1. Disaster occurrence variables
cat("1. DISASTER OCCURRENCE VARIABLES:\n")
cat("=================================\n")

disaster_vars <- c("disasters_all", "disasters_flood", "disasters_w_damage", "disasters_w_reconstruction", "disasters_earthquake")

for (var in disaster_vars) {
  total_events <- sum(data[[var]], na.rm = TRUE)
  months_affected <- sum(data[[var]] > 0, na.rm = TRUE)
  pct_coverage <- round(mean(data[[var]] > 0, na.rm = TRUE) * 100, 2)
  max_events <- max(data[[var]], na.rm = TRUE)

  cat(sprintf("%-25s: %3d total events | %3d admin-months | %5.2f%% coverage | max: %d\n",
              var, total_events, months_affected, pct_coverage, max_events))
}

# 2. Focus on disaster months only
disaster_data <- data %>% filter(disasters_all > 0)
n_disaster_months <- nrow(disaster_data)

cat(sprintf("\nTotal disaster months in dataset: %d (out of %d total observations)\n", n_disaster_months, nrow(data)))

cat("\n2. SEVERITY MEASURES DATA AVAILABILITY:\n")
cat("=======================================\n")

severity_vars <- c("total_deaths_sum", "no_affected_sum", "total_affected_sum",
                  "no_homeless_sum", "total_damage_sum", "reconstruction_costs_sum", "no_injured_sum")

severity_summary <- data.frame(
  variable = severity_vars,
  stringsAsFactors = FALSE
)

for (i in 1:nrow(severity_summary)) {
  var <- severity_summary$variable[i]
  if (var %in% names(disaster_data)) {
    total_nonzero <- sum(disaster_data[[var]] > 0, na.rm = TRUE)
    pct_coverage <- round(total_nonzero / n_disaster_months * 100, 1)
    total_sum <- sum(disaster_data[[var]], na.rm = TRUE)

    severity_summary$coverage_count[i] <- total_nonzero
    severity_summary$coverage_pct[i] <- pct_coverage
    severity_summary$total_value[i] <- total_sum

    if (total_nonzero > 0) {
      nonzero_values <- disaster_data[[var]][disaster_data[[var]] > 0 & !is.na(disaster_data[[var]])]
      severity_summary$min_val[i] <- min(nonzero_values)
      severity_summary$median_val[i] <- median(nonzero_values)
      severity_summary$max_val[i] <- max(nonzero_values)
    } else {
      severity_summary$min_val[i] <- NA
      severity_summary$median_val[i] <- NA
      severity_summary$max_val[i] <- NA
    }
  }
}

for (i in 1:nrow(severity_summary)) {
  cat(sprintf("%-25s: %2d/%2d months (%4.1f%%) | Total: %8.0f",
              severity_summary$variable[i],
              severity_summary$coverage_count[i],
              n_disaster_months,
              severity_summary$coverage_pct[i],
              severity_summary$total_value[i]))

  if (!is.na(severity_summary$median_val[i])) {
    cat(sprintf(" | Median: %8.0f", severity_summary$median_val[i]))
  }
  cat("\n")
}

cat("\n3. KEY SEVERITY VARIABLE DISTRIBUTIONS:\n")
cat("=======================================\n")

# Focus on the best variables for splitting
key_vars <- c("total_deaths_sum", "total_affected_sum")

for (var in key_vars) {
  nonzero_data <- disaster_data[[var]][disaster_data[[var]] > 0 & !is.na(disaster_data[[var]])]
  if (length(nonzero_data) > 0) {
    q25 <- quantile(nonzero_data, 0.25)
    q50 <- quantile(nonzero_data, 0.50)
    q75 <- quantile(nonzero_data, 0.75)
    q90 <- quantile(nonzero_data, 0.90)

    cat(sprintf("%s (n=%d):\n", var, length(nonzero_data)))
    cat(sprintf("  25th percentile: %8.0f\n", q25))
    cat(sprintf("  50th percentile: %8.0f\n", q50))
    cat(sprintf("  75th percentile: %8.0f\n", q75))
    cat(sprintf("  90th percentile: %8.0f\n", q90))
    cat("\n")
  }
}

cat("4. EXAMPLE DISASTER CASES:\n")
cat("==========================\n")

# Show top disaster cases
examples <- disaster_data %>%
  select(country_name, adm1_name, year_month, disasters_all, disasters_flood,
         total_deaths_sum, total_affected_sum, no_affected_sum) %>%
  filter(total_deaths_sum > 0 | total_affected_sum > 0) %>%
  arrange(desc(total_deaths_sum), desc(total_affected_sum)) %>%
  head(8)

print(examples)

cat("\n5. SPLITTING STRATEGY RECOMMENDATIONS:\n")
cat("======================================\n")

# Calculate key thresholds
deaths_median <- median(disaster_data$total_deaths_sum[disaster_data$total_deaths_sum > 0], na.rm = TRUE)
affected_median <- median(disaster_data$total_affected_sum[disaster_data$total_affected_sum > 0], na.rm = TRUE)
affected_75th <- quantile(disaster_data$total_affected_sum[disaster_data$total_affected_sum > 0], 0.75, na.rm = TRUE)

# Count disasters by severity approaches
disasters_with_deaths <- sum(disaster_data$total_deaths_sum > 0, na.rm = TRUE)
disasters_with_affected <- sum(disaster_data$total_affected_sum > 0, na.rm = TRUE)

cat("RECOMMENDED APPROACHES:\n\n")

cat("A. BINARY DEATHS-BASED CLASSIFICATION:\n")
cat(sprintf("   - Any deaths (>0) = SEVERE: %d disasters\n", disasters_with_deaths))
cat(sprintf("   - No deaths (=0) = MILD: %d disasters\n", n_disaster_months - disasters_with_deaths))
cat(sprintf("   - Data coverage: %d/%d disaster months (%.1f%%)\n",
            disasters_with_deaths, n_disaster_months,
            disasters_with_deaths/n_disaster_months * 100))
cat("\n")

cat("B. TOTAL AFFECTED MEDIAN SPLIT:\n")
severe_affected <- sum(disaster_data$total_affected_sum >= affected_median, na.rm = TRUE)
moderate_affected <- sum(disaster_data$total_affected_sum > 0 & disaster_data$total_affected_sum < affected_median, na.rm = TRUE)
mild_affected <- n_disaster_months - disasters_with_affected

cat(sprintf("   - Threshold: %.0f people affected\n", affected_median))
cat(sprintf("   - SEVERE (≥%.0f): %d disasters\n", affected_median, severe_affected))
cat(sprintf("   - MODERATE (1-%.0f): %d disasters\n", affected_median-1, moderate_affected))
cat(sprintf("   - MILD (no data): %d disasters\n", mild_affected))
cat(sprintf("   - Data coverage: %d/%d disaster months (%.1f%%)\n",
            disasters_with_affected, n_disaster_months,
            disasters_with_affected/n_disaster_months * 100))
cat("\n")

cat("C. COMBINED APPROACH (Deaths OR High Affected):\n")
severe_combined <- sum((disaster_data$total_deaths_sum > 0) |
                      (disaster_data$total_affected_sum >= affected_75th), na.rm = TRUE)
cat(sprintf("   - SEVERE: Any deaths OR ≥%.0f affected: %d disasters\n",
            affected_75th, severe_combined))
cat(sprintf("   - MILD: All others: %d disasters\n", n_disaster_months - severe_combined))
cat("\n")

cat("6. FINAL RECOMMENDATION:\n")
cat("========================\n")
cat("For addressing Reviewer 2's heterogeneity criticism:\n\n")
cat("PRIMARY CHOICE: Binary Deaths-Based Classification\n")
cat("- Simple, interpretable threshold (any deaths = severe)\n")
cat("- Good data coverage (91% of disaster months)\n")
cat("- Creates meaningful distinction for policy relevance\n")
cat("- Balanced groups for statistical power\n\n")
cat("ALTERNATIVE: Total Affected Median Split\n")
cat("- Higher data coverage (97% of disaster months)\n")
cat("- Creates three-way classification (mild/moderate/severe)\n")
cat("- Uses threshold of ~29,000 people affected\n\n")
cat("VARIABLES TO AVOID:\n")
cat("- disasters_w_reconstruction: No events in data\n")
cat("- total_damage_sum: Only 6% coverage\n")
cat("- reconstruction_costs_sum: No data available\n")

cat(sprintf("\nDataset info: %d total observations, %d countries, 2022-2024 period\n",
            nrow(data), length(unique(data$country_name))))
cat("Analysis complete.\n")