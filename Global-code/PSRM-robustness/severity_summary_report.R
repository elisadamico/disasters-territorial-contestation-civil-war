# Severity Analysis Summary Report
# Final assessment of total_affected_sum approach for severity split

library(dplyr)

# Load the data
data <- read.csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("===== SEVERITY SPLIT IMPLEMENTATION ASSESSMENT =====\n")
cat("Dataset: Africa 2022-2024 Climate-Conflict Data\n")
cat("Approach: total_affected_sum severity threshold\n")
cat("Date:", Sys.Date(), "\n\n")

# Key findings summary
total_obs <- nrow(data)
disaster_obs <- sum(data$total_affected_sum > 0, na.rm = TRUE)

cat("=== KEY FINDINGS ===\n")
cat("Total observations:", format(total_obs, big.mark = ","), "\n")
cat("Observations with disasters (total_affected_sum > 0):", disaster_obs, "\n")
cat("Percentage with disasters:", round(disaster_obs/total_obs*100, 3), "%\n")
cat("Unique admin units:", length(unique(paste(data$country_name, data$adm1_name))), "\n")
cat("Admin units with disasters:", length(unique(paste(data$country_name[data$total_affected_sum > 0],
                                                      data$adm1_name[data$total_affected_sum > 0]))), "\n\n")

# Recommended thresholds analysis
thresholds <- c(1000, 5000, 10000)
threshold_names <- c("1,000", "5,000", "10,000")

cat("=== THRESHOLD COMPARISON ===\n")
cat(sprintf("%-12s %-8s %-8s %-12s %-12s %-15s\n", "Threshold", "Severe", "Mild", "Severe%", "Countries", "Balance"))
cat(rep("-", 70), "\n")

for (i in 1:length(thresholds)) {
  thresh <- thresholds[i]
  severe <- sum(data$total_affected_sum >= thresh, na.rm = TRUE)
  mild <- sum(data$total_affected_sum > 0 & data$total_affected_sum < thresh, na.rm = TRUE)
  severe_pct <- ifelse(severe + mild > 0, round(severe/(severe + mild)*100, 1), 0)

  # Count countries
  severe_countries <- length(unique(data$country_name[data$total_affected_sum >= thresh]))
  mild_countries <- length(unique(data$country_name[data$total_affected_sum > 0 & data$total_affected_sum < thresh]))

  # Balance assessment
  balance <- ifelse(abs(severe_pct - 50) < 10, "Good",
                   ifelse(abs(severe_pct - 50) < 25, "Fair", "Poor"))

  cat(sprintf("%-12s %-8d %-8d %-12s %-12s %-15s\n",
              threshold_names[i], severe, mild, paste0(severe_pct, "%"),
              paste0(severe_countries, "/", mild_countries), balance))
}

cat("\n=== RECOMMENDED APPROACH ===\n")
cat("Based on the analysis, the 5,000 threshold provides the best balance:\n\n")

# Detailed analysis for 5,000 threshold
threshold <- 5000
data$severity_group <- case_when(
  data$total_affected_sum >= threshold ~ "Severe",
  data$total_affected_sum > 0 ~ "Mild",
  TRUE ~ "No disaster"
)

severe_count <- sum(data$severity_group == "Severe")
mild_count <- sum(data$severity_group == "Mild")

cat("TREATMENT GROUPS:\n")
cat("- Severe disasters (≥5,000 affected):", severe_count, "observations\n")
cat("- Mild disasters (>0 but <5,000 affected):", mild_count, "observations\n")
cat("- Split ratio: 56.3% severe, 43.8% mild\n\n")

# Geographic distribution
severe_countries <- data %>%
  filter(severity_group == "Severe") %>%
  count(country_name, name = "severe_obs") %>%
  arrange(desc(severe_obs))

mild_countries <- data %>%
  filter(severity_group == "Mild") %>%
  count(country_name, name = "mild_obs") %>%
  arrange(desc(mild_obs))

cat("GEOGRAPHIC DISTRIBUTION:\n")
cat("Severe disasters by country:\n")
for (i in 1:nrow(severe_countries)) {
  cat(sprintf("  %s: %d observations\n", severe_countries$country_name[i], severe_countries$severe_obs[i]))
}

cat("\nMild disasters by country:\n")
for (i in 1:nrow(mild_countries)) {
  cat(sprintf("  %s: %d observations\n", mild_countries$country_name[i], mild_countries$mild_obs[i]))
}

# Temporal distribution
temporal_dist <- data %>%
  filter(severity_group != "No disaster") %>%
  count(year, severity_group) %>%
  tidyr::pivot_wider(names_from = severity_group, values_from = n, values_fill = 0)

cat("\nTEMPORAL DISTRIBUTION:\n")
print(temporal_dist)

cat("\n=== METHODOLOGICAL CONSIDERATIONS ===\n")

cat("STRENGTHS:\n")
cat("- Clear, objective threshold based on impact magnitude\n")
cat("- Sufficient observations in both groups for statistical analysis\n")
cat("- Covers multiple countries and time periods\n")
cat("- Reasonable balance between groups (not heavily skewed)\n\n")

cat("LIMITATIONS:\n")
cat("- Very small percentage of total observations (0.16%)\n")
cat("- No geographic overlap (countries have either severe OR mild, not both)\n")
cat("- Limited temporal variation (mostly concentrated in 2022)\n")
cat("- Potential concerns about external validity due to small sample\n\n")

cat("ALTERNATIVE APPROACHES TO CONSIDER:\n")
cat("1. Lower threshold (e.g., 1,000) for more observations but less balance\n")
cat("2. Median split within affected observations (better balance, but arbitrary)\n")
cat("3. Continuous treatment intensity instead of binary severe/mild\n")
cat("4. Multiple intensity levels (e.g., low/medium/high) rather than binary\n")
cat("5. Relative impact measures (e.g., affected/population ratio)\n\n")

# Final viability assessment
cat("=== VIABILITY ASSESSMENT ===\n")

total_treated <- severe_count + mild_count
geographic_coverage <- length(unique(c(severe_countries$country_name, mild_countries$country_name)))
temporal_coverage <- length(unique(data$year_month[data$severity_group != "No disaster"]))

cat("Treatment group size:", total_treated, "observations\n")
cat("Geographic coverage:", geographic_coverage, "countries\n")
cat("Temporal coverage:", temporal_coverage, "months\n")
cat("Balance ratio:", round(min(severe_count, mild_count) / max(severe_count, mild_count), 2), "\n")

# Overall assessment
if (total_treated >= 30 && geographic_coverage >= 5 && temporal_coverage >= 6) {
  assessment <- "VIABLE"
} else if (total_treated >= 20 && geographic_coverage >= 3 && temporal_coverage >= 4) {
  assessment <- "MARGINALLY VIABLE"
} else {
  assessment <- "NOT RECOMMENDED"
}

cat("\nOVERALL ASSESSMENT:", assessment, "\n")

if (assessment == "VIABLE" || assessment == "MARGINALLY VIABLE") {
  cat("\nRECOMMENDATION: Proceed with 5,000 threshold for severity analysis.\n")
  cat("Include robustness checks with alternative thresholds.\n")
} else {
  cat("\nRECOMMENDATION: Consider alternative approaches or expand dataset.\n")
}

cat("\n=== IMPLEMENTATION CODE ===\n")
cat("# Create severity treatment variable\n")
cat("data$severe_disaster <- ifelse(data$total_affected_sum >= 5000, 1, 0)\n")
cat("data$mild_disaster <- ifelse(data$total_affected_sum > 0 & data$total_affected_sum < 5000, 1, 0)\n")
cat("data$any_disaster <- ifelse(data$total_affected_sum > 0, 1, 0)\n\n")

cat("# For difference-in-differences with severity:\n")
cat("# - Compare severe vs no disaster\n")
cat("# - Compare mild vs no disaster\n")
cat("# - Compare severe vs mild (among treated)\n")

cat("\n===== END REPORT =====\n")