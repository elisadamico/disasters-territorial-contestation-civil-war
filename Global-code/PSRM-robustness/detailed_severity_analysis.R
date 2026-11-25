# Detailed Severity Analysis - Geographic and Temporal Patterns
# Focus on understanding the viability of treatment groups

library(dplyr)
library(ggplot2)

# Load the data
data <- read.csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("=== DETAILED ANALYSIS OF SEVERITY SPLIT VIABILITY ===\n\n")

# Focus on 5,000 threshold as it provides most balanced groups
threshold <- 5000

# Create severity categories
data$severity_group <- case_when(
  data$total_affected_sum >= threshold ~ "Severe",
  data$total_affected_sum > 0 ~ "Mild",
  TRUE ~ "No disaster"
)

cat("Using threshold of", threshold, "affected people\n\n")

# 1. TEMPORAL DISTRIBUTION
cat("=== TEMPORAL DISTRIBUTION ===\n")
temporal_analysis <- data %>%
  filter(severity_group != "No disaster") %>%
  group_by(year_month, severity_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(year_month)

print(temporal_analysis)

# Monthly summary
monthly_summary <- data %>%
  filter(severity_group != "No disaster") %>%
  group_by(year, month, severity_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(year, month)

cat("\nMonthly distribution:\n")
print(monthly_summary)

# 2. GEOGRAPHIC CONCENTRATION
cat("\n=== GEOGRAPHIC CONCENTRATION ===\n")

# Country-level analysis
country_analysis <- data %>%
  filter(severity_group != "No disaster") %>%
  group_by(country_name, severity_group) %>%
  summarise(
    observations = n(),
    unique_admin_units = n_distinct(adm1_name),
    .groups = 'drop'
  ) %>%
  arrange(desc(observations))

cat("Country-level distribution:\n")
print(country_analysis)

# Admin unit level detail
admin_detail <- data %>%
  filter(severity_group != "No disaster") %>%
  group_by(country_name, adm1_name, severity_group) %>%
  summarise(
    observations = n(),
    total_affected_range = paste(min(total_affected_sum), "-", max(total_affected_sum)),
    avg_affected = round(mean(total_affected_sum)),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_affected))

cat("\nAdmin unit level detail:\n")
print(admin_detail)

# 3. BALANCE ASSESSMENT
cat("\n=== TREATMENT GROUP BALANCE ASSESSMENT ===\n")

# Geographic spread
severe_countries <- unique(data$country_name[data$severity_group == "Severe"])
mild_countries <- unique(data$country_name[data$severity_group == "Mild"])

cat("Countries with severe disasters:", length(severe_countries), "\n")
cat("Countries with mild disasters:", length(mild_countries), "\n")
cat("Countries with both types:", length(intersect(severe_countries, mild_countries)), "\n")

cat("\nCountries only in severe group:", setdiff(severe_countries, mild_countries), "\n")
cat("Countries only in mild group:", setdiff(mild_countries, severe_countries), "\n")
cat("Countries with both types:", intersect(severe_countries, mild_countries), "\n")

# 4. ROBUSTNESS CHECKS WITH DIFFERENT THRESHOLDS
cat("\n=== ROBUSTNESS CHECKS ===\n")

thresholds_to_test <- c(2000, 3000, 7500, 15000, 25000)

robustness_results <- data.frame()

for (thresh in thresholds_to_test) {
  severe_count <- sum(data$total_affected_sum >= thresh, na.rm = TRUE)
  mild_count <- sum(data$total_affected_sum > 0 & data$total_affected_sum < thresh, na.rm = TRUE)

  # Geographic spread
  temp_data <- data
  temp_data$temp_severity <- case_when(
    temp_data$total_affected_sum >= thresh ~ "Severe",
    temp_data$total_affected_sum > 0 ~ "Mild",
    TRUE ~ "No disaster"
  )

  severe_countries_n <- length(unique(temp_data$country_name[temp_data$temp_severity == "Severe"]))
  mild_countries_n <- length(unique(temp_data$country_name[temp_data$temp_severity == "Mild"]))
  both_countries_n <- length(intersect(
    unique(temp_data$country_name[temp_data$temp_severity == "Severe"]),
    unique(temp_data$country_name[temp_data$temp_severity == "Mild"])
  ))

  robustness_results <- rbind(robustness_results, data.frame(
    threshold = thresh,
    severe_obs = severe_count,
    mild_obs = mild_count,
    severe_pct = round(severe_count/(severe_count + mild_count)*100, 1),
    severe_countries = severe_countries_n,
    mild_countries = mild_countries_n,
    both_countries = both_countries_n
  ))
}

cat("Robustness check results:\n")
print(robustness_results)

# 5. SPECIFIC EXAMPLES FOR UNDERSTANDING
cat("\n=== SPECIFIC EXAMPLES ===\n")

# Show some specific disasters for context
severe_examples <- data %>%
  filter(severity_group == "Severe") %>%
  select(country_name, adm1_name, year_month, total_affected_sum, disasters_all, disasters_flood) %>%
  arrange(desc(total_affected_sum)) %>%
  head(10)

cat("Top 10 severe disaster observations:\n")
print(severe_examples)

mild_examples <- data %>%
  filter(severity_group == "Mild") %>%
  select(country_name, adm1_name, year_month, total_affected_sum, disasters_all, disasters_flood) %>%
  arrange(desc(total_affected_sum)) %>%
  head(10)

cat("\nTop 10 mild disaster observations:\n")
print(mild_examples)

# 6. TREATMENT ASSIGNMENT VIABILITY
cat("\n=== TREATMENT ASSIGNMENT VIABILITY ===\n")

# Check if we have enough variation for DID
viability_check <- data %>%
  group_by(country_name, adm1_name) %>%
  summarise(
    total_obs = n(),
    any_severe = any(severity_group == "Severe"),
    any_mild = any(severity_group == "Mild"),
    severe_months = sum(severity_group == "Severe"),
    mild_months = sum(severity_group == "Mild"),
    no_disaster_months = sum(severity_group == "No disaster"),
    .groups = 'drop'
  ) %>%
  filter(any_severe | any_mild)

cat("Admin units with treatment variation:\n")
cat("Units with only severe disasters:", sum(viability_check$any_severe & !viability_check$any_mild), "\n")
cat("Units with only mild disasters:", sum(!viability_check$any_severe & viability_check$any_mild), "\n")
cat("Units with both types:", sum(viability_check$any_severe & viability_check$any_mild), "\n")

# Show units with both types
both_types <- viability_check %>%
  filter(any_severe & any_mild) %>%
  arrange(desc(severe_months + mild_months))

if (nrow(both_types) > 0) {
  cat("\nAdmin units with both severe and mild disasters:\n")
  print(both_types)
}

cat("\n=== CONCLUSION ===\n")
cat("The", threshold, "threshold provides:\n")
cat("- Severe group:", sum(data$severity_group == "Severe"), "observations\n")
cat("- Mild group:", sum(data$severity_group == "Mild"), "observations\n")
cat("- Covers", length(severe_countries), "countries with severe and", length(mild_countries), "countries with mild disasters\n")
cat("- Geographic overlap in", length(intersect(severe_countries, mild_countries)), "countries\n")

total_treated <- sum(data$severity_group %in% c("Severe", "Mild"))
cat("- Total treated observations:", total_treated, "(", round(total_treated/nrow(data)*100, 2), "% of dataset)\n")

cat("\nThis appears to be a viable split for severity analysis.\n")