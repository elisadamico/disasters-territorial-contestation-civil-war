# Severity Analysis for total_affected_sum
# Analysis of different thresholds for severe vs mild disasters

library(dplyr)
library(ggplot2)

# Load the data
data <- read.csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("Dataset dimensions:", dim(data), "\n")
cat("Total observations:", nrow(data), "\n\n")

# Basic descriptives of total_affected_sum
cat("=== TOTAL_AFFECTED_SUM DESCRIPTIVES ===\n")
cat("Summary statistics:\n")
print(summary(data$total_affected_sum))

# Count non-zero values (any affected)
any_affected <- sum(data$total_affected_sum > 0, na.rm = TRUE)
cat("\nObservations with any affected (> 0):", any_affected, "\n")
cat("Percentage of total observations:", round(any_affected/nrow(data)*100, 2), "%\n")

# Calculate median for median split
median_affected <- median(data$total_affected_sum[data$total_affected_sum > 0], na.rm = TRUE)
cat("Median value among affected observations:", median_affected, "\n\n")

# Function to analyze severity splits
analyze_threshold <- function(threshold_value, threshold_name) {
  cat("=== THRESHOLD:", threshold_name, "(", threshold_value, ") ===\n")

  # Create severity categories
  data$severity_group <- case_when(
    data$total_affected_sum >= threshold_value ~ "Severe",
    data$total_affected_sum > 0 ~ "Mild",
    TRUE ~ "No disaster"
  )

  # Count treatment units
  severe_count <- sum(data$severity_group == "Severe", na.rm = TRUE)
  mild_count <- sum(data$severity_group == "Mild", na.rm = TRUE)
  no_disaster_count <- sum(data$severity_group == "No disaster", na.rm = TRUE)

  cat("Severe disasters (>= ", threshold_value, "):", severe_count, "\n")
  cat("Mild disasters (> 0 but < ", threshold_value, "):", mild_count, "\n")
  cat("No disasters:", no_disaster_count, "\n")

  # Calculate percentages
  total_treated <- severe_count + mild_count
  if (total_treated > 0) {
    cat("Percentage severe among treated units:", round(severe_count/total_treated*100, 2), "%\n")
    cat("Percentage mild among treated units:", round(mild_count/total_treated*100, 2), "%\n")
  }

  # Geographic distribution - countries with severe disasters
  if (severe_count > 0) {
    severe_countries <- data %>%
      filter(severity_group == "Severe") %>%
      group_by(country_name) %>%
      summarise(
        severe_units = n(),
        unique_admin_units = n_distinct(adm1_name),
        .groups = 'drop'
      ) %>%
      arrange(desc(severe_units))

    cat("\nCountries with severe disasters:\n")
    print(severe_countries)

    # Admin units with severe disasters
    severe_admin <- data %>%
      filter(severity_group == "Severe") %>%
      group_by(country_name, adm1_name) %>%
      summarise(
        severe_observations = n(),
        max_affected = max(total_affected_sum, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(max_affected))

    cat("\nTop admin units with severe disasters:\n")
    print(head(severe_admin, 10))
  }

  # Geographic distribution - countries with mild disasters
  if (mild_count > 0) {
    mild_countries <- data %>%
      filter(severity_group == "Mild") %>%
      group_by(country_name) %>%
      summarise(
        mild_units = n(),
        unique_admin_units = n_distinct(adm1_name),
        .groups = 'drop'
      ) %>%
      arrange(desc(mild_units))

    cat("\nCountries with mild disasters (top 10):\n")
    print(head(mild_countries, 10))
  }

  cat("\n" , rep("=", 60), "\n\n")

  return(list(
    threshold = threshold_value,
    threshold_name = threshold_name,
    severe_count = severe_count,
    mild_count = mild_count,
    no_disaster_count = no_disaster_count,
    total_treated = total_treated
  ))
}

# Analyze different thresholds
thresholds <- list(
  list(1000, "1,000 affected"),
  list(5000, "5,000 affected"),
  list(10000, "10,000 affected"),
  list(round(median_affected), paste("Median split (~", format(round(median_affected), big.mark = ","), " affected)", sep = ""))
)

# Store results
results <- list()
for (i in seq_along(thresholds)) {
  results[[i]] <- analyze_threshold(thresholds[[i]][[1]], thresholds[[i]][[2]])
}

# Summary comparison
cat("=== THRESHOLD COMPARISON SUMMARY ===\n")
comparison_df <- data.frame(
  Threshold = sapply(results, function(x) x$threshold_name),
  Threshold_Value = sapply(results, function(x) x$threshold),
  Severe_Count = sapply(results, function(x) x$severe_count),
  Mild_Count = sapply(results, function(x) x$mild_count),
  Total_Treated = sapply(results, function(x) x$total_treated),
  Severe_Percent = round(sapply(results, function(x)
    ifelse(x$total_treated > 0, x$severe_count/x$total_treated*100, 0)), 2)
)

print(comparison_df)

# Additional analysis: Distribution of affected values
cat("\n=== DISTRIBUTION ANALYSIS ===\n")

# Look at the actual distribution of non-zero values
non_zero_affected <- data$total_affected_sum[data$total_affected_sum > 0]
cat("Distribution of non-zero total_affected_sum values:\n")
print(quantile(non_zero_affected, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE))

# Count observations by ranges
cat("\nCount of observations by ranges:\n")
ranges <- list(
  "1-999" = sum(data$total_affected_sum >= 1 & data$total_affected_sum < 1000, na.rm = TRUE),
  "1,000-4,999" = sum(data$total_affected_sum >= 1000 & data$total_affected_sum < 5000, na.rm = TRUE),
  "5,000-9,999" = sum(data$total_affected_sum >= 5000 & data$total_affected_sum < 10000, na.rm = TRUE),
  "10,000-49,999" = sum(data$total_affected_sum >= 10000 & data$total_affected_sum < 50000, na.rm = TRUE),
  "50,000-99,999" = sum(data$total_affected_sum >= 50000 & data$total_affected_sum < 100000, na.rm = TRUE),
  "100,000+" = sum(data$total_affected_sum >= 100000, na.rm = TRUE)
)

for (range_name in names(ranges)) {
  cat(range_name, ":", ranges[[range_name]], "\n")
}

# Unique admin units analysis
cat("\n=== UNIQUE ADMIN UNITS ANALYSIS ===\n")
unique_units_analysis <- data %>%
  group_by(country_name, adm1_name) %>%
  summarise(
    any_disaster = any(total_affected_sum > 0, na.rm = TRUE),
    max_affected = max(total_affected_sum, na.rm = TRUE),
    total_disaster_months = sum(total_affected_sum > 0, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Total unique admin units:", nrow(unique_units_analysis), "\n")
cat("Admin units with any disasters:", sum(unique_units_analysis$any_disaster), "\n")

# For each threshold, count unique admin units
for (i in seq_along(results)) {
  threshold_val <- results[[i]]$threshold
  severe_units <- sum(unique_units_analysis$max_affected >= threshold_val, na.rm = TRUE)
  mild_units <- sum(unique_units_analysis$max_affected > 0 & unique_units_analysis$max_affected < threshold_val, na.rm = TRUE)

  cat("Threshold", results[[i]]$threshold_name, ":\n")
  cat("  - Unique admin units with severe disasters:", severe_units, "\n")
  cat("  - Unique admin units with mild disasters:", mild_units, "\n")
}

cat("\nAnalysis complete!\n")