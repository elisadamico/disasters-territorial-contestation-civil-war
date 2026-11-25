# Severity-Based DiD Analysis
# Test if severe vs mild disasters have different effects

library(dplyr)
library(did)

# Load the data
data <- read.csv("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")

cat("=== CREATING SEVERITY TREATMENT VARIABLES ===\n")

# Create severity splits based on total_affected_sum
data <- data %>%
  mutate(
    # Severe disasters: ≥5,000 people affected
    severe_disaster = ifelse(total_affected_sum >= 5000, 1, 0),
    # Mild disasters: >0 but <5,000 people affected
    mild_disaster = ifelse(total_affected_sum > 0 & total_affected_sum < 5000, 1, 0)
  )

# Create treatment timing - first treatment time for each unit
severe_timing <- data %>%
  filter(severe_disaster == 1) %>%
  group_by(iso3c_adm1_encoded) %>%
  summarise(first_severe_treat = min(year_month_encoded), .groups = 'drop')

mild_timing <- data %>%
  filter(mild_disaster == 1) %>%
  group_by(iso3c_adm1_encoded) %>%
  summarise(first_mild_treat = min(year_month_encoded), .groups = 'drop')

# Join back to main data
data <- data %>%
  left_join(severe_timing, by = "iso3c_adm1_encoded") %>%
  left_join(mild_timing, by = "iso3c_adm1_encoded") %>%
  mutate(
    severe_treat_time = ifelse(is.na(first_severe_treat), 0, first_severe_treat),
    mild_treat_time = ifelse(is.na(first_mild_treat), 0, first_mild_treat)
  )

# Count treatment units
severe_units <- data %>% filter(severe_disaster == 1) %>% distinct(iso3c_adm1_encoded) %>% nrow()
mild_units <- data %>% filter(mild_disaster == 1) %>% distinct(iso3c_adm1_encoded) %>% nrow()

cat("Severe disaster units:", severe_units, "\n")
cat("Mild disaster units:", mild_units, "\n\n")

# Prepare data for did package
data_severe <- data %>%
  mutate(
    treat_timing = severe_treat_time,
    treated = ifelse(severe_treat_time > 0, 1, 0)
  ) %>%
  filter(!is.na(civilian_bd_onesided), !is.na(battles), !is.na(total_idps))

data_mild <- data %>%
  mutate(
    treat_timing = mild_treat_time,
    treated = ifelse(mild_treat_time > 0, 1, 0)
  ) %>%
  filter(!is.na(civilian_bd_onesided), !is.na(battles), !is.na(total_idps))

cat("=== RUNNING DID ANALYSIS ===\n")

# Function to run DiD analysis safely
safe_att_gt <- function(data, outcome_var, name) {
  cat("Analyzing", name, "for", outcome_var, "...\n")

  tryCatch({
    result <- att_gt(
      yname = outcome_var,
      tname = "year_month_encoded",
      idname = "iso3c_adm1_encoded",
      gname = "treat_timing",
      data = data,
      control_group = "nevertreated"
    )

    agg_result <- aggte(result, type = "group")
    cat("Overall ATT for", name, outcome_var, ":", round(agg_result$overall.att, 4),
        " (p-value:", round(agg_result$overall.pval, 4), ")\n")

    return(list(result = result, agg = agg_result))

  }, error = function(e) {
    cat("Error in", name, outcome_var, ":", e$message, "\n")
    return(NULL)
  })
}

# Analyze key outcomes for severe disasters
cat("\n=== SEVERE DISASTERS ANALYSIS ===\n")
severe_repression <- safe_att_gt(data_severe, "civilian_bd_onesided", "SEVERE")
severe_military <- safe_att_gt(data_severe, "battles", "SEVERE")
severe_displacement <- safe_att_gt(data_severe, "total_idps", "SEVERE")

# Analyze key outcomes for mild disasters
cat("\n=== MILD DISASTERS ANALYSIS ===\n")
mild_repression <- safe_att_gt(data_mild, "civilian_bd_onesided", "MILD")
mild_military <- safe_att_gt(data_mild, "battles", "MILD")
mild_displacement <- safe_att_gt(data_mild, "total_idps", "MILD")

cat("\n=== COMPARISON SUMMARY ===\n")
cat("This analysis tests whether severe disasters (≥5,000 affected) have different effects\n")
cat("than mild disasters (<5,000 affected), addressing treatment heterogeneity criticism.\n")