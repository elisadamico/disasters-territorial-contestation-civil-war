###############################
### COMPREHENSIVE ROBUSTNESS CHECKS
### All 8 outcomes (4 outcomes × 2 risk levels)
###############################

library(readr)
library(dplyr)
library(did)
library(data.table)
library(xtable)

set.seed(666)

###############################
### HELPER FUNCTION: Extract ATT and format for tables
###############################

extract_att_summary <- function(agg_results, outcome_name, spec_name, risk_level) {
  # Get overall ATT for post-treatment period
  post_att <- mean(agg_results$att.egt[agg_results$egt >= 0], na.rm = TRUE)
  post_se <- mean(agg_results$se.egt[agg_results$egt >= 0], na.rm = TRUE)
  
  # Calculate significance
  t_stat <- post_att / post_se
  p_val <- 2 * (1 - pnorm(abs(t_stat)))
  stars <- ifelse(p_val < 0.01, "***",
                  ifelse(p_val < 0.05, "**",
                         ifelse(p_val < 0.10, "*", "")))
  
  # Get sample info
  n_treated <- sum(!is.na(agg_results$att.egt))
  
  return(data.frame(
    outcome = outcome_name,
    risk_level = risk_level,
    specification = spec_name,
    att = post_att,
    se = post_se,
    stars = stars,
    n_periods = n_treated
  ))
}

###############################
### RUN ALL ROBUSTNESS CHECKS
###############################

# Initialize results storage
all_robustness_results <- list()

# Outcome variables and names
outcomes_low <- list(
  rep = "civilian_bd_onesided",
  mil = "battles",
  terr = "territorial_events_all",
  idp = "total_idps"
)

outcomes_high <- list(
  rep = "civilian_bd_onesided",
  mil = "battles",
  terr = "territorial_rebel_gains",
  idp = "total_idps"
)

outcome_labels <- c("Repression", "Military Activity", "Territorial Events", "Displacement")

###############################
### BASELINE (for reference)
###############################

cat("=== Running Baseline Models ===\n")

# Already run in your original code, but let's store them
baseline_results <- list(
  rep_low = agg_effects_rep_low,
  mil_low = agg_effects_mil_low,
  terr_low = agg_effects_terr_low,
  idp_low = agg_effects_idp_low,
  rep_high = agg_effects_rep_high,
  mil_high = agg_effects_mil_high,
  terr_high = agg_effects_terr_high,
  idp_high = agg_effects_idp_high
)

###############################
### ROBUSTNESS 1: UNIVERSAL MEAN THRESHOLD
###############################

cat("=== Robustness 1: Universal Mean Threshold ===\n")

THRESHOLD_UNIVERSAL <- mean(df$grid_risk_max, na.rm = TRUE)
cat("Universal threshold:", THRESHOLD_UNIVERSAL, "\n")

df_high_risk_univ <- df %>% filter(grid_risk_max > THRESHOLD_UNIVERSAL)
df_low_risk_univ <- df %>% filter(grid_risk_max <= THRESHOLD_UNIVERSAL)

rob1_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk_univ,
    panel = FALSE,
    control_group = "notyettreated",
    cores = 7
  )
  
  rob1_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk_univ,
    panel = FALSE,
    control_group = "notyettreated",
    cores = 7
  )
  
  rob1_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

###############################
### ROBUSTNESS 2: NEVER TREATED
###############################

cat("=== Robustness 2: Never Treated Control Group ===\n")

rob2_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk,
    panel = FALSE,
    control_group = "nevertreated",  # CHANGED
    cores = 7
  )
  
  rob2_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk,
    panel = FALSE,
    control_group = "nevertreated",  # CHANGED
    cores = 7
  )
  
  rob2_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

###############################
### ROBUSTNESS 3: MEDIAN THRESHOLD
###############################

cat("=== Robustness 3: Median Threshold ===\n")

df_high_risk_med <- df %>%
  group_by(country_name) %>%
  mutate(country_median_grid_risk = median(grid_risk_max, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(grid_risk_max > country_median_grid_risk)

df_low_risk_med <- df %>%
  group_by(country_name) %>%
  mutate(country_median_grid_risk = median(grid_risk_max, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(grid_risk_max <= country_median_grid_risk)

rob3_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk_med,
    panel = FALSE,
    control_group = "notyettreated",
    cores = 7
  )
  
  rob3_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk_med,
    panel = FALSE,
    control_group = "notyettreated",
    cores = 7
  )
  
  rob3_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

###############################
### ROBUSTNESS 4: IPW ESTIMATOR
###############################

cat("=== Robustness 4: IPW Estimator ===\n")

rob4_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk,
    panel = FALSE,
    control_group = "notyettreated",
    est_method = "ipw",  # CHANGED
    cores = 7
  )
  
  rob4_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk,
    panel = FALSE,
    control_group = "notyettreated",
    est_method = "ipw",  # CHANGED
    cores = 7
  )
  
  rob4_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

###############################
### ROBUSTNESS 5: DOUBLY ROBUST ESTIMATOR
###############################

cat("=== Robustness 5: Doubly Robust Estimator ===\n")

rob5_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk,
    panel = FALSE,
    control_group = "notyettreated",
    est_method = "dr",  # CHANGED
    cores = 7
  )
  
  rob5_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk,
    panel = FALSE,
    control_group = "notyettreated",
    est_method = "dr",  # CHANGED
    cores = 7
  )
  
  rob5_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

###############################
### ROBUSTNESS 6: ANTICIPATION EFFECTS
###############################

cat("=== Robustness 6: Anticipation Effects (2 months) ===\n")

rob6_results <- list()

# LOW RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_low)[i]
  outcome_var <- outcomes_low[[i]]
  
  cat("  Running:", outcome_labels[i], "(Low Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_low_risk,
    panel = FALSE,
    control_group = "notyettreated",
    anticipation = 2,  # ADDED
    cores = 7
  )
  
  rob6_results[[paste0(outcome_name, "_low")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

# HIGH RISK
for (i in 1:4) {
  outcome_name <- names(outcomes_high)[i]
  outcome_var <- outcomes_high[[i]]
  
  cat("  Running:", outcome_labels[i], "(High Risk)\n")
  
  did_result <- att_gt(
    yname = outcome_var,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = df_high_risk,
    panel = FALSE,
    control_group = "notyettreated",
    anticipation = 2,  # ADDED
    cores = 7
  )
  
  rob6_results[[paste0(outcome_name, "_high")]] <- aggte(did_result, type = "dynamic", na.rm = TRUE)
}

cat("\n=== All Robustness Checks Complete ===\n")

###############################
### CREATE LATEX TABLES
###############################

create_robustness_table <- function(outcome_idx, risk_level = "low") {
  
  outcome_name <- names(outcomes_low)[outcome_idx]
  outcome_label <- outcome_labels[outcome_idx]
  
  # Get baseline result
  baseline_key <- paste0(outcome_name, "_", risk_level)
  baseline <- baseline_results[[baseline_key]]
  
  # Get robustness results
  rob1 <- rob1_results[[baseline_key]]
  rob2 <- rob2_results[[baseline_key]]
  rob3 <- rob3_results[[baseline_key]]
  rob4 <- rob4_results[[baseline_key]]
  rob5 <- rob5_results[[baseline_key]]
  rob6 <- rob6_results[[baseline_key]]
  
  # Extract post-treatment ATTs
  extract_post_att <- function(agg_obj) {
    post_idx <- which(agg_obj$egt >= 0)
    att_post <- agg_obj$att.egt[post_idx]
    se_post <- agg_obj$se.egt[post_idx]
    
    # Calculate average post-treatment effect
    avg_att <- mean(att_post, na.rm = TRUE)
    avg_se <- sqrt(mean(se_post^2, na.rm = TRUE))  # Pool SEs
    
    t_stat <- avg_att / avg_se
    p_val <- 2 * (1 - pnorm(abs(t_stat)))
    
    stars <- ifelse(p_val < 0.01, "***",
                    ifelse(p_val < 0.05, "**",
                           ifelse(p_val < 0.10, "*", "")))
    
    return(list(att = avg_att, se = avg_se, stars = stars))
  }
  
  base_res <- extract_post_att(baseline)
  rob1_res <- extract_post_att(rob1)
  rob2_res <- extract_post_att(rob2)
  rob3_res <- extract_post_att(rob3)
  rob4_res <- extract_post_att(rob4)
  rob5_res <- extract_post_att(rob5)
  rob6_res <- extract_post_att(rob6)
  
  # Create table
  risk_label <- ifelse(risk_level == "low", "Low", "High")
  
  cat("\n")
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\caption{Robustness Checks: ", outcome_label, " (", risk_label, " Contestation)}\n")
  cat("\\label{tab:robust_", outcome_name, "_", risk_level, "}\n")
  cat("\\begin{tabular}{lccccccc}\n")
  cat("\\toprule\n")
  cat(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\\n")
  cat(" & Baseline & Universal & Never & Median & IPW & Doubly & Anticipation \\\\\n")
  cat(" &  & Mean & Treated & Threshold &  & Robust & (2 months) \\\\\n")
  cat("\\midrule\n")
  
  cat("Post-Treatment ATT & ",
      sprintf("%.3f%s", base_res$att, base_res$stars), " & ",
      sprintf("%.3f%s", rob1_res$att, rob1_res$stars), " & ",
      sprintf("%.3f%s", rob2_res$att, rob2_res$stars), " & ",
      sprintf("%.3f%s", rob3_res$att, rob3_res$stars), " & ",
      sprintf("%.3f%s", rob4_res$att, rob4_res$stars), " & ",
      sprintf("%.3f%s", rob5_res$att, rob5_res$stars), " & ",
      sprintf("%.3f%s", rob6_res$att, rob6_res$stars), " \\\\\n")
  
  cat(" & ",
      sprintf("(%.3f)", base_res$se), " & ",
      sprintf("(%.3f)", rob1_res$se), " & ",
      sprintf("(%.3f)", rob2_res$se), " & ",
      sprintf("(%.3f)", rob3_res$se), " & ",
      sprintf("(%.3f)", rob4_res$se), " & ",
      sprintf("(%.3f)", rob5_res$se), " & ",
      sprintf("(%.3f)", rob6_res$se), " \\\\\n")
  
  cat("\\midrule\n")
  cat("Control Group & Not Yet & Not Yet & Never & Not Yet & Not Yet & Not Yet & Not Yet \\\\\n")
  cat(" & Treated & Treated & Treated & Treated & Treated & Treated & Treated \\\\\n")
  cat("Threshold & Country & Universal & Country & Country & Country & Country & Country \\\\\n")
  cat(" & Mean & Mean & Mean & Median & Mean & Mean & Mean \\\\\n")
  cat("Estimator & DR & DR & DR & DR & IPW & DR & DR \\\\\n")
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\floatnote{Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.10. Post-Treatment ATT represents the average treatment effect across all post-treatment periods.}\n")
  cat("\\end{table}\n")
  cat("\n")
}

# Generate all 8 tables
cat("\n\n%%% ROBUSTNESS TABLES %%%\n\n")

# Low Risk Tables
for (i in 1:4) {
  create_robustness_table(i, "low")
}

# High Risk Tables
for (i in 1:4) {
  create_robustness_table(i, "high")
}

###############################
### SAVE RESULTS FOR FURTHER ANALYSIS
###############################

save(baseline_results, rob1_results, rob2_results, rob3_results, 
     rob4_results, rob5_results, rob6_results,
     file = "../data/robustness_results.RData")

cat("\n=== Results saved to robustness_results.RData ===\n")




###############################
### CREATE LATEX TABLES (WITHOUT ANTICIPATION)
###############################

create_robustness_table <- function(outcome_idx, risk_level = "low") {
  
  outcome_name <- names(outcomes_low)[outcome_idx]
  outcome_label <- outcome_labels[outcome_idx]
  
  # Get baseline result
  baseline_key <- paste0(outcome_name, "_", risk_level)
  baseline <- baseline_results[[baseline_key]]
  
  # Get robustness results (SKIP rob6)
  rob1 <- rob1_results[[baseline_key]]
  rob2 <- rob2_results[[baseline_key]]
  rob3 <- rob3_results[[baseline_key]]
  rob4 <- rob4_results[[baseline_key]]
  rob5 <- rob5_results[[baseline_key]]
  
  # Extract post-treatment ATTs
  extract_post_att <- function(agg_obj) {
    post_idx <- which(agg_obj$egt >= 0)
    att_post <- agg_obj$att.egt[post_idx]
    se_post <- agg_obj$se.egt[post_idx]
    
    # Calculate average post-treatment effect
    avg_att <- mean(att_post, na.rm = TRUE)
    avg_se <- sqrt(mean(se_post^2, na.rm = TRUE))  # Pool SEs
    
    t_stat <- avg_att / avg_se
    p_val <- 2 * (1 - pnorm(abs(t_stat)))
    
    stars <- ifelse(p_val < 0.01, "***",
                    ifelse(p_val < 0.05, "**",
                           ifelse(p_val < 0.10, "*", "")))
    
    return(list(att = avg_att, se = avg_se, stars = stars))
  }
  
  base_res <- extract_post_att(baseline)
  rob1_res <- extract_post_att(rob1)
  rob2_res <- extract_post_att(rob2)
  rob3_res <- extract_post_att(rob3)
  rob4_res <- extract_post_att(rob4)
  rob5_res <- extract_post_att(rob5)
  
  # Create table
  risk_label <- ifelse(risk_level == "low", "Low", "High")
  
  cat("\n")
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\caption{Robustness Checks: ", outcome_label, " (", risk_label, " Contestation)}\n")
  cat("\\label{tab:robust_", outcome_name, "_", risk_level, "}\n")
  cat("\\begin{tabular}{lcccccc}\n")
  cat("\\toprule\n")
  cat(" & (1) & (2) & (3) & (4) & (5) & (6) \\\\\n")
  cat(" & Baseline & Universal & Never & Median & IPW & Doubly \\\\\n")
  cat(" &  & Mean & Treated & Threshold &  & Robust \\\\\n")
  cat("\\midrule\n")
  
  cat("Post-Treatment ATT & ",
      sprintf("%.3f%s", base_res$att, base_res$stars), " & ",
      sprintf("%.3f%s", rob1_res$att, rob1_res$stars), " & ",
      sprintf("%.3f%s", rob2_res$att, rob2_res$stars), " & ",
      sprintf("%.3f%s", rob3_res$att, rob3_res$stars), " & ",
      sprintf("%.3f%s", rob4_res$att, rob4_res$stars), " & ",
      sprintf("%.3f%s", rob5_res$att, rob5_res$stars), " \\\\\n")
  
  cat(" & ",
      sprintf("(%.3f)", base_res$se), " & ",
      sprintf("(%.3f)", rob1_res$se), " & ",
      sprintf("(%.3f)", rob2_res$se), " & ",
      sprintf("(%.3f)", rob3_res$se), " & ",
      sprintf("(%.3f)", rob4_res$se), " & ",
      sprintf("(%.3f)", rob5_res$se), " \\\\\n")
  
  cat("\\midrule\n")
  cat("Control Group & Not Yet & Not Yet & Never & Not Yet & Not Yet & Not Yet \\\\\n")
  cat(" & Treated & Treated & Treated & Treated & Treated & Treated \\\\\n")
  cat("Threshold & Country & Universal & Country & Country & Country & Country \\\\\n")
  cat(" & Mean & Mean & Mean & Median & Mean & Mean \\\\\n")
  cat("Estimator & DR & DR & DR & DR & IPW & DR \\\\\n")
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\floatnote{Robust standard errors in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.10. Post-Treatment ATT represents the average treatment effect across all post-treatment periods.}\n")
  cat("\\end{table}\n")
  cat("\n")
}

# Generate all 8 tables
cat("\n\n%%% ROBUSTNESS TABLES %%%\n\n")

# Low Risk Tables
for (i in 1:4) {
  create_robustness_table(i, "low")
}

# High Risk Tables
for (i in 1:4) {
  create_robustness_table(i, "high")
}

