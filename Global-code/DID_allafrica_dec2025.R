# Clear environment
rm(list = setdiff(ls(), "ged"))
gc()

# EITM Admin 1-Level Merge Script (Country, ISO3C, Admin1, Month)
# Created by Elisa D'Amico — 12 December 2025
# Modified for high-risk vs low-risk conflict areas

# -----------------------------
# Load Libraries
# -----------------------------
library(readr)
library(fixest)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(did)

# -----------------------------
# Load and Filter Data
# -----------------------------
YEAR_RANGE <- c(2022, 2024)

df <- read_csv("../data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv") %>%
  mutate(across(c(battles, year_month_encoded, iso3c_adm1_encoded, disasters_all_treat),
                as.numeric)) %>%
  filter(year >= YEAR_RANGE[1], year <= YEAR_RANGE[2])

# # Remove countries with >95% zero battles (low conflict variation)
# countries_to_drop <- df %>%
#   group_by(country_name) %>%
#   summarise(pct_zero = sum(battles == 0, na.rm = TRUE) / n() * 100) %>%
#   filter(pct_zero > 99.0) %>%
#   pull(country_name)
# 
# cat("=== Filtering Low-Conflict Countries ===\n")
# cat("Countries dropped (>95% zeros):", length(countries_to_drop), "\n")
# print(countries_to_drop)
# 
# df <- df %>% filter(!country_name %in% countries_to_drop)
# 
# cat("\nObservations after filtering:", nrow(df), "\n")
# cat("Countries remaining:", n_distinct(df$country_name), "\n\n")

# -----------------------------
# Split by Grid Risk Threshold
# -----------------------------
THRESHOLD <- 0.5

df_high_risk <- df %>% filter(grid_risk_max > THRESHOLD)
df_low_risk <- df %>% filter(grid_risk_max <= THRESHOLD)

# Summary statistics
cat("=== Grid Risk Split (Threshold:", THRESHOLD, ") ===\n\n")

cat("HIGH RISK (grid_risk >", THRESHOLD, "):\n")
cat("  Total observations:", nrow(df_high_risk), "\n")
cat("  Treated observations:", sum(df_high_risk$disasters_all_treat != 0), "\n")
cat("  Mean battles:", round(mean(df_high_risk$battles, na.rm = TRUE), 2), "\n")
cat("  Mean civilian deaths:", round(mean(df_high_risk$civilian_bd_onesided, na.rm = TRUE), 2), "\n\n")

cat("LOW RISK (grid_risk <=", THRESHOLD, "):\n")
cat("  Total observations:", nrow(df_low_risk), "\n")
cat("  Treated observations:", sum(df_low_risk$disasters_all_treat != 0), "\n")
cat("  Mean battles:", round(mean(df_low_risk$battles, na.rm = TRUE), 2), "\n")
cat("  Mean civilian deaths:", round(mean(df_low_risk$civilian_bd_onesided, na.rm = TRUE), 2), "\n\n")

conflict_ratio <- mean(df_high_risk$battles, na.rm = TRUE) / mean(df_low_risk$battles, na.rm = TRUE)
cat("Conflict ratio (High/Low):", round(conflict_ratio, 2), "\n")
cat("(Target: > 1.5 for meaningful split)\n\n")

# Set seed for reproducibility
set.seed(666)

###############################
### Run Models - HIGH RISK
###############################

cat("=== Running DiD Models: HIGH RISK ===\n")

# HIGH RISK - Repression
did_results_rep_high <- att_gt(
  yname = "civilian_bd_onesided",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_rep_high <- aggte(did_results_rep_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_rep_high)

# HIGH RISK - Military
did_results_mil_high <- att_gt(
  yname = "battles",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_mil_high <- aggte(did_results_mil_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_mil_high)

# HIGH RISK - IDPs
did_results_idp_high <- att_gt(
  yname = "total_idps",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_idp_high <- aggte(did_results_idp_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_idp_high)

###############################
### Run Models - LOW RISK
###############################

cat("=== Running DiD Models: LOW RISK ===\n")

# LOW RISK - Repression
did_results_rep_low <- att_gt(
  yname = "civilian_bd_onesided",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_rep_low <- aggte(did_results_rep_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_rep_low)

# LOW RISK - Military
did_results_mil_low <- att_gt(
  yname = "battles",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_mil_low <- aggte(did_results_mil_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_mil_low)

# LOW RISK - IDPs
did_results_idp_low <- att_gt(
  yname = "total_idps",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_idp_low <- aggte(did_results_idp_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_idp_low)

###############################
### Create Plots
###############################

# Repression - High Risk
plot_rep_high <- ggdid(agg_effects_rep_high) + 
  labs(
    title = paste0("Repression (High Risk: Grid Risk > ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_rep_high)

# Repression - Low Risk
plot_rep_low <- ggdid(agg_effects_rep_low) + 
  labs(
    title = paste0("Repression (Low Risk: Grid Risk ≤ ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_rep_low)

# Military - High Risk
plot_mil_high <- ggdid(agg_effects_mil_high) + 
  labs(
    title = paste0("Military Effort (High Risk: Grid Risk > ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_mil_high)

# Military - Low Risk
plot_mil_low <- ggdid(agg_effects_mil_low) + 
  labs(
    title = paste0("Military Effort (Low Risk: Grid Risk ≤ ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_mil_low)

# IDPs - High Risk
plot_idp_high <- ggdid(agg_effects_idp_high) + 
  labs(
    title = paste0("Displacement (High Risk: Grid Risk > ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_idp_high)

# IDPs - Low Risk
plot_idp_low <- ggdid(agg_effects_idp_low) + 
  labs(
    title = paste0("Displacement (Low Risk: Grid Risk ≤ ", THRESHOLD, ")"),
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, 
                               family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
print(plot_idp_low)

cat("\n=== Analysis Complete ===\n")


# HIGH RISK - 3 plots stacked vertically
grid_high <- grid.arrange(
  plot_rep_high + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_mil_high + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_idp_high + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  ncol = 1,
  top = grid::textGrob(paste0("High Risk Areas (Grid Risk > ", THRESHOLD, ")"),
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)

# LOW RISK - 3 plots stacked vertically
grid_low <- grid.arrange(
  plot_rep_low + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_mil_low + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_idp_low + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  ncol = 1,
  top = grid::textGrob(paste0("Low Risk Areas (Grid Risk ≤ ", THRESHOLD, ")"),
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)

