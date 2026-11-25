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
library(gridExtra)

# -----------------------------
# Load and Filter Data
# -----------------------------
YEAR_RANGE <- c(2022, 2024)

df <- read_csv("../data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv") %>%
  mutate(across(c(battles, year_month_encoded, iso3c_adm1_encoded, disasters_all_treat),
                as.numeric)) %>%
  filter(year >= YEAR_RANGE[1], year <= YEAR_RANGE[2])


THRESHOLD_MAX <- mean(df$grid_risk_max, na.rm = TRUE)
THRESHOLD_MEAN <- mean(df$grid_risk_mean, na.rm = TRUE)
THRESHOLD_MIN <- mean(df$grid_risk_min, na.rm = TRUE)

cat("=== Grid Risk Thresholds ===\n")
cat("Mean grid_risk_max:", round(THRESHOLD_MAX, 4), "\n")
cat("Mean grid_risk_mean:", round(THRESHOLD_MEAN, 4), "\n")
cat("Mean grid_risk_min:", round(THRESHOLD_MIN, 4), "\n\n")


THRESHOLD <- "Mean per country" #0.257 #(mean of max)
# 
# df_high_risk <- df %>% filter(grid_risk_max > THRESHOLD)
# df_low_risk <- df %>% filter(grid_risk_max <= THRESHOLD)


df <- df %>%
  group_by(country_name) %>%
  mutate(
    country_mean_grid_risk = mean(grid_risk_max, na.rm = TRUE)
  ) %>%
  ungroup()
df_high_risk <- df %>%
  filter(grid_risk_max > country_mean_grid_risk)

df_low_risk <- df %>%
  filter(grid_risk_max <= country_mean_grid_risk)



# # Summary statistics
# cat("=== Grid Risk Split (Threshold:", THRESHOLD, ") ===\n\n")
# 
# cat("HIGH RISK (grid_risk >", THRESHOLD, "):\n")
# cat("  Total observations:", nrow(df_high_risk), "\n")
# cat("  Treated observations:", sum(df_high_risk$disasters_all_treat != 0), "\n")
# cat("  Mean battles:", round(mean(df_high_risk$battles, na.rm = TRUE), 2), "\n")
# cat("  Mean civilian deaths:", round(mean(df_high_risk$civilian_bd_onesided, na.rm = TRUE), 2), "\n\n")
# 
# cat("LOW RISK (grid_risk <=", THRESHOLD, "):\n")
# cat("  Total observations:", nrow(df_low_risk), "\n")
# cat("  Treated observations:", sum(df_low_risk$disasters_all_treat != 0), "\n")
# cat("  Mean battles:", round(mean(df_low_risk$battles, na.rm = TRUE), 2), "\n")
# cat("  Mean civilian deaths:", round(mean(df_low_risk$civilian_bd_onesided, na.rm = TRUE), 2), "\n\n")
# 
conflict_ratio <- mean(df_high_risk$battles, na.rm = TRUE) / mean(df_low_risk$battles, na.rm = TRUE)
cat("Conflict ratio (High/Low):", round(conflict_ratio, 2), "\n")
cat("(Target: > 1.5 for meaningful split)\n\n")

# Set seed for reproducibility
set.seed(666)




###############################
### Run Models - HIGH RISK (Including Territorial Rebel Gains)
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

# HIGH RISK - Territorial Rebel Gains (NEW)
did_results_terr_high <- att_gt(
  yname = "territorial_rebel_gains",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_terr_high <- aggte(did_results_terr_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_terr_high)

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
### Run Models - LOW RISK (Including Territorial Rebel Gains)
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

# LOW RISK - Territorial Rebel Gains (NEW)
did_results_terr_low <- att_gt(
  yname = "territorial_rebel_gains",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_terr_low <- aggte(did_results_terr_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_terr_low)

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
### Create Plots (Including Territorial Rebel Gains)
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
  top = grid::textGrob(paste0("High Territorial Claim"),
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)


# Territorial Rebel Gains - High Risk
plot_terr_high <- ggdid(agg_effects_terr_high) + 
  labs(
    title = paste0("Territorial Rebel Gains (High Risk: Grid Risk > ", THRESHOLD, ")"),
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
print(plot_terr_high)

# Territorial Rebel Gains - Low Risk
plot_terr_low <- ggdid(agg_effects_terr_low) + 
  labs(
    title = paste0("Territorial Rebel Gains (Low Risk: Grid Risk ≤ ", THRESHOLD, ")"),
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
print(plot_terr_low)

# Update grids to include territorial gains (4 plots each)
# HIGH RISK - 4 plots stacked vertically
grid_high <- grid.arrange(
  plot_rep_high + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_mil_high + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_terr_high + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_idp_high + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  ncol = 1,
  top = grid::textGrob(paste0("High Territorial Claim"),
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)

# LOW RISK - 4 plots stacked vertically
grid_low <- grid.arrange(
  plot_rep_low + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_mil_low + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_terr_low + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm")),
  plot_idp_low + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  ncol = 1,
  top = grid::textGrob(paste0("Low Territorial Claim"),
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)

cat("\n=== Analysis Complete ===\n")

# Print only the territorial rebel gains plots
print(plot_terr_low)
summary(agg_effects_terr_low)
print(plot_terr_high)
summary(agg_effects_terr_high)

# Or if you want them side by side:
grid.arrange(
  plot_terr_low,
  plot_terr_high,
  nrow = 2,
  top = grid::textGrob("Territorial Rebel Gains by Level of Contestation",
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)



###############################
### Run Models - Territorial Events All
###############################

cat("=== Running DiD Models: TERRITORIAL EVENTS ALL ===\n")

# HIGH RISK - Territorial Events All
did_results_terr_all_high <- att_gt(
  yname = "territorial_events_all",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_terr_all_high <- aggte(did_results_terr_all_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_terr_all_high)

# LOW RISK - Territorial Events All
did_results_terr_all_low <- att_gt(
  yname = "territorial_events_all",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_terr_all_low <- aggte(did_results_terr_all_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_terr_all_low)

###############################
### Create Plots
###############################

# Territorial Events All - High Risk
plot_terr_all_high <- ggdid(agg_effects_terr_all_high) + 
  labs(
    title = paste0("Contested"),
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

# Territorial Events All - Low Risk
plot_terr_all_low <- ggdid(agg_effects_terr_all_low) + 
  labs(
    title = paste0("Uncontested"),
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

# Print plots
print(plot_terr_all_high)
print(plot_terr_all_low)

# Or side by side:
grid.arrange(
  plot_terr_all_low,
  plot_terr_all_high,
  nrow = 2,
  top = grid::textGrob("All Territorial Events by Level of Contestation",
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)


###############################
### Run Models - Territorial Government Regains
###############################

cat("=== Running DiD Models: TERRITORIAL GOVERNMENT REGAINS ===\n")

# HIGH RISK - Territorial Government Regains
did_results_govt_regains_high <- att_gt(
  yname = "territorial_govt_regains",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_high_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_govt_regains_high <- aggte(did_results_govt_regains_high, type = "dynamic", na.rm = TRUE)
summary(agg_effects_govt_regains_high)

# LOW RISK - Territorial Government Regains
did_results_govt_regains_low <- att_gt(
  yname = "territorial_govt_regains",
  tname = "year_month_encoded",
  idname = "iso3c_adm1_encoded",
  gname = "disasters_all_treat", 
  data = df_low_risk,
  panel = FALSE, 
  control_group = "notyettreated",
  cores = 7
)
agg_effects_govt_regains_low <- aggte(did_results_govt_regains_low, type = "dynamic", na.rm = TRUE)
summary(agg_effects_govt_regains_low)

###############################
### Create Plots
###############################

# Territorial Government Regains - High Risk
plot_govt_regains_high <- ggdid(agg_effects_govt_regains_high) + 
  labs(
    title = "High Contestation",
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

# Territorial Government Regains - Low Risk
plot_govt_regains_low <- ggdid(agg_effects_govt_regains_low) + 
  labs(
    title = "Low Contestation",
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

# Print in 2 rows
grid.arrange(
  plot_govt_regains_low,
  plot_govt_regains_high,
  nrow = 2,
  top = grid::textGrob("Government Territorial Regains by Levels of Contestation",
                       gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman"))
)
