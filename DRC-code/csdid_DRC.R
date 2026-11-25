# Clear environment
rm(list = ls())
gc()

# EITM Admin 1-Level Merge Script (Country, ISO3C, Admin1, Year)
# Created by Elisa D’Amico — 9 August 2025

# Update log:
# 2025-12-11 NH
# 2025-12-30 NH
# 2026-01-03 NH 


# -----------------------------
# Load Libraries
# -----------------------------
library(readr)
library(fixest)
library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(did)
library(gtsummary)
library(tidyverse)

# Load data
#df <- read_csv("DRC_final_data.csv") 

#df <- read_csv("~/Documents/Academic Work/Post-Doc/Summer 2025/EITM/Project/DRC_final_data.csv")
df <- read_rds("../data/merged_data_2026-01-03.Rds")
###############################
### Filtering to matched sample 
###############################
#matches <- read_csv("data/matches.csv") %>% distinct()
#df <- df %>%semi_join(matches, by = c("iso3c", "adm1_name", "year", "month"))

colnames(df)

# Set seed
set.seed(666)


# For the data with risk, we can only take data from 2023 onward
# 2023-01 is month number 49.

df <- df %>%
  st_drop_geometry() %>% 
  mutate(treated_binary = ifelse(disaster_treat > 0, 1, 0)) %>% 
  group_by(name) %>%
  mutate(group = case_when(
    any(aid_unit == 0 | is.na(aid_unit), na.rm = TRUE) ~ "no_aid",
    any( aid_unit >= 1, na.rm = TRUE) ~ "aid",
    TRUE ~ NA_character_
  ),
  risk_above_50 = ifelse(risk_mean > 0.5, 1, 0),
  risk_above_avr = ifelse(risk_mean > 0.394, 1, 0)) %>%
  ungroup()


# split into samples
no_aid <- df %>% filter(group == "no_aid")
aid    <- df %>% filter(group == "aid")

# Subset data to after 2023

#omit_month <- str_c("2022-", c("01", "02", "03", "04", "05", "06"))

df_subset <- df %>% 
  #filter(year > 2021, 
  #       !(year_month %in% omit_month)) %>% 
  group_by(name) %>% 
  mutate(month_id = row_number())

first_month_treated <- df_subset %>% 
  st_drop_geometry() %>% 
  dplyr::select(-c(aid_unit:no_homeless)) %>% 
  filter(disaster_bin == 1) %>% 
  group_by(name) %>% 
  arrange(row_number()) %>% 
  slice(1) %>% 
  mutate(disaster_treated_23 = month_id) %>% 
  select(name, disaster_treated_23)

# Merge back 

df_subset <- left_join(df_subset, first_month_treated, by = "name") %>% 
  mutate(disaster_treated_23 = replace_na(disaster_treated_23, 0))

avr_risk <- mean(df_subset$risk_mean[df_subset$year == 2022])

contested <- df_subset %>% 
  filter(year == 2022) |> 
  group_by(name) %>% 
  filter(any(risk_mean > avr_risk)) %>% 
  pull(name) %>% 
  unique(.)

df_subset_contested <- df_subset %>% 
  filter(name %in% contested)

df_subset_uncontested <- df_subset %>% 
  filter(!(name %in% contested))

###############################
### Descriptive Stats --------
###############################

desc_df <- df %>% 
  dplyr::select(aid_unit:distinct_destination, risk_mean, disaster_treat, group, treated_binary)

summary(desc_df)
# make sure variables are in correct format 
# note that we only have conflict risk for 2022-2024
desc_df %>% 
  tbl_summary(by = treated_binary)

df_subset_contested %>% dplyr::select(aid_unit:distinct_destination, 
                                risk_mean, disaster_treat, group, 
                                treated_binary) %>% 
  tbl_summary(by = treated_binary)

df_subset_uncontested %>% dplyr::select(aid_unit:distinct_destination, 
                                      risk_mean, disaster_treat, group, 
                                      treated_binary) %>% 
  tbl_summary(by = treated_binary)

# Should plot out contestation on map

###############################
### Run Models
###############################

# Run DiD estimation with wild bootstrap and not-yet-treated as controls
did_results_rep <- att_gt(
  yname = "civilian_bd_onesided",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "year_month_encoded",
  idname = "name_encoded",
  gname = "disaster_treat", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = aid,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

# Aggregate event study
agg_effects_rep <- aggte(did_results_rep, type = "dynamic", na.rm = TRUE)
summary(agg_effects_rep)


# Run DiD estimation with wild bootstrap and not-yet-treated as controls
did_results_mil <- att_gt(
  yname = "battles",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "year_month_encoded",
  idname = "name_encoded",
  gname = "disaster_treat", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = aid,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_mil <- aggte(did_results_mil, type = "dynamic", na.rm = TRUE)
summary(agg_effects_mil)

# Run DiD estimation with wild bootstrap and not-yet-treated as controls
did_results_idp <- att_gt(
  yname = "total_displaced_person",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "year_month_encoded",
  idname = "name_encoded",
  gname = "disaster_treat", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = aid,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_idp <- aggte(did_results_idp, type = "dynamic", na.rm = TRUE)
summary(agg_effects_idp)


# # Run DiD estimation with wild bootstrap and not-yet-treated as controls
# did_results_aid_unit <- att_gt(
#   yname = "aid_unit",
#   #total_displaced_person
#   #civilian_bd_onesided
#   #battles
#   tname = "year_month_encoded",
#   idname = "name_encoded",
#   gname = "disaster_treat", 
#   # disasters_w_damage_treat 
#   # disasters_w_reconstruction_treat 
#   # disasters_w_damage_treat
#   # disasters_flood_treat
#   # disasters_earthquake_treat
#   #xformla =~ civilian_bd_onesided, # REPRESSION
#   #xformla =~ battles, # MILITARY EFFORT
#   #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
#   data = df,
#   panel = FALSE, 
#   control_group = "notyettreated",   # use not yet treated as control group
#   #weightsname = "ag_grp_pc_usd",
#   #bstrap = TRUE,                     # bootstrap inference
#   #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
#   cores = 7
# )
# # Aggregate event study
# agg_effects_aid_unit <- aggte(did_results_aid_unit, type = "dynamic", na.rm = TRUE)
# summary(agg_effects_aid_unit)

######### Split by contested vs. noncontested ----

# Staggered DiD. Outcome: Repression
did_results_rep <- att_gt(
  yname = "civilian_bd_onesided",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

# Aggregate event study
summary(did_results_rep)
agg_effects_rep <- aggte(did_results_rep, type = "dynamic", na.rm = TRUE, 
                         min_e = -12, max_e = 12)
summary(agg_effects_rep)
# After recoding, no more increase in repression immediately

# Outcome: Fighting
did_results_mil <- att_gt(
  yname = "battles",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_mil <- aggte(did_results_mil, type = "dynamic", na.rm = TRUE, 
                         min_e = -12, max_e = 12)
summary(agg_effects_mil)
# Increase in fighting

# outcome: Displacement

did_results_idp <- att_gt(
  yname = "total_displaced_person",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "year_month_encoded",
  idname = "name_encoded",
  gname = "disaster_treat", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_idp <- aggte(did_results_idp, type = "dynamic", na.rm = TRUE, 
                         min_e = -12, max_e = 12)
summary(agg_effects_idp)
# Decrease in displacement

# Staggered DiD. Outcome: Repression
did_results_rep_uc <- att_gt(
  yname = "civilian_bd_onesided",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

# Aggregate event study
agg_effects_rep_uc <- aggte(did_results_rep_uc, type = "dynamic", na.rm = TRUE, 
                            min_e = -12, max_e = 12)
summary(agg_effects_rep_uc)
# In uncontested, no effect on repression

# Outcome: Fighting
did_results_mil_uc <- att_gt(
  yname = "battles",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_mil_uc <- aggte(did_results_mil_uc, type = "dynamic", na.rm = TRUE,
                            min_e = -12, max_e = 12)
summary(agg_effects_mil_uc)
# New result: decreased fighting

# outcome: Displacement

did_results_idp_uc <- att_gt(
  yname = "total_displaced_person",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "year_month_encoded",
  idname = "name_encoded",
  gname = "disaster_treat", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)
# Aggregate event study
agg_effects_idp_uc <- aggte(did_results_idp_uc, type = "dynamic", na.rm = TRUE, 
                            min_e = -12, max_e = 12)
summary(agg_effects_idp_uc)
# No effect on displacement at the >0.5 threshold
# When adjusted for 0.394 (the mean), there's a positive effect (not 
# overlapping 0)

###############################
### Final plots
###############################

#########
plot_rep <- ggdid(agg_effects_rep) + 
  labs(
    #title = "",
    title = "Disaster -> Repression (DRC) in contested areas",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

print(plot_rep)
ggsave(paste0("../output/figures/", today(), "_rep.png"), 
       plot_rep)

plot_mil <- ggdid(agg_effects_mil) + 
  labs(
    # title = "",
    title = "Disaster -> Military Operations (DRC) in contested area",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

print(plot_mil)
ggsave(paste0("../output/figures/", today(), "_mil.png"), 
       plot_mil)

plot_idp <- ggdid(agg_effects_idp) + 
  labs(
    #title = "",
    title = "Disaster -> IDPs (DRC) in contested areas",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

options(scipen = 999)
print(plot_idp)
ggsave(paste0("../output/figures/", today(), "_idp.png"), 
       plot_idp)

# Plots for uncontested areas

plot_rep_uc <- ggdid(agg_effects_rep_uc) + 
  labs(
    #title = "",
    title = "Disaster -> Repression (DRC) in uncontested areas",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

print(plot_rep_uc)
ggsave(paste0("../output/figures/", today(), "_rep_uc.png"), 
       plot_rep_uc)

plot_mil_uc <- ggdid(agg_effects_mil_uc) + 
  labs(
    # title = "",
    title = "Disaster -> Military Operations (DRC) in uncontested area",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

print(plot_mil_uc)
ggsave(paste0("../output/figures/", today(), "_mil_uc.png"), 
       plot_mil_uc)

plot_idp_uc <- ggdid(agg_effects_idp_uc) + 
  labs(
    #title = "",
    title = "Disaster -> IDPs (DRC) in uncontested areas",
    x = "Time to Treatment (Months)",
    y = "ATTs"
  ) + 
  scale_color_manual(
    values = c("#F08080", "#8B008B"), 
    labels = c("Pre-Treatment", "Post-Treatment")
  ) + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

print(plot_idp_uc)
ggsave(paste0("../output/figures/", today(), "_idp_uc.png"), 
       plot_idp_uc)

# Adding results for territorial takeover events ----

did_results_takeover <- att_gt(
  yname = "no_takeover_any",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover <- aggte(did_results_takeover, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover)


# Adding results for territorial takeover events ----
did_results_takeover_any <- att_gt(
  yname = "no_takeover_any",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  #xformla = 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_any <- aggte(did_results_takeover_any, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_any)


did_results_takeover_govt <- att_gt(
  yname = "no_takeover_govt",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  #xformla = 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_govt <- aggte(did_results_takeover_govt, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_govt)

did_results_takeover_m23 <- att_gt(
  yname = "no_takeover_m23",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_m23 <- aggte(did_results_takeover_m23, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_m23)

did_results_takeover_nonvio <- att_gt(
  yname = "no_takeover_peaceful",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_contested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_nonvio <- aggte(did_results_takeover_nonvio, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_nonvio)

# Takeover in noncontested areas ----
# We only have a few territorial takeover in these areas. 

did_results_takeover_uc <- att_gt(
  yname = "no_takeover_any",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  # disasters_w_damage_treat 
  # disasters_w_reconstruction_treat 
  # disasters_w_damage_treat
  # disasters_flood_treat
  # disasters_earthquake_treat
  #xformla =~ civilian_bd_onesided, # REPRESSION
  #xformla =~ battles, # MILITARY EFFORT
  #xformla =~ grp_pc_usd, # WAGES or ppp, etc. - won't run
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_uc <- aggte(did_results_takeover_uc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_uc)


# Adding results for territorial takeover events ----

did_results_takeover_govt_uc <- att_gt(
  yname = "no_takeover_govt",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_govt_uc <- aggte(did_results_takeover_govt_uc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_govt_uc)

did_results_takeover_m23_uc <- att_gt(
  yname = "no_takeover_m23",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_m23_uc <- aggte(did_results_takeover_m23_uc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_m23_uc)

did_results_takeover_nonvio_uc <- att_gt(
  yname = "no_takeover_peaceful",
  #total_displaced_person
  #civilian_bd_onesided
  #battles
  tname = "month_id",
  idname = "name_encoded",
  gname = "disaster_treated_23", 
  data = df_subset_uncontested,
  panel = FALSE, 
  control_group = "notyettreated",   # use not yet treated as control group
  #weightsname = "ag_grp_pc_usd",
  #bstrap = TRUE,                     # bootstrap inference
  #biters = 1000,                    # number of bootstrap iterations, adjust as needed)
  cores = 7
)

agg_effects_takeover_nonvio_uc <- aggte(did_results_takeover_nonvio_uc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_takeover_nonvio_uc)
