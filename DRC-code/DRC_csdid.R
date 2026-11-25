# Clear environment
rm(list = ls())
gc()

# EITM Admin 1-Level Merge Script (Country, ISO3C, Admin1, Year)
# Created by Elisa D’Amico — 9 August 2025

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

# Load data
df <- read_csv("../data/DRC_final_data.csv") 

###############################
### Filtering to matched sample 
###############################
#matches <- read_csv("data/matches.csv") %>% distinct()
#df <- df %>%semi_join(matches, by = c("iso3c", "adm1_name", "year", "month"))

colnames(df)

# Set seed
set.seed(666)

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
  data = df,
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
  data = df,
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
  data = df,
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



###############################
### Final plots
###############################

#########
plot <- ggdid(agg_effects_rep) + 
  labs(
    #title = "",
    title = "Disaster -> Repression (DRC)",
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

print(plot)

plot <- ggdid(agg_effects_mil) + 
  labs(
    # title = "",
    title = "Disaster -> Military Operations (DRC)",
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

print(plot)


plot <- ggdid(agg_effects_idp) + 
  labs(
    #title = "",
    title = "Disaster -> IDPs (DRC)",
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

print(plot)
