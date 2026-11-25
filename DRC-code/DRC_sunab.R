# =========================================================
# EITM Admin 1-Level Merge Script
# Country, ISO3C, Admin1, Month
# Created by Elisa D'Amico — 6 August 2025
# Modified for monthly climate and conflict data
# =========================================================

# -----------------------------
# Load Libraries
# -----------------------------
library(fixest)
library(countrycode)
library(dplyr)
library(furrr)
library(data.table)
library(janitor)
library(jsonlite)
library(purrr)
library(readr)
library(readxl)
library(rnaturalearth)
library(sf)
library(stringr)
library(tidyr)
library(tidyverse)
library(rlang)
library(zoo)
library(lubridate)
library(tibble)

# -----------------------------
# Load & Prepare Data
# -----------------------------
df_month <- read_csv("../data/DRC_final_data.csv") %>%
  mutate(
    battles = as.numeric(battles),
    year_month_encoded = as.numeric(year_month_encoded),
    name_encoded = as.numeric(name_encoded),
    disaster_treat = as.numeric(disaster_treat)
  )

## FILTER TO ONLY EASTERN ##
df_month <- df_month %>%
  filter(total_battles > 12)

# -----------------------------
# Identify First Disaster Month per Unit
# -----------------------------
subset_df_month <- df_month %>%
  select(name_encoded, year_month_encoded, disaster_treat) %>%
  filter(disaster_treat != 0) %>%
  arrange(name_encoded, year_month_encoded) %>%
  group_by(name_encoded) %>%
  slice(1) %>%
  mutate(firstdisas_binary = 1)

df_month <- left_join(
  df_month,
  subset_df_month %>% select(-disaster_treat),
  by = c("name_encoded", "year_month_encoded")
) %>%
  mutate(firstdisas_binary = replace_na(firstdisas_binary, 0))

# -----------------------------
# Treatment & Time to Treatment
# -----------------------------
setDT(df_month)
df_month[, treat := ifelse(!is.na(disaster_treat) & disaster_treat != 0, 1, 0)]
df_month[, time_to_treat := ifelse(treat == 1, year_month_encoded - disaster_treat, 0)]
df_month[, never_treated := ifelse(disaster_treat == 0, 1, 0)]

# -----------------------------
# Coefficients for Mediators
# -----------------------------

# Create dataframe for military battles results
battles_predval <- data.frame(
  time_to_treat = c(-61, -60, -59, -58, -57, -56, -55, -54, -53, -52, -51, -50, -49, -48, -47, -46, -45, -44, -43, -42, -41, -40, -39, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69),
  battles_predval = c(0.0105, 0.0794, 0.3243, -0.7462, -0.0030, -0.0886, 0.0044, 0.0000, 0.7894, -0.7546, 0.0035, -0.1799, 0.1248, -0.1944, 0.2522, 0.1457, -0.2086, 0.1009, -0.0363, 0.0581, -0.2256, 0.1502, 0.3205, 0.0687, -0.3183, 0.2630, 0.0469, 0.0747, -0.0967, -0.0669, 0.0140, 0.0835, -0.6680, 0.4287, -0.4203, 0.6479, -0.4828, -0.1794, 0.6194, -0.5210, 0.1795, -0.0244, -0.0116, -0.2751, 0.1858, -0.2656, 0.3477, -0.2616, -0.0676, 0.0280, 0.0859, 0.0179, -0.1573, 0.1959, -0.0376, 0.0847, -0.1490, 0.1111, -0.0363, 0.0066, 0.0850, 0.0994, -0.0304, 0.0099, 0.1522, 0.1096, -0.1205, -0.0471, 0.0014, 0.1463, 0.1632, 0.6279, 0.3080, 0.5128, 0.1127, -0.0226, 0.5164, 0.2676, 0.5051, 0.2435, -0.0121, 0.5550, 0.6215, 0.2204, -0.0278, -0.1340, 0.2202, -0.0303, -0.1864, 0.1355, -0.1005, -0.1081, 0.0580, -0.1515, 0.0047, -0.1719, -0.2649, 0.3143, -0.4193, -0.3401, -0.1992, -0.3524, -0.3423, -0.1889, -0.2081, -0.2861, -0.2487, -0.2586, 0.0523, 0.3605, 0.0531, 0.0218, 0.1596, 0.0851, -0.0361, -0.1715, -0.0805, -0.1863, -0.0253, -0.1745, -0.1227, 0.2485, 0.2031, -0.2123, 0.0440, -0.1702, -0.1652, -0.2143, 1.5635, 0.4286, -0.2679)
)

# Create dataframe for repression results  
repression_predval <- data.frame(
  time_to_treat = c(-61, -60, -59, -58, -57, -56, -55, -54, -53, -52, -51, -50, -49, -48, -47, -46, -45, -44, -43, -42, -41, -40, -39, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69),
  repression_predval = c(0.0789, 0.1323, 0.3274, -0.2092, -0.0274, -0.1893, 0.0545, -0.0677, 0.3158, -0.4279, 0.1193, -0.1857, -0.1981, 0.0306, -0.1462, 0.5041, -0.3510, 0.0284, -0.0567, -0.0828, -0.1744, 0.5630, -0.5619, 0.4006, -0.2236, 0.6863, -0.5335, 0.0311, -0.1660, 0.1503, 0.1369, -0.2102, -0.0434, -0.5545, -0.1160, 0.1023, -0.2926, 0.0606, 0.1771, -0.2159, 0.2366, -0.3888, 0.0432, 0.1749, -0.1930, 0.5007, -0.3785, -0.4268, -0.0105, -0.0452, 0.1249, 0.1360, -0.1863, 0.4818, -0.1547, 0.6369, -0.7389, -0.0812, 0.1799, 0.5273, -0.6503, -0.3818, -0.3555, 0.1673, -0.1903, 0.2375, -0.5071, -0.0467, -0.1533, -0.5149, -0.5632, 0.3736, 0.0619, 1.4206, -1.0019, -0.0441, 2.1794, 0.3045, 0.4140, 0.3308, 0.3934, 10.2223, 0.5639, 1.1366, 0.2463, -0.4190, 5.2234, -0.8310, -0.7752, 1.4213, 1.0342, -1.4512, 1.6489, -0.6018, 0.6971, -0.0220, -1.6131, 2.3973, -1.0540, -1.0306, -0.5705, -0.8795, -1.0358, -0.8433, -1.0350, -0.8943, -1.4327, -1.5246, -0.2724, 2.3163, -0.2236, -0.3665, -0.9511, 4.8683, -1.2004, -0.5445, -1.8167, -1.0301, -1.5865, -2.3186, -0.5686, -0.6808, -0.6310, -2.2272, -1.1095, -0.7000, -0.8586, -0.4449, 2.2976, -0.8671, -1.0476)
)








# First, make sure you have the time_to_treat_vals defined
# (I assume this should be the range you want, like -61 to 69 based on your data)
time_to_treat_vals <- -61:69  # or whatever range you need

# Create the coefficients dataframe by merging the predval dataframes
df_coefs <- tibble(time_to_treat = time_to_treat_vals) %>%
  left_join(battles_predval, by = "time_to_treat") %>%
  left_join(repression_predval, by = "time_to_treat") 
# Then merge onto your main dataframe
df_month <- df_month %>%
  left_join(df_coefs, by = "time_to_treat")



df_month <- df_month %>%
  group_by(name) %>%
  mutate(group = case_when(
    any(time_to_treat < 0 & (aid_unit == 0 | is.na(aid_unit)), na.rm = TRUE) ~ "no_aid",
    any(time_to_treat < 0 & aid_unit >= 1, na.rm = TRUE) ~ "aid",
    TRUE ~ NA_character_
  )) %>%
  ungroup()

# split into samples
no_aid <- df_month %>% filter(group == "no_aid")
aid    <- df_month %>% filter(group == "aid")


# -----------------------------
# Mediation Models
# -----------------------------

# mining_count, has_mining, mining_disasters, mining_hit_by_disaster


model_mediation <- feols(
  total_displaced_person ~ sunab(disaster_treat, year_month_encoded) + battles + repression_predval  + mining_hit_by_disaster| #+ ag_grp_pc_usd
    name_encoded + year_month_encoded,
  data = no_aid,
  cluster = ~name_encoded
)
summary(model_mediation)

iplot(
  model_mediation, sep = 0.2,
  main = "Disaster Impact on IDPs in Neighboring Units (Monthly) - Repression",
  xlab = "Event Time (Months)", ylab = "Effect on Population Change"
)



model_mediation <- feols(
  total_displaced_person ~ sunab(disaster_treat, year_month_encoded) + battles_predval  | #+ ag_grp_pc_usd
    name_encoded + year_month_encoded,
  data = no_aid,
  cluster = ~name_encoded
)
summary(model_mediation)

iplot(
  model_mediation, sep = 0.2,
  main = "Disaster Impact on IDPs in Neighboring Units (Monthly) - Military Operations",
  xlab = "Event Time (Months)", ylab = "Effect on Population Change"
)

##################
### POTENTIALLY SEE IF AID MODERATES????
##################

# model_mediation <- feols(
#   total_displaced_person ~ sunab(disaster_treat, year_month_encoded) + aid_unit_predval |
#     name_encoded + year_month_encoded,
#   data = no_aid,
#   cluster = ~name_encoded
# )
# summary(model_mediation)
# 
# iplot(
#   model_mediation, sep = 0.2,
#   main = "Disaster Impact on IDPs in Neighboring Units (Monthly) - Aid",
#   xlab = "Event Time (Months)", ylab = "Effect on Population Change"
# )

# 
# # -----------------------------
# # Sun & Abraham Interaction Models - OLD - NOT USED
# # -----------------------------
# model_repress_monthly <- feols(
#   total_displaced_person ~ sunab(disaster_treat, year_month_encoded) * aid_unit  + ag_grp_pc_usd |
#     name_encoded + year_month_encoded,
#   data = df_month,
#   cluster = ~name_encoded
# )
# summary(model_repress_monthly)
# 
# iplot(
#   model_repress_monthly, sep = 0.2,
#   main = "Disaster Impact on IDPs in Neighboring Units (Monthly) - Repression",
#   xlab = "Event Time (Months)", ylab = "Effect on Population Change"
# )
# 
# model_military_monthly <- feols(
#   total_displaced_person ~ sunab(disaster_treat, year_month_encoded) * aid_unit  + battles + civilian_bd_onesided + ag_grp_pc_usd |
#     name_encoded + year_month_encoded,
#   data = df_month,
#   cluster = ~name_encoded
# )
# summary(model_military_monthly)
# 
# iplot(
#   model_military_monthly, sep = 0.2,
#   main = "Disaster Impact on IDPs in Neighboring Units (Monthly) - Military Operations",
#   xlab = "Event Time (Months)", ylab = "Effect on Population Change"
# )
