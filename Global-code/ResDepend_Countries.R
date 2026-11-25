# ============================================================
# Clear environment
# ============================================================
rm(list = setdiff(ls(), "ged"))
gc()

# ============================================================
# Libraries
# ============================================================
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(did)
library(gridExtra)

# ============================================================
# Load & Filter Data
# ============================================================
YEAR_RANGE <- c(2022, 2024)

df <- read_csv(
  "../data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv"
) %>%
  mutate(
    across(
      c(battles, year_month_encoded, iso3c_adm1_encoded, disasters_all_treat),
      as.numeric
    )
  ) %>%
  filter(year >= YEAR_RANGE[1], year <= YEAR_RANGE[2])

# ============================================================
# Resource Dependence Indicator
# ============================================================
resource_dependent_countries <- c(
    "DZA",  # Algeria - oil/gas
    "AGO",  # Angola - oil
    "LBY",  # Libya - oil
    "NGA",  # Nigeria - oil
    "TCD",  # Chad - oil
    "COG",  # Congo - oil
    "SDN",  # Sudan - oil
    "SSD",  # South Sudan - oil
    "COD",  # DRC - minerals
    "ZAF",  # South Africa - minerals
    "BFA",  # Burkina Faso - gold
    "MLI",  # Mali - gold
    "NER",  # Niger - uranium
    "MRT"   # Mauritania - iron ore/gold
  )

df <- df %>%
  mutate(resource_dependent = iso3c %in% resource_dependent_countries)

# ============================================================
# Country-Level Mean Grid Risk (WITHIN-COUNTRY SPLIT)
# ============================================================
df <- df %>%
  group_by(country_name) %>%
  mutate(
    country_mean_grid_risk = mean(grid_risk_max, na.rm = TRUE)
  ) %>%
  ungroup()

df <- df %>%
  mutate(
    high_risk_within_country = grid_risk_max > country_mean_grid_risk
  )

# ============================================================
# 2 × 2 SPLIT
# ============================================================

df_resource_high <- df %>%
  filter(resource_dependent, high_risk_within_country)

df_resource_low <- df %>%
  filter(resource_dependent, !high_risk_within_country)

df_nonresource_high <- df %>%
  filter(!resource_dependent, high_risk_within_country)

df_nonresource_low <- df %>%
  filter(!resource_dependent, !high_risk_within_country)

# ============================================================
# Sanity Checks
# ============================================================
cat("\n=== Sample Sizes ===\n")
print(
  sapply(
    list(
      res_high = df_resource_high,
      res_low  = df_resource_low,
      nonres_high = df_nonresource_high,
      nonres_low  = df_nonresource_low
    ),
    nrow
  )
)

# ============================================================
# Set seed
# ============================================================
set.seed(666)

# ============================================================
# FUNCTION TO RUN DiD + AGGREGATE
# ============================================================
run_did <- function(data, outcome) {
  did <- att_gt(
    yname = outcome,
    tname = "year_month_encoded",
    idname = "iso3c_adm1_encoded",
    gname = "disasters_all_treat",
    data = data,
    panel = FALSE,
    control_group = "notyettreated",
    cores = 7
  )
  aggte(did, type = "dynamic", na.rm = TRUE)
}

# ============================================================
# RUN MODELS (ALL 4 CELLS)
# ============================================================

# Repression
rep_res_high <- run_did(df_resource_high, "civilian_bd_onesided")
rep_res_low  <- run_did(df_resource_low,  "civilian_bd_onesided")
rep_non_high <- run_did(df_nonresource_high, "civilian_bd_onesided")
rep_non_low  <- run_did(df_nonresource_low,  "civilian_bd_onesided")

# Military
mil_res_high <- run_did(df_resource_high, "battles")
mil_res_low  <- run_did(df_resource_low,  "battles")
mil_non_high <- run_did(df_nonresource_high, "battles")
mil_non_low  <- run_did(df_nonresource_low,  "battles")

# IDPs
idp_res_high <- run_did(df_resource_high, "total_idps")
idp_res_low  <- run_did(df_resource_low,  "total_idps")
idp_non_high <- run_did(df_nonresource_high, "total_idps")
idp_non_low  <- run_did(df_nonresource_low,  "total_idps")

# ============================================================
# PLOTTING FUNCTION
# ============================================================
plot_did <- function(agg, title) {
  ggdid(agg) +
    labs(
      title = title,
      x = "Time to Treatment (Months)",
      y = "ATTs"
    ) +
    scale_color_manual(
      values = c("#F08080", "#8B008B"),
      labels = c("Pre-Treatment", "Post-Treatment")
    ) +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", hjust = 0.5,
        family = "Times New Roman"
      ),
      axis.text.x = element_text(
        size = 8, angle = 45, hjust = 1,
        family = "Times New Roman"
      ),
      axis.text.y = element_text(size = 8, family = "Times New Roman"),
      axis.title = element_text(size = 10, family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman")
    )
}

# ============================================================
# CREATE 4-PANEL FIGURES (EXAMPLE: REPRESSION)
# ============================================================

grid_rep <- grid.arrange(
  plot_did(rep_res_low,  "Resource-Dependent × Low Risk (Within Country)"),
  plot_did(rep_res_high, "Resource-Dependent × High Risk (Within Country)"),
  plot_did(rep_non_low,  "Non-Resource × Low Risk (Within Country)"),
  plot_did(rep_non_high, "Non-Resource × High Risk (Within Country)"),
  ncol = 2,
  top = grid::textGrob(
    "Repression Effects by Resource Dependence and Within-Country Risk",
    gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "Times New Roman")
  )
)

cat("\n=== Analysis Complete ===\n")

grid_mil <- grid.arrange(
  plot_did(mil_res_low,  "Resource-Dependent × Low Risk (Within Country)"),
  plot_did(mil_res_high, "Resource-Dependent × High Risk (Within Country)"),
  plot_did(mil_non_low,  "Non-Resource × Low Risk (Within Country)"),
  plot_did(mil_non_high, "Non-Resource × High Risk (Within Country)"),
  ncol = 2,
  top = grid::textGrob(
    "Military Effort (Battles) by Resource Dependence and Within-Country Risk",
    gp = grid::gpar(
      fontsize = 16,
      fontface = "bold",
      fontfamily = "Times New Roman"
    )
  )
)

grid_idp <- grid.arrange(
  plot_did(idp_res_low,  "Resource-Dependent × Low Risk (Within Country)"),
  plot_did(idp_res_high, "Resource-Dependent × High Risk (Within Country)"),
  plot_did(idp_non_low,  "Non-Resource × Low Risk (Within Country)"),
  plot_did(idp_non_high, "Non-Resource × High Risk (Within Country)"),
  ncol = 2,
  top = grid::textGrob(
    "Displacement (IDPs) by Resource Dependence and Within-Country Risk",
    gp = grid::gpar(
      fontsize = 16,
      fontface = "bold",
      fontfamily = "Times New Roman"
    )
  )
)
