# Clear environment
rm(list = setdiff(ls(), "ged"))
gc()
# Comment out ged after first load because it takes 5ever to load in

# EITM Admin 1-Level Merge Script (Country, ISO3C, Admin1, Month)
# Created by Elisa D'Amico — 12 December 2025
# Modified for monthly climate and conflict data - AFRICA ONLY, 2022+

# -----------------------------
# Load Libraries
# -----------------------------
library(countrycode)
library(dplyr)
library(furrr)
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

setwd("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/code")

# -----------------------------
# Load and filter EMDAT data - AFRICA ONLY, 2022+
# -----------------------------
emdat <- read_excel("../data/raw/public_emdat_custom_request_2025-08-05_90bba8de-dcf5-4ae8-9d0c-f57817ba8c81.xlsx") %>%
  clean_names() %>%
  mutate(year = as.integer(start_year), month = as.integer(start_month)) %>%
  filter(year >= 2022) %>%
  mutate(
    country_fixed = ifelse(country == "Canary Islands", "Spain", country),
    iso3c = countrycode(country_fixed, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
    continent = countrycode(iso3c, origin = "iso3c", destination = "continent")
  ) %>%
  filter(continent == "Africa") %>%
  select(-continent)



# -----------------------------
# Load and filter GED data - AFRICA ONLY, 2022+
# -----------------------------
# ged <- read_excel("GEDEvent_v25_1.xlsx") %>%
#   clean_names() %>%
#   mutate(
#     date_start_formatted = as.Date(date_start, format = "%Y-%m-%d"),
#     year = lubridate::year(date_start_formatted),
#     month = lubridate::month(date_start_formatted)
#   ) %>%
#   filter(year >= 2022) %>%
#   mutate(
#     country_fixed = ifelse(country == "Yemen (North Yemen)", "Yemen", country),
#     iso3c = countrycode(country_fixed, origin = "country.name", destination = "iso3c"),
#     country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
#     continent = countrycode(iso3c, origin = "iso3c", destination = "continent")
#   ) %>%
#   filter(continent == "Africa") %>%
#   select(-continent)


# Load adm1 shapefile and filter to Africa
adm1 <- st_read("../data/raw/geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp") %>%
  mutate(continent = countrycode(shapeGroup, origin = "iso3c", destination = "continent")) %>%
  filter(continent == "Africa") %>%
  select(-continent)

# Process GED data
ged_sf <- st_as_sf(ged, coords = c("longitude", "latitude"), crs = 4326)
sf::sf_use_s2(FALSE)
ged_with_adm <- st_join(ged_sf, adm1, join = st_within) %>%
  select(-iso3c) %>%
  rename(
    iso3c = shapeGroup,
    adm1_name = shapeName
  )
sf::sf_use_s2(TRUE)

# Filter GED and summarize monthly
ged_monthly <- ged_with_adm %>%
  #filter(dyad_dset_id %in% ucdpprio$dyad_id) %>%
  group_by(iso3c, adm1_name, year, month) %>%
  summarise(
    battles = n(),
    battles_onesided = sum(type_of_violence == 3),
    civilian_bd_onesided = sum(deaths_civilians[type_of_violence == 3]),
    combatant_bd_onesided = sum(deaths_b[type_of_violence == 3]),
    onesided_bd_total = sum(deaths_civilians[type_of_violence == 3]) +
      sum(deaths_b[type_of_violence == 3]),
    .groups = 'drop'
  )


# -----------------------------
# Process ACLED Territorial Conflicts - AFRICA ONLY, 2022+
# -----------------------------

acled <- read_excel("../data/raw/Africa_lagged_data_up_to-2025-01-16.xlsx", 
                    sheet = "Sheet1")

# Clean and prepare ACLED data
acled_clean <- acled %>%
  clean_names() %>%
  mutate(
    event_date = as.Date(event_date),
    year = lubridate::year(event_date),
    month = lubridate::month(event_date)
  ) %>%
  filter(year >= 2022) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
    continent = countrycode(iso3c, origin = "iso3c", destination = "continent")
  ) %>%
  filter(continent == "Africa") %>%
  select(-continent)

# Convert ACLED to sf object using lat/lon
acled_sf <- st_as_sf(acled_clean, 
                     coords = c("longitude", "latitude"), 
                     crs = 4326)

# Spatial join with adm1 to get admin1 names
sf::sf_use_s2(FALSE)
acled_with_adm <- st_join(acled_sf, adm1, join = st_within) %>%
  select(-iso3c) %>%
  rename(
    iso3c = shapeGroup,
    adm1_name = shapeName
  )
sf::sf_use_s2(TRUE)

# Aggregate ACLED territorial conflicts to adm1-month level
acled_monthly <- acled_with_adm %>%
  st_drop_geometry() %>%
  group_by(iso3c, adm1_name, year, month) %>%
  summarise(
    # Total territorial events
    territorial_events_all = sum(sub_event_type %in% c(
      "Non-state actor overtakes territory",
      "Government regains territory"
    ), na.rm = TRUE),
    
    # Rebel territorial gains
    territorial_rebel_gains = sum(sub_event_type == "Non-state actor overtakes territory", na.rm = TRUE),
    
    # Government territorial gains
    territorial_govt_regains = sum(sub_event_type == "Government regains territory", na.rm = TRUE),
    
    # All battles (for context)
    battles_all = sum(event_type == "Battles", na.rm = TRUE),
    
    # Armed clashes (non-territorial battles)
    battles_armed_clash = sum(sub_event_type == "Armed clash", na.rm = TRUE),
    
    # Fatalities from territorial events
    territorial_fatalities = sum(fatalities[sub_event_type %in% c(
      "Non-state actor overtakes territory",
      "Government regains territory"
    )], na.rm = TRUE),
    
    .groups = 'drop'
  )


# -----------------------------
# Load and filter UCDP PRIO Data - AFRICA ONLY, 2022+
# -----------------------------
ucdpprio <- read_csv("../data/raw/Dyadic_v25_1.csv") %>%
  clean_names() %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2022, type_of_conflict == 3) %>%
  select(dyad_id) %>%
  distinct()

# Load adm1 shapefile and filter to Africa
adm1 <- st_read("../data/raw/geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp") %>%
  mutate(continent = countrycode(shapeGroup, origin = "iso3c", destination = "continent")) %>%
  filter(continent == "Africa") %>%
  select(-continent)



####
####
####
####
####
####



# -----------------------------
# ADD DTM DATA
# -----------------------------

# -----------------------------
# ADD DTM DATA - AFRICA ONLY, 2022+
# -----------------------------

# -----------------------------
# ADD DTM DATA - AFRICA ONLY, 2022+
# -----------------------------

dtm <- read_csv("../data/raw/global-iom-dtm-from-api-admin-0-to-2.csv") %>%
  clean_names() %>%
  mutate(year = as.integer(year_reporting_date_number_date_year_reported)) %>%
  mutate(month = as.integer(month_reporting_date_number_date_month_reported)) %>%
  filter(admin_level_number_adm_level == 1, year >= 2022) %>%
  mutate(
    iso3c = countrycode(admin0name_number_country_name, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(admin0name_number_country_name, origin = "country.name", destination = "country.name"),
    adm1_name = admin1name_number_adm1_name,
    continent = countrycode(iso3c, origin = "iso3c", destination = "continent")
  ) %>%
  filter(continent == "Africa") %>%
  select(-continent)

# Process DTM data monthly
dtm_monthly <- dtm %>%
  group_by(iso3c, adm1_name, year, month) %>%
  summarise(
    dtm_individuals_number = sum(num_present_idp_ind_number_affected_idps, na.rm = TRUE),
    .groups = 'drop'
  )

# -----------------------------
# MIGRATION VAR
# -----------------------------

# Disable s2 for adjacency check (avoids strict polygon errors)
sf_use_s2(FALSE)

# Repair any invalid geometries
adm1 <- st_make_valid(adm1)

# Find touching polygons
touch_list <- st_touches(adm1)

# Build adjacency data frame
adjacency_df <- data.frame(
  from = rep(seq_along(touch_list), lengths(touch_list)),
  to   = unlist(touch_list)
)

# Add attributes: shapeName & shapeGroup for both ends
adjacency_df <- adjacency_df %>%
  mutate(
    from_shapeName  = adm1$shapeName[from],
    to_shapeName    = adm1$shapeName[to],
    from_shapeGroup = adm1$shapeGroup[from],
    to_shapeGroup   = adm1$shapeGroup[to]
  ) %>%
  filter(from != to, from_shapeGroup == to_shapeGroup)  # Only within same group

# -----------------------------
# Create shell - 2022-2024 ONLY
# -----------------------------
unique_adm1_pairs <- adm1 %>%
  st_drop_geometry() %>%
  select(adm1_name = shapeName, iso3c = shapeGroup) %>%
  distinct()

years <- 2022:2024
months <- 1:12

shell_panel <- expand_grid(unique_adm1_pairs, year = years, month = months)

# -----------------------------
# Merge in 
# -----------------------------
merged_df <- full_join(
  shell_panel,
  adjacency_df,
  by = c(
    "adm1_name" = "from_shapeName",
    "iso3c"     = "from_shapeGroup"
  )
)

merged_df2 <- left_join(
  merged_df,
  dtm,
  by = c(
    "to_shapeName"  = "admin1name_number_adm1_name",
    "to_shapeGroup" = "iso3c",
    "month"         = "month",
    "year"          = "year"
  )
)


idp_neighbortotal <- merged_df2 %>%
  select(adm1_name = adm1_name.x, iso3c, month, year, num_present_idp_ind_number_affected_idps) %>%
  group_by(adm1_name, iso3c, month, year) %>%
  summarise(
    total_idps = sum(num_present_idp_ind_number_affected_idps, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Load and filter DOSE data - AFRICA ONLY, 2022+
# -----------------------------

# -----------------------------
# Load and filter Economic/GRP data - AFRICA ONLY, 2022+
# -----------------------------

dose <- read_csv("../data/raw/DOSE_V2.9.csv") %>%
  clean_names() %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2022) %>%
  mutate(
    country_fixed = case_when(
      country == "Netherlands Antilles" ~ "Netherlands Antilles",
      TRUE ~ country
    ),
    iso3c = case_when(
      country_fixed == "Netherlands Antilles" ~ "ANT",
      TRUE ~ countrycode(country_fixed, origin = "country.name", destination = "iso3c")
    ),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
    continent = countrycode(iso3c, origin = "iso3c", destination = "continent")
  ) %>%
  filter(continent == "Africa") %>%
  select(-continent)

# Process economic data - expand to monthly (annual values repeated for each month)
dose_monthly <- dose %>%
  expand_grid(month = 1:12) %>%
  group_by(iso3c, year, month) %>%
  summarise(
    grp_pc_usd_2015 = mean(grp_pc_usd_2015, na.rm = TRUE),
    grp_pc_lcu2015_usd = mean(grp_pc_lcu2015_usd, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE),
    ag_grp_pc_usd_2015 = mean(ag_grp_pc_usd_2015, na.rm = TRUE),
    man_grp_pc_usd_2015 = mean(man_grp_pc_usd_2015, na.rm = TRUE),
    serv_grp_pc_usd_2015 = mean(serv_grp_pc_usd_2015, na.rm = TRUE),
    .groups = 'drop'
  )


# -----------------------------
# Process EMDAT data - AFRICA ONLY, 2022+
# -----------------------------

emdat_combined <- read_csv("../data/raw/emdat_adm1.csv") %>%
  mutate(
    continent = countrycode(iso3c, origin = "iso3c", destination = "continent"),
    year = as.integer(start_year)
  ) %>%
  filter(continent == "Africa", year >= 2022) %>%
  select(-continent)

emdat_monthly <- emdat_combined %>%
  mutate(month = as.integer(start_month), year = as.integer(start_year)) %>%
  group_by(iso3c, adm1_name, year, month) %>%
  summarise(
    disasters_all = n(),
    disasters_w_reconstruction = sum(!is.na(reconstruction_costs_adjusted_000_us)),
    disasters_w_damage = sum(!is.na(total_damage_000_us)),
    disasters_flood = sum(disaster_type == "Flood", na.rm = TRUE),
    disasters_earthquake = sum(disaster_type == "Earthquake", na.rm = TRUE),
    total_deaths_sum = sum(total_deaths, na.rm = TRUE),
    no_injured_sum = sum(no_injured, na.rm = TRUE),
    no_affected_sum = sum(no_affected, na.rm = TRUE),
    no_homeless_sum = sum(no_homeless, na.rm = TRUE),
    total_affected_sum = sum(total_affected, na.rm = TRUE),
    reconstruction_costs_sum = sum(reconstruction_costs_000_us, na.rm = TRUE),
    reconstruction_costs_adjusted_sum = sum(reconstruction_costs_adjusted_000_us, na.rm = TRUE),
    insured_damage_sum = sum(insured_damage_000_us, na.rm = TRUE),
    insured_damage_adjusted_sum = sum(insured_damage_adjusted_000_us, na.rm = TRUE),
    total_damage_sum = sum(total_damage_000_us, na.rm = TRUE),
    total_damage_adjusted_sum = sum(total_damage_adjusted_000_us, na.rm = TRUE),
    .groups = 'drop'
  )


# -----------------------------
# Load and process Grid Risk data - AFRICA ONLY, 2022+
# -----------------------------

grid_risk <- read_csv("../data/raw/grid_risk_africa_2022_2024.csv")

# Convert to sf object
grid_risk_sf <- st_as_sf(grid_risk, 
                         coords = c("Longitude", "Latitude"), 
                         crs = 4326)

# Spatial join with adm1 to get admin1 names
sf_use_s2(FALSE)
grid_risk_with_adm <- st_join(grid_risk_sf, adm1, join = st_within) %>%
  st_drop_geometry() %>%
  select(year, month, isocode, risk, shapeName, shapeGroup) %>%
  rename(
    adm1_name = shapeName,
    iso3c = shapeGroup
  )
sf_use_s2(TRUE)

# Aggregate grid risk to adm1-month level
grid_risk_monthly <- grid_risk_with_adm %>%
  filter(!is.na(adm1_name), !is.na(iso3c)) %>%
  group_by(iso3c, adm1_name, year, month) %>%
  summarise(
    grid_risk_mean = mean(risk, na.rm = TRUE),
    grid_risk_max = max(risk, na.rm = TRUE),
    grid_risk_min = min(risk, na.rm = TRUE),
    grid_cells_count = n(),
    .groups = 'drop'
  )




# -----------------------------
# Create shell - 2022-2024 ONLY
# -----------------------------
unique_adm1_pairs <- adm1 %>%
  st_drop_geometry() %>%
  select(adm1_name = shapeName, iso3c = shapeGroup) %>%
  distinct()

years <- 2022:2024
months <- 1:12

shell_panel <- expand_grid(unique_adm1_pairs, year = years, month = months)


####
####
####
####
####
####


# -----------------------------
# Final Merge (Updated to include Economic/GRP and DTM)
# -----------------------------
# -----------------------------
# Final Merge (Updated to include Economic/GRP, DTM, and Grid Risk)
# -----------------------------
# -----------------------------
# Final Merge (Updated to include ACLED)
# -----------------------------
merged_monthly <- shell_panel %>%
  left_join(ged_monthly, by = c("iso3c", "adm1_name", "year", "month")) %>%
  left_join(emdat_monthly, by = c("iso3c", "adm1_name", "year", "month")) %>%
  left_join(dtm_monthly, by = c("iso3c", "adm1_name", "year", "month")) %>%
  left_join(idp_neighbortotal, by = c("iso3c", "adm1_name", "year", "month")) %>%
  left_join(grid_risk_monthly, by = c("iso3c", "adm1_name", "year", "month")) %>%
  left_join(acled_monthly, by = c("iso3c", "adm1_name", "year", "month")) %>%  # ADD THIS
  left_join(dose_monthly, by = c("iso3c", "year", "month"))

# Replace NAs with 0 for all conflict, climate, displacement, economic, and grid risk variables
merged_monthly <- merged_monthly %>%
  mutate(across(
    c(
      battles, battles_onesided, civilian_bd_onesided, combatant_bd_onesided,
      onesided_bd_total, disasters_all, disasters_w_reconstruction, disasters_w_damage,
      disasters_flood, disasters_earthquake, total_deaths_sum, no_injured_sum,
      no_affected_sum, no_homeless_sum, total_affected_sum, reconstruction_costs_sum,
      reconstruction_costs_adjusted_sum, insured_damage_sum, insured_damage_adjusted_sum,
      total_damage_sum, total_damage_adjusted_sum, dtm_individuals_number, 
      total_idps, grp_pc_usd_2015, grp_pc_lcu2015_usd, pop,
      ag_grp_pc_usd_2015, man_grp_pc_usd_2015, serv_grp_pc_usd_2015,
      grid_risk_mean, grid_risk_max, grid_risk_min, grid_cells_count,
      # ADD ACLED VARIABLES HERE
      territorial_events_all, territorial_rebel_gains, territorial_govt_regains,
      battles_all, battles_armed_clash, territorial_fatalities
    ),
    ~ replace_na(.x, 0)
  ))
# -----------------------------
# Treatment Vars (adapted for monthly data)
# -----------------------------
# Create year_month and encoded version first
merged_monthly <- merged_monthly %>%
  mutate(
    year_month = sprintf("%04d-%02d", year, month),  # ensures "2022-02" not "2022-2"
    year_month_encoded = match(
      year_month,
      sort(unique(sprintf("%04d-%02d", year, month)))
    )
  )

# List of disaster variables to loop through
disaster_vars <- c(
  "disasters_all",
  "disasters_w_reconstruction",
  "disasters_w_damage",
  "disasters_flood",
  "disasters_earthquake"
)

# Loop over each disaster variable
walk(disaster_vars, function(var) {
  treat_var <- paste0(var, "_treat")
  
  # Create a treatment df for the current disaster variable
  disaster_treatment_df <- merged_monthly %>%
    select(iso3c, adm1_name, year_month_encoded, !!sym(var)) %>%
    filter(!!sym(var) != 0) %>%
    arrange(iso3c, adm1_name, year_month_encoded) %>%
    group_by(iso3c, adm1_name) %>%
    summarise(!!treat_var := first(year_month_encoded), .groups = 'drop')
  
  # Join treatment info back and create binary indicator
  merged_monthly <<- merged_monthly %>%
    left_join(disaster_treatment_df, by = c("iso3c", "adm1_name")) %>%
    mutate(
      !!treat_var := ifelse(is.na(!!sym(treat_var)), "0", !!sym(treat_var)),
      !!paste0(var, "_bin") := as.numeric(year_month_encoded >= !!sym(treat_var))
    )
})

# -----------------------------
# Encoding
# -----------------------------
merged_monthly <- merged_monthly %>%
  mutate(
    iso3c_adm1 = paste0(iso3c, "-", adm1_name),
    iso3c_adm1_encoded = as.integer(factor(iso3c_adm1))
  )


####
####
####
####
####
####


## Filter only to civil war states
merged_monthly <- merged_monthly %>% filter(!is.na(iso3c) & iso3c %in% ged_with_adm$iso3c)

# -----------------------------
# Save Output
# -----------------------------
merged_clean <- merged_monthly %>%
  # Drop the geometry column if it exists
  {if("geometry" %in% names(.)) select(., -geometry) else .} %>%
  
  # Create new variables in a single mutate call
  mutate(
    country_name = countrycode(iso3c, "iso3c", "country.name"),
    iso3n = countrycode(iso3c, "iso3c", "iso3n"),
    year_month = paste(year, sprintf("%02d", month), sep = "-")
  ) %>%
  
  # Select and reorder the final columns
  select(
    country_name,
    iso3c,
    iso3n,
    iso3c_adm1,
    iso3c_adm1_encoded,
    adm1_name,
    year,
    month,
    year_month,
    year_month_encoded,
    everything()
  )

data.table::fwrite(merged_clean, "../data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv")