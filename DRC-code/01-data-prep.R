# Version 10/31/2025 from EDA
# Update log:
# 2025-12-30 NH: add territorial takeover events
# 2026-01-03 NH: recode treatment variable

gc()
rm(list = setdiff(ls(all.names = TRUE), c("ged_drc")))

# Load required libraries
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(countrycode)
library(tidyr)
library(readr)      # for CSV files
library(readxl)     # for Excel files
library(haven)      # for various data formats including some package files
library(sf)
library(stringr)
library(purrr)
library(ggplot2)
library(jsonlite)
library(geodata)


# setwd("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/")

# Load the CSV file
ocha_data <- read_csv("../data/raw/DRC/ocha_hz_month.csv")

# Load the Excel files
drc_aid <- read_excel("../data/raw/DRC/drc_aid.xlsx")
drc_disaster <- read_excel("../data/raw/DRC/drc_disaster.xlsx") %>%
  clean_names() %>%
  mutate(year = as.integer(start_year), month = as.integer(start_month)) %>%
  filter(year >= 2019) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name")
  )

# Simple location expansion
emdat_expanded <- drc_disaster %>%
  # Step 1: Delete everything in parentheses and the parentheses themselves
  mutate(location = str_replace_all(location, "\\([^)]*\\)", "")) %>%
  # Step 2: Delete all periods
  mutate(location = str_replace_all(location, "\\.", "")) %>%
  # Step 3: Expand by comma and "and"
  separate_rows(location, sep = ",") %>%
  separate_rows(location, sep = ";") %>%
  separate_rows(location, sep = " and ") %>%
  # Clean whitespace
  mutate(location = str_trim(location)) %>%
  # Remove blank, missing, and empty locations
  filter(!is.na(location) & location != "" & nchar(location) > 0) %>%
  # Remove problematic entries
  filter(
    !str_detect(location, "lake"),                           # Remove entries starting with "1 "
    !str_detect(location, "Lake"),                           # Remove entries starting with "1 "
    !str_detect(location, "City"),                           # Remove entries starting with "1 "
    !str_detect(location, "city"),                           # Remove entries starting with "1 "
    !str_detect(location, "Town"),                           # Remove entries starting with "1 "
    !str_detect(location, "town"),                           # Remove entries starting with "1 "
    !str_detect(location, "Territory"),                           # Remove entries starting with "1 "
    !str_detect(location, "territory"),                           # Remove entries starting with "1 "
    !str_detect(location, "^1 "),                           # Remove entries starting with "1 "
    !str_detect(location, "creating a humanitarian"),       # Remove sentence fragments
    !str_detect(location, "area"),       # Remove sentence fragments
    !str_detect(location, "Equateur"),       # Remove sentence fragments
    !str_detect(location, "Near"),       # Remove sentence fragments
    !str_detect(location, "near"),       # Remove sentence fragments
    !str_detect(location, "province"),       # Remove sentence fragments
    !str_detect(location, "Province"),       # Remove sentence fragments
    !str_detect(location, "eastern DRC\\)$"),              # Remove incomplete parentheses
    !str_detect(location, "between "),                     # Remove "between X" fragments
    !str_detect(location, "\\);"),                         # Remove entries with "); " 
    !str_detect(location, "^Nord -"),                      # Remove "Nord -" variants
    !str_detect(location, "^Sud -"),                       # Remove "Sud -" variants
    !str_detect(location, "^Bas -")                        # Remove "Bas -" variants
  ) %>%
  # Step 4: Sort and keep first occurrence of each location
  mutate(location = str_trim(location)) %>%
  arrange(start_year, start_month, location) %>%
  group_by(location) %>%
  slice(1) %>%
  ungroup() |> 
  filter(
    year > 2020
  )


# For the .gpkg file (GeoPackage - a spatial data format)
# You'll need the sf package for spatial data
osm_data <- st_read("../data/raw/DRC/osm_rdc_sante_zones_211212.gpkg")

# ggplot(osm_data) +
#   geom_sf() +
#   theme_minimal()

# -----------------------------
# Load and filter GED data with country fixes - DRC ONLY
# -----------------------------
#COMMENT/UNCOMMENT FOR RUNNING QUICKER

ged_drc <- read_excel("../../Global/data/raw/GEDEvent_v25_1.xlsx") %>%
  clean_names() %>%
  mutate(
    # Convert date_start to a date object and then extract the year and month
    date_start_formatted = as.Date(date_start, format = "%Y-%m-%d"),
    year = lubridate::year(date_start_formatted),
    month = lubridate::month(date_start_formatted)
  ) %>%
  filter(year >= 2019) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name")
  ) %>%
  # Filter for DRC specifically
  filter(iso3c == "COD" |
           country %in% c("Democratic Republic of the Congo", "Democratic Republic of Congo", "DR Congo", "DRC", "Congo, Dem. Rep."))



### Setting adm units 

# Manual coordinate lookup for DRC locations
drc_coords <- data.frame(
  location = c(
    "Angumu", "Bandalungwa", "Bankay", "Barumbu", "Bas-Uele",
    "Bihambwe", "Bolenge", "Bondo", "Bukavu", "Bulwa",
    "Bumbu", "Burhinyi", "Bushushu", "Buyofwe", "Congo river",
    "Dekese", "Disengomoka districts", "Goma", "Gombe", "Haut-Katanga",
    "Haut-Uele", "Ituri", "Kabalo", "Kalamu", "Kalemie",
    "Kampene", "Kananga", "Kasaba", "Kasai", "Kasaï",
    "Kasaï Central", "Kibuba", "Kinshasa", "Kisenso", "Kitambo",
    "Kongolo", "Kutu", "Kwilu et Tshuapa Kongo Central", "Lemba", "Limete",
    "Lomami", "Lukenie river", "Mahanga", "Mai Ndombe", "Makala",
    "Maniema", "Manon", "Masina", "Masisi", "Matete",
    "Matete communes", "Matshia", "Mbandaka", "Mbanza-Ngungu", "Mboko",
    "Moba", "Mongala", "Mongala river", "Mount Ngafula", "Ngaba",
    "Ngaliema", "Ngiri-Ngiri", "Noki", "Nord-Kivu", "Nord-Ubangi",
    "North Kivu", "Nyabibwe", "Nyamukubi", "Paapa", "Punia Kasongo",
    "Rethy", "Rubaya", "Sankuru", "Selembao", "South Kivu",
    "Sud-Kivu", "Sud-Ubangi", "Tanganyika", "Territories of Kailo", "Tshimbinda",
    "Tshopo", "Uvira", "Wangata"
  ),
  latitude = c(
    2.13, -4.37, -2.95, -4.32, 3.5,
    -1.47, -0.25, 3.82, -2.51, -1.58,
    -4.38, -2.87, -2.48, -6.12, -4.0,
    -2.64, -5.88, -1.68, -4.32, -11.5,
    3.0, 2.0, -6.08, -4.34, -5.92,
    -2.35, -5.90, -2.85, -5.5, -5.5,
    -5.9, -5.5, -4.32, -4.39, -4.31,
    -5.34, -3.0, -5.0, -4.35, -4.37,
    -4.5, -3.2, -1.65, -2.0, -4.34,
    -4.0, -6.12, -4.51, -1.60, -4.34,
    -4.34, -6.2, 0.05, -5.85, -6.12,
    -4.66, 1.5, 1.5, -4.42, -4.33,
    -4.33, -4.31, -6.05, -1.0, 3.5,
    -1.0, -2.29, -2.55, -1.85, -2.6,
    2.18, -1.48, -4.0, -4.35, -2.5,
    -2.5, 3.5, -4.5, -2.8, -6.5,
    0.5, -3.37, 0.08
  ),
  longitude = c(
    30.92, 15.28, 19.8, 15.30, 25.0,
    29.32, 18.25, 23.69, 28.86, 28.9,
    15.25, 28.9, 28.84, 16.75, 20.0,
    20.85, 13.05, 29.23, 15.32, 27.0,
    28.0, 30.0, 29.08, 15.31, 29.19,
    26.7, 22.42, 28.8, 22.0, 22.0,
    22.5, 18.5, 15.31, 15.32, 15.30,
    27.21, 18.5, 17.0, 15.32, 15.28,
    25.0, 19.5, 29.18, 18.0, 15.31,
    26.0, 29.08, 15.32, 29.15, 15.31,
    15.31, 22.4, 18.10, 14.86, 29.08,
    29.62, 21.0, 21.0, 15.25, 15.29,
    15.29, 15.31, 13.28, 29.0, 21.0,
    29.0, 28.65, 28.8, 29.2, 26.8,
    30.83, 29.24, 24.0, 15.28, 28.5,
    28.5, 21.0, 29.0, 26.5, 22.5,
    24.0, 29.14, 18.10
  ),
  stringsAsFactors = FALSE
)

###### 
#----- new as of 24-October for disaster treatment
###### 

emdat_expanded <- emdat_expanded %>%
  mutate(
    admin_parsed = map(admin_units, possibly(~ fromJSON(.), otherwise = list())),
    adm1_code = map(admin_parsed, ~ if(length(.) > 0 && "adm1_code" %in% names(.)) .$adm1_code else NA_real_),
    adm1_name = map(admin_parsed, ~ if(length(.) > 0 && "adm1_name" %in% names(.)) .$adm1_name else NA_character_),
    adm2_code = map(admin_parsed, ~ if(length(.) > 0 && "adm2_code" %in% names(.)) .$adm2_code else NA_real_),
    adm2_name = map(admin_parsed, ~ if(length(.) > 0 && "adm2_name" %in% names(.)) .$adm2_name else NA_character_)
  ) %>%
  select(-admin_parsed) %>%
  unnest(cols = c(adm1_code, adm1_name, adm2_code, adm2_name), keep_empty = TRUE)


emdat_with_coords <- emdat_expanded %>%
  left_join(
    drc_coords %>% rename(lat_lookup = latitude, lon_lookup = longitude), 
    by = "location"
  ) %>%
  mutate(
    latitude = if_else(is.na(adm1_name) & is.na(adm2_name), lat_lookup, NA_real_),
    longitude = if_else(is.na(adm1_name) & is.na(adm2_name), lon_lookup, NA_real_)
  ) %>%
  select(-lat_lookup, -lon_lookup)

#emdat_with_coords <- emdat_expanded %>%
#  left_join(drc_coords, by = "location")


# Plot the coordinates on the map
ggplot() +
  geom_sf(data = osm_data) +
  geom_point(data = drc_coords,
             aes(x = longitude, y = latitude),
             color = "red",
             size = 2,
             alpha = 0.7) +
  theme_minimal() +
  labs(title = "DRC EMDAT Disaster Locations",
       subtitle = "Red points show disaster-affected locations") +
  coord_sf(xlim = c(12, 32), ylim = c(-13, 6))

# -----------------------------
# Merge DRC Aid data onto SF object by lon/lat
# -----------------------------

# Convert drc_aid to sf object for spatial join
# (Assuming lon/lat columns exist - adjust column names as needed)
drc_aid_sf <- drc_aid %>%
  # Clean and prepare the data
  filter(!is.na(lon) & !is.na(lat)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # Convert to sf object
  st_transform(st_crs(osm_data))  # Transform to match osm_data CRS

# Spatial join: find which zone each aid point falls into
aid_joined <- st_join(drc_aid_sf, osm_data, join = st_within)

# Convert back to regular dataframe and aggregate by name, year, month
aid_aggregated <- aid_joined %>%
  st_drop_geometry() %>%  # Remove geometry column
  # Group by zone name, year, month and count occurrences
  group_by(name) %>%  # Adjust these column names based on your data
  summarise(
    aid_unit = n(),
    .groups = 'drop'
  )

# -----------------------------
# Merge GED data onto SF object by lat/lon
# -----------------------------


# Convert ged_drc to sf object for spatial join
ged_drc_sf <- ged_drc %>%
  # Clean and prepare the data
  filter(!is.na(longitude) & !is.na(latitude)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  # Convert to sf object
  st_transform(st_crs(osm_data))  # Transform to match osm_data CRS

# Spatial join: find which zone each conflict event falls into
ged_joined <- st_join(ged_drc_sf, osm_data, join = st_within)

# Convert back to regular dataframe and aggregate by name, year, month
ged_aggregated <- ged_joined %>%
  st_drop_geometry() %>%  # Remove geometry column
  # Group by zone name, year, month and count events
  group_by(name, year, month) %>%  # name comes from osm_data zones
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
# Merge aggregated data back onto the SF object
# -----------------------------

# Create a complete grid of all zone names, years, and months to ensure no missing combinations
all_zones <- osm_data %>%
  st_drop_geometry() %>%
  select(name) %>%
  distinct()

# Create year-month combinations (adjust range as needed)
year_range <- 2022:2024
month_range <- 1:12

complete_grid <- expand_grid(
  name = all_zones$name,
  year = year_range,
  month = month_range
)

# -----------------------------
# Merge Disaster data onto SF object by lat/lon
# -----------------------------

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# ADDED 24-OCT FOR DISASTER TREATMENT REVISIONS

# -------------------------------------------------------------------------------------------------

# Get DRC administrative boundaries and convert to sf
drc_adm2 <- geodata::gadm(country = "COD", 
                          level = 2, path = tempdir())
drc_adm2 <- st_as_sf(drc_adm2)  # Convert to sf object

# Function to clean names for matching
clean_names_for_match <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[[:space:]-]", "") %>%  # Remove spaces and hyphens
    str_replace_all("[àâä]", "a") %>%
    str_replace_all("[éèêë]", "e") %>%
    str_replace_all("[îï]", "i") %>%
    str_replace_all("[ôö]", "o") %>%
    str_replace_all("[ùûü]", "u") %>%
    str_replace_all("[^a-z0-9]", "")  # Remove any remaining special chars
}

# Prepare emdat_with_coords
emdat_with_coords <- emdat_expanded %>%
  left_join(
    drc_coords %>% rename(lat_manual = latitude, lon_manual = longitude), 
    by = "location"
  )

# Split into two groups: those with adm2_name and those without
emdat_with_adm2 <- emdat_with_coords %>%
  filter(!is.na(adm2_name)) %>%
  mutate(
    adm2_name_recoded = case_when(
      adm2_name == "Cataractes" ~ "Kongo Central",
      adm2_name == "Bukavu" ~ "Sud-Kivu",
      TRUE ~ adm2_name
    ),
    adm2_name_clean = clean_names_for_match(adm2_name_recoded)
  )

emdat_without_adm2 <- emdat_with_coords %>%
  filter(is.na(adm2_name)) %>%
  filter(!is.na(lon_manual) & !is.na(lat_manual)) %>%
  mutate(
    longitude = lon_manual,
    latitude = lat_manual
  )

# Clean GADM names
drc_adm2 <- drc_adm2 %>%
  mutate(NAME_1_clean = clean_names_for_match(NAME_1))

# Join emdat data onto the adm2 shapefile by cleaned names
emdat_adm2_sf <- drc_adm2 %>%
  inner_join(
    emdat_with_adm2,
    by = c("NAME_1_clean" = "adm2_name_clean")
  ) %>%
  st_transform(st_crs(osm_data))

# For the manual coords group, convert to sf
emdat_without_adm2_sf <- emdat_without_adm2 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(osm_data))

# Spatial join each to osm
disaster_joined_adm2 <- st_join(emdat_adm2_sf, osm_data, join = st_intersects)
disaster_joined_manual <- st_join(emdat_without_adm2_sf, osm_data, join = st_within)

# Combine results
disaster_joined <- bind_rows(
  disaster_joined_adm2 %>% st_drop_geometry(),
  disaster_joined_manual %>% st_drop_geometry()
)

# Aggregate by health zone
disaster_aggregated <- disaster_joined %>%
  group_by(name, year, month) %>%
  summarise(
    disaster_events = n(),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_affected = sum(no_affected, na.rm = TRUE),
    no_homeless = sum(no_homeless, na.rm = TRUE),
    total_damage_000_us = sum(total_damage_000_us, na.rm = TRUE),
    .groups = 'drop'
  )


# -----------------------------
# OCHA Name Mapping and Processing
# -----------------------------

# Create name mapping for OCHA zones
ocha_name_mapping <- data.frame(
  hz_label_from = c("Gethy", "Kiyambi", "Mongbalu", "Nia-Nia", "Nyakunde", "Nyiragongo", "Tshitenge"),
  name = c("Gety", "Kiambi", "Mongbwalu", "Nia Nia", "Nyankunde", "Nyirangongo", "Tshilenge")
)

# Process OCHA data
ocha_processed <- ocha_data %>%
  clean_names() %>%
  # Apply name mapping for the 7 mismatched names
  left_join(ocha_name_mapping, by = "hz_label_from") %>%
  # Use mapped name if available, otherwise use original hz_label_from
  mutate(name = ifelse(is.na(name), hz_label_from, name)) %>%
  # Filter to only zones that exist in OSM data
  filter(name %in% osm_data$name) %>%
  # Use correct column names
  mutate(
    year = as.integer(year),
    month = as.integer(monthno)  # Use monthno for numeric month
  ) %>%
  filter(year >= 2019) %>%
  # Aggregate by zone, year, month
  group_by(name, year, month) %>%
  summarise(
    total_displaced_hh = sum(total_displaced_hh, na.rm = TRUE),  # Remove quotes, add na.rm
    total_displaced_person = sum(total_displaced_person, na.rm = TRUE),  # Remove quotes, add na.rm, add comma
    distinct_destination = sum(distinct_destination, na.rm = TRUE),  # Add this variable
    .groups = 'drop'
  )

# Get unique zone names that appear in OCHA data
ocha_zones <- ocha_processed %>%
  select(name) %>%
  distinct() %>%
  pull(name)

# Add conflict forecast data ----

forecast_hz <- readr::read_csv("../data/raw/DRC/conflict-forecast/forecast_drc_hz.csv")
forecast_hz_id <- osm_data %>% select(full_id, name) %>% 
  left_join(forecast_hz, ., 
            by = "full_id") %>% 
  select(-1) %>% 
  mutate(year = as.numeric(sub("m.*", "", month)),
         month = as.numeric(sub(".*m", "", month)))

# Add territorial takeover data ----

acled <- readr::read_csv("../data/raw/DRC/acled/working_data_hz_2025-09-05.csv")
ocha_zones_2 <- acled |> pull(hz_label_from) |> unique() |> sort()
ocha_zones_1 <- complete_grid |> pull(name) |> unique() |> sort()
union(ocha_zones_1, ocha_zones_2)

acled_territory <- acled |> 
  select(hz_label_from:monthname, no_takeover_govt:no_osv_protests) |> 
  rename(name = hz_label_from)

# Then update your final merge section to include OCHA data:

# Merge all data together (updated to include OCHA data)
final_data <- complete_grid %>%
  left_join(aid_aggregated, by = c("name")) %>%
  left_join(ged_aggregated, by = c("name", "year", "month")) %>%
  left_join(disaster_aggregated, by = c("name", "year", "month")) %>%
  left_join(ocha_processed, by = c("name", "year", "month")) %>%  # Add this line
  left_join(., forecast_hz_id, by = c("name", "year", "month")) %>% 
  left_join(., acled_territory, by = c("name" = "name", 
                                       "year" = "year", 
                                       "month" = "monthno")) |> 
  mutate(across(c(aid_unit:distinct_destination), ~ replace_na(., 0))) %>% 
  mutate(across(c(no_takeover_govt:no_osv_protests), ~ replace_na(., 0))) #%>% 
  # Replace NA values with 0 for counts - using the correct variable names
  # mutate(
  #   # Aid variables
  #   aid_unit = ifelse(is.na(aid_unit), 0, aid_unit),
  #   
  #   # Conflict variables  
  #   battles = ifelse(is.na(battles), 0, battles),
  #   battles_onesided = ifelse(is.na(battles_onesided), 0, battles_onesided),
  #   civilian_bd_onesided = ifelse(is.na(civilian_bd_onesided), 0, civilian_bd_onesided),
  #   combatant_bd_onesided = ifelse(is.na(combatant_bd_onesided), 0, combatant_bd_onesided),
  #   onesided_bd_total = ifelse(is.na(onesided_bd_total), 0, onesided_bd_total),
  #   
  #   # Disaster variables
  #   disaster_events = ifelse(is.na(disaster_events), 0, disaster_events),
  #   total_deaths = ifelse(is.na(total_deaths), 0, total_deaths),
  #   total_affected = ifelse(is.na(total_affected), 0, total_affected),
  #   no_homeless = ifelse(is.na(no_homeless), 0, no_homeless),
  #   total_damage_000_us = ifelse(is.na(total_damage_000_us), 0, total_damage_000_us),
  #   
  #   # OCHA variables - Add this section
  #   total_displaced_hh = ifelse(is.na(total_displaced_hh), 0, total_displaced_hh),
  #   total_displaced_person = ifelse(is.na(total_displaced_person), 0, total_displaced_person),
  #   distinct_destination = ifelse(is.na(distinct_destination), 0, distinct_destination)
  # ) %>%
  #filter(name %in% ocha_zones)


# -----------------------------
# OCHA Name Mapping and Processing
# -----------------------------

# Create year_month encoded
final_data <- final_data %>%
  mutate(
    year_month = sprintf("%04d-%02d", year, month),
    year_month = zoo::as.yearmon(year_month)) |> 
  filter(year_month > "2022-06") |> 
  mutate(
    year_month_encoded = match(year_month, sort(unique(year_month)))
  )

# Create disaster treatment variable
disaster_treatment <- final_data %>%
  select(name, year_month_encoded, disaster_events) %>%
  filter(disaster_events != 0) %>%
  arrange(name, year_month_encoded) %>%
  group_by(name) %>%
  summarise(disaster_treat := first(year_month_encoded), .groups = 'drop')

# Add treatment to final data
final_data <- final_data %>%
  left_join(disaster_treatment, by = "name") %>%
  mutate(
    disaster_treat = ifelse(is.na(disaster_treat), 0, disaster_treat),
    disaster_bin = as.numeric(year_month_encoded >= disaster_treat & disaster_treat > 0)
  )

final_data <- final_data %>%
  mutate(
    name_encoded = as.numeric(as.factor(name))
  )


write_rds(final_data, paste0("../data/merged_data_", today(), ".Rds"))


### SPLIT BY DISASTER EVENT PERHAPS