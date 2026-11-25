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
  ungroup()


# For the .gpkg file (GeoPackage - a spatial data format)
# You'll need the sf package for spatial data
osm_data <- st_read("../data/raw/DRC/osm_rdc_sante_zones_211212.gpkg")

# ggplot(osm_data) +
#   geom_sf() +
#   theme_minimal()

# -----------------------------
# Load and filter GED data with country fixes - DRC ONLY
#-----------------------------
ged_drc <- read_excel("../../Global/data/raw/GEDEvent_v25_1.xlsx") %>%
  clean_names() %>%
  mutate(
    # Convert date_start to a date object and then extract the year and month
    date_start_formatted = as.Date(date_start, format = "%Y-%m-%d"),
    year = lubridate::year(date_start_formatted),
    month = lubridate::month(date_start_formatted)
  ) 

ged_drc <- ged_drc %>%
  filter(year >= 2019) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name")
  ) %>%
  # Filter for DRC specifically
  filter(iso3c == "COD" |
           country %in% c("Democratic Republic of the Congo", "Democratic Republic of Congo", "DR Congo", "DRC", "Congo, Dem. Rep."))


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

emdat_with_coords <- emdat_expanded %>%
  left_join(drc_coords, by = "location")


# Plot the coordinates on the map
# ggplot() +
#   geom_sf(data = osm_data) +
#   geom_point(data = drc_coords, 
#              aes(x = longitude, y = latitude), 
#              color = "red", 
#              size = 2, 
#              alpha = 0.7) +
#   theme_minimal() +
#   labs(title = "DRC EMDAT Disaster Locations", 
#        subtitle = "Red points show disaster-affected locations") +
#   coord_sf(xlim = c(12, 32), ylim = c(-13, 6))

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
year_range <- 2019:2024
month_range <- 1:12

complete_grid <- expand_grid(
  name = all_zones$name,
  year = year_range,
  month = month_range
)

# -----------------------------
# Merge Disaster data onto SF object by lat/lon
# -----------------------------

# Convert drc_disaster to sf object for spatial join
drc_disaster_sf <- emdat_with_coords %>%
  # Clean and prepare the data
  filter(!is.na(longitude.y) & !is.na(latitude.y)) %>%  # Remove rows with missing coordinates
  st_as_sf(coords = c("longitude.y", "latitude.y"), crs = 4326) %>%  # Convert to sf object
  st_transform(st_crs(osm_data))  # Transform to match osm_data CRS

# Spatial join: find which zone each disaster event falls into
disaster_joined <- st_join(drc_disaster_sf, osm_data, join = st_within)

# Convert back to regular dataframe and aggregate by name, year, month
disaster_aggregated <- disaster_joined %>%
  st_drop_geometry() %>%  # Remove geometry column
  # Group by zone name, year, month and count events
  group_by(name, year, month) %>%  # name comes from osm_data zones
  summarise(
    disaster_events = n(),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_affected = sum(no_affected, na.rm = TRUE),
    no_homeless = sum(no_homeless, na.rm = TRUE),
    total_damage_000_us = sum(total_damage_000_us, na.rm = TRUE),
    .groups = 'drop'
  )

# Add this section right after loading your data and before the aid processing

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

# Then update your final merge section to include OCHA data:

# Merge all data together (updated to include OCHA data)
final_data <- complete_grid %>%
  left_join(aid_aggregated, by = c("name")) %>%
  left_join(ged_aggregated, by = c("name", "year", "month")) %>%
  left_join(disaster_aggregated, by = c("name", "year", "month")) %>%
  left_join(ocha_processed, by = c("name", "year", "month")) %>%  # Add this line
  # Replace NA values with 0 for counts - using the correct variable names
  mutate(
    # Aid variables
    aid_unit = ifelse(is.na(aid_unit), 0, aid_unit),
    
    # Conflict variables  
    battles = ifelse(is.na(battles), 0, battles),
    battles_onesided = ifelse(is.na(battles_onesided), 0, battles_onesided),
    civilian_bd_onesided = ifelse(is.na(civilian_bd_onesided), 0, civilian_bd_onesided),
    combatant_bd_onesided = ifelse(is.na(combatant_bd_onesided), 0, combatant_bd_onesided),
    onesided_bd_total = ifelse(is.na(onesided_bd_total), 0, onesided_bd_total),
    
    # Disaster variables
    disaster_events = ifelse(is.na(disaster_events), 0, disaster_events),
    total_deaths = ifelse(is.na(total_deaths), 0, total_deaths),
    total_affected = ifelse(is.na(total_affected), 0, total_affected),
    no_homeless = ifelse(is.na(no_homeless), 0, no_homeless),
    total_damage_000_us = ifelse(is.na(total_damage_000_us), 0, total_damage_000_us),
    
    # OCHA variables - Add this section
    total_displaced_hh = ifelse(is.na(total_displaced_hh), 0, total_displaced_hh),
    total_displaced_person = ifelse(is.na(total_displaced_person), 0, total_displaced_person),
    distinct_destination = ifelse(is.na(distinct_destination), 0, distinct_destination)
  ) %>%
  filter(name %in% ocha_zones)


# -----------------------------
# OCHA Name Mapping and Processing
# -----------------------------

# Create year_month encoded
final_data <- final_data %>%
  mutate(
    year_month = sprintf("%04d-%02d", year, month),
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

#############
#############

# ADDING way to only look for conflict areas of DRC/Eastern

# Just select name and battles, then sum battles by name
battles_per_unit <- ged_aggregated %>%
  group_by(name) %>%
  summarise(total_battles = sum(battles, na.rm = TRUE), .groups = 'drop')

osm_data <- osm_data %>%
  left_join(battles_per_unit, by = "name") %>%
  mutate(total_battles = replace_na(total_battles, 0))

# Get centroids for label placement
osm_centroids <- st_centroid(osm_data)


# ggplot(osm_data) +
#   geom_sf(aes(fill = total_battles)) +
#   geom_sf_text(data = osm_centroids, aes(label = total_battles), size = 3) +
#   scale_fill_viridis_c(name = "Total Battles") +
#   theme_void() +
#   labs(title = "Health Units with Battle Counts in DRC")


high_conflict_units <- battles_per_unit %>%
  mutate(high_conflict = ifelse(total_battles > 29, 1, 0)) %>%
  select(name, high_conflict)

final_data <- final_data %>%
  left_join(battles_per_unit, by = "name")

final_data <- final_data %>%
  left_join(high_conflict_units, by = "name")

#############
#############


#############
#############

# Now, trying mining concessions. 
## 1. get percent of unit with mining coverage
## 2. do whether or count of mining areas hit by disaster. 


# Load mining concessions data
mining_concessions <- st_read("../data/raw/DRC/mining_concessions_drc.shp")
mining_concessions <- st_transform(mining_concessions, crs = st_crs(osm_data))

# Calculate area of each health unit
osm_data$unit_area <- st_area(osm_data)

mining_within <- st_within(mining_concessions, osm_data, sparse = FALSE)
# Count how many mining areas are in each health unit
osm_data$mining_count <- colSums(mining_within)
osm_data$has_mining <- ifelse(osm_data$mining_count > 0, 1, 0)


## if disaster hit mining area...


# Convert emdat to sf points
disaster_points <- st_as_sf(emdat_with_coords, coords = c("longitude.y", "latitude.y"), crs = st_crs(mining_concessions))

# Find which disasters hit mining concessions directly
disasters_in_mining_areas <- st_filter(disaster_points, mining_concessions, .predicate = st_intersects)

# Count disasters that hit mining by time period
mining_disaster_counts <- disasters_in_mining_areas %>%
  st_drop_geometry() %>%
  group_by(year, month) %>%
  summarise(disasters_hitting_mining = n(), .groups = 'drop')

# If you want to link back to health units
disasters_mining_by_health_unit <- st_join(disasters_in_mining_areas, osm_data, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(name, year, month) %>%
  summarise(mining_disasters = n(), .groups = 'drop')




#############
#############



# Merge mining disaster counts back into final_data
final_data <- final_data %>%
  left_join(disasters_mining_by_health_unit, by = c("name", "year", "month")) %>%
  mutate(
    mining_disasters = replace_na(mining_disasters, 0),
    mining_hit_by_disaster = ifelse(mining_disasters > 0, 1, 0)
  )

# Also merge the mining characteristics from osm_data
final_data <- final_data %>%
  left_join(osm_data %>% 
              st_drop_geometry() %>% 
              select(name, mining_count, has_mining), 
            by = "name")

#############
#############

# Now write the updated final dataset
write_csv(final_data, "../data/DRC_final_data.csv")




### SPLIT BY DISASTER EVENT PERHAPS