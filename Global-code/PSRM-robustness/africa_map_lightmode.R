# Clear environment
rm(list = setdiff(ls(), "ged"))
gc()

# -----------------------------
# Load Libraries
# -----------------------------
library(sf)
library(ggplot2)
library(viridis)
library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(countrycode)
library(scales)

setwd("/Users/elisadamico/Library/CloudStorage/Dropbox/EITM-Small-group/working-folder/Global/code")

YEAR_RANGE <- c(2022, 2024)

# -----------------------------
# Load Data
# -----------------------------

# Load merged data for admin1 risk
df <- read_csv("../data/raw/admin1_country_iso3c_month_merged_climate_conflict_africa_2022.csv") %>%
  mutate(across(c(battles, year_month_encoded, iso3c_adm1_encoded, disasters_all_treat),
                as.numeric)) %>%
  filter(year >= YEAR_RANGE[1], year <= YEAR_RANGE[2])

# Load EMDAT individual disaster events
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

# Load admin1 shapefile
adm1_africa <- st_read("../data/raw/geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp") %>%
  mutate(continent = countrycode(shapeGroup, origin = "iso3c", destination = "continent")) %>%
  filter(continent == "Africa") %>%
  st_make_valid()

# Get admin1 centroids for mapping disasters to locations
adm1_centroids <- adm1_africa %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# -----------------------------
# Calculate Risk and Prepare Map Data
# -----------------------------

# Calculate contestation by admin1 - LOG MEAN GRID RISK
adm1_risk <- df %>%
  group_by(iso3c, adm1_name) %>%
  summarise(
    avg_grid_risk = mean(grid_risk_mean, na.rm = TRUE),
    log_grid_risk = log(mean(grid_risk_mean, na.rm = TRUE) + 0.001),
    .groups = 'drop'
  ) %>%
  mutate(
    scaled_log_risk = rescale(log_grid_risk, to = c(0, 1))
  )

# Join risk to shapefile
adm1_map <- adm1_africa %>%
  left_join(adm1_risk, by = c("shapeGroup" = "iso3c", "shapeName" = "adm1_name"))

# ONE DOT PER LOCATION - Size = number of disasters in that area
disaster_counts_by_location <- emdat %>%
  left_join(
    adm1_centroids %>% 
      st_drop_geometry() %>%
      select(shapeGroup, shapeName, lon, lat) %>%
      group_by(shapeGroup) %>%
      slice(1),
    by = c("iso3c" = "shapeGroup")
  ) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  group_by(iso3c, lon, lat) %>%  # ONE UNIQUE LOCATION
  summarise(n_disasters = n(), .groups = 'drop') %>%  # COUNT all disasters at that location
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# -----------------------------
# Create Map
# -----------------------------

map_africa <- ggplot() +
  # Admin1 polygons colored by logged and scaled grid risk
  geom_sf(data = adm1_map,
          aes(fill = scaled_log_risk),
          color = "grey40",
          size = 0.1) +
  # ONE dot per location, sized by disaster count
  geom_sf(data = disaster_counts_by_location, 
          aes(size = n_disasters),
          shape = 21,
          fill = alpha("cyan", 0.2),
          color = "cyan",
          stroke = 0.5) +
  # Color scale for polygons
  scale_fill_viridis_c(
    option = "plasma",
    name = "Territorial\nContestation",
    na.value = "grey20",
    limits = c(0, 1)
  ) +
  # Size scale for disaster points
  scale_size_continuous(
    name = "Climate Disasters",
    range = c(0.5, 4),
    breaks = c(1, 5, 10, 20)
  ) +
  # Customize legend appearance
  guides(
    fill = guide_colorbar(order = 1, title.position = "top", title.hjust = 0.5),
    size = guide_legend(
      order = 2, 
      title.position = "top", 
      title.hjust = 0.5,
      override.aes = list(fill = alpha("cyan", 0.2), color = "cyan", stroke = 0.5)
    )
  ) +
  # Theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.text = element_text(color = "black", size = 10, family = "serif"),
    legend.title = element_text(color = "black", size = 12, face = "bold", family = "serif"),
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5, family = "serif"),
    plot.caption = element_text(color = "black", size = 8, family = "serif"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    caption = paste0("Note: Data from EM-DAT (", nrow(disaster_counts_by_location), " locations) and Conflict Forecast. Dot size = number of disasters")
  )

print(map_africa)

# Create output directory
output_dir <- "../output/figures_lightmode"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save the light mode map
ggsave(paste0(output_dir, "/africa_contestation_map.png"),
       plot = map_africa,
       width = 12, height = 10,
       dpi = 300,
       bg = "white")

cat("\n=== Map Complete ===\n")
cat("Disaster locations plotted:", nrow(disaster_counts_by_location), "\n")
cat("Admin1 areas mapped:", nrow(adm1_map), "\n")