#https://worldbank.github.io/blackmarbler/
# Load required packages

library(sf)
library(terra)
library(tidyverse)
library(blackmarbler)
library(geodata)
library(exactextractr)
gdal()

# Define your NASA bearer token (replace with your actual token)
# bearer <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImVsaXNhZGFtaWNvOTUiLCJleHAiOjE3NjI1NTk5OTksImlhdCI6MTc1NzM2NjE1OCwiaXNzIjoiaHR0cHM6Ly91cnMuZWFydGhkYXRhLm5hc2EuZ292IiwiaWRlbnRpdHlfcHJvdmlkZXIiOiJlZGxfb3BzIiwiYWNyIjoiZWRsIiwiYXNzdXJhbmNlX2xldmVsIjozfQ.hMMsrS8a6oal1xZGWnzDFjmfIvZEa5dnrjG641yljKJHUjePMr5MFK7EME7K5LDH73TQYG58E01C--ba7rp9MhJaHP3NjQ3QP7UJ7HA6ceBpbQhMmmKOD4FW18d5abmKZM0quhGOOmLawnx7a-de4vatPFJv3VsK8ofj7MQ81cBjt9CcM4PMWJE4fwle0wDW0yHoh0excw1REtH1_MGQlCQMBbPzOM2olZGURKuXpa5RuP7mU2lb0zaF1dE9XcIEO7ICoEyeBlxCwj0I-HbMZScBLF4WbJN99KkPJtOIpITDw_HYycN_03DQ5PtgFOO0_RsaXhNFhOhSscVFV8KqqQ"
# NH's
bearer <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImhhdG5ndXllbiIsImV4cCI6MTc3MjcxMjc5MywiaWF0IjoxNzY3NTI4NzkzLCJpc3MiOiJodHRwczovL3Vycy5lYXJ0aGRhdGEubmFzYS5nb3YiLCJpZGVudGl0eV9wcm92aWRlciI6ImVkbF9vcHMiLCJhY3IiOiJlZGwiLCJhc3N1cmFuY2VfbGV2ZWwiOjN9.qx04GN2fFfTBJARQ-2hwX-7BiTJOxp5Aq97sxRUX1t56tXIttOGmdlkz_Vl7AUzAwT7AMR98hZha9sYW5zD_B6TkCIfBA57fLVs9vOej2eCCJLHy6NinFACt2s1vMs_4_yMiOymxt9ynaOoz0D3mamNV1WSpn9VRSZiwIsPidYgZpq7ZTG2A7VJwH8RbhjOmkrqd1yt48WmcwCJo95mOOoLa8tnyv42pexyrgTKEg7LBdiRI1Hzm8nXb2ZsqdasX5rFzQCMhNXaMNFWSGEP7oPbpdHmbYJRducZphpNbu42M-sXl3H3HPwNXK9C0CgjudR6Wn4CDiVRAFxWpiCQabQ"
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = "../data/raw/DRC")

# Download monthly nighttime lights data for DRC for all of 2012
# This creates a sequence of dates for each month in 2012
monthly_dates <- seq.Date(from = as.Date("2022-01-01"), 
                          to = as.Date("2022-01-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2224 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = seq.Date(from = ymd("2021-01-01"), to = ymd("2022-12-01"), by = "month"),
                              bearer = bearer, 
                              file_dir = "../data/raw/NTL/")

osm_data <- st_read("../data/raw/DRC/osm_rdc_sante_zones_211212.gpkg")

# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2019, osm_data, 'mean')

# Convert to long format with health zone ID and month columns

health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2019.rds")

saveRDS(combined_raster, "../data/raw/NTL/combined_raster.rds")


# 2020
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = tempdir())

# Download monthly nighttime lights data for DRC for all of 2019
# This creates a sequence of dates for each month in 2019
monthly_dates <- seq.Date(from = as.Date("2020-01-01"), 
                          to = as.Date("2020-12-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2020 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = monthly_dates,
                              bearer = bearer)


# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2020, osm_data, 'mean')

# Convert to long format with health zone ID and month columns
library(tidyr)
health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2020.rds")


################################################################################


# 2021
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = tempdir())

# Download monthly nighttime lights data for DRC for all of 2019
# This creates a sequence of dates for each month in 2019
monthly_dates <- seq.Date(from = as.Date("2021-01-01"), 
                          to = as.Date("2021-12-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2021 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = monthly_dates,
                              bearer = bearer)


# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2021, osm_data, 'mean')

# Convert to long format with health zone ID and month columns
library(tidyr)
health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2021.rds")

################################################################################




# 2022
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = tempdir())

# Download monthly nighttime lights data for DRC for all of 2019
# This creates a sequence of dates for each month in 2019
monthly_dates <- seq.Date(from = as.Date("2022-01-01"), 
                          to = as.Date("2022-12-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2022 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = monthly_dates,
                              bearer = bearer)


# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2022, osm_data, 'mean')

# Convert to long format with health zone ID and month columns
library(tidyr)
health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2022.rds")
################################################################################




# 2023
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = tempdir())

# Download monthly nighttime lights data for DRC for all of 2019
# This creates a sequence of dates for each month in 2019
monthly_dates <- seq.Date(from = as.Date("2023-01-01"), 
                          to = as.Date("2023-12-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2023 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = monthly_dates,
                              bearer = bearer)


# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2023, osm_data, 'mean')

# Convert to long format with health zone ID and month columns
library(tidyr)
health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2023.rds")

################################################################################



# 2024
# Get DRC boundary polygon
# This will download the country boundary for Democratic Republic of Congo
roi_sf <- gadm(country = "COD", level = 0, path = tempdir())

# Download monthly nighttime lights data for DRC for all of 2019
# This creates a sequence of dates for each month in 2019
monthly_dates <- seq.Date(from = as.Date("2024-01-01"), 
                          to = as.Date("2024-5-01"), 
                          by = "month")

# Download the data
# This will create a raster with 12 bands (one for each month)
drc_monthly_2024 <- bm_raster(roi_sf = roi_sf,
                              product_id = "VNP46A3",  # Monthly data
                              date = monthly_dates,
                              bearer = bearer)


# This will give you average nighttime light values for each health boundary for each month
health_ntl_values <- exact_extract(drc_monthly_2024, osm_data, 'mean')

# Convert to long format with health zone ID and month columns
health_ntl_long <- health_ntl_values %>%
  bind_cols(osm_data) %>%  # Add health zone info
  pivot_longer(cols = starts_with("mean"), 
               names_to = "month", 
               values_to = "ntl_value")


#Export

saveRDS(health_ntl_long, "../data/raw/NTL/health_ntl_monthly_2024.rds")


