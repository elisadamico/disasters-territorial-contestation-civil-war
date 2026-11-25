# 00_setup.R
# Shared config, paths, directory creation, and constants.
# Source this at the top of every other script.

library(tidyverse)

# ── Paths ──────────────────────────────────────────────────────────────────────
ROOT       <- "C:/Users/hatng/Dropbox/PC/Documents/Grad School/projects/extend-gdis"
EMDAT_FILE <- list.files(file.path(ROOT, "data"), pattern = "\\.xlsx$", full.names = TRUE)[1]
CACHE_DIR  <- file.path(ROOT, "data", "cache")
GADM_DIR   <- file.path(ROOT, "data", "cache", "gadm")
OUTPUT_DIR <- file.path(ROOT, "output")

for (d in c(CACHE_DIR, GADM_DIR, OUTPUT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ── Geographic scope ──────────────────────────────────────────────────────────
# Set to NULL to include all regions
REGION_FILTER <- "Africa"

# ── Year scope ─────────────────────────────────────────────────────────────────
YEAR_MIN <- 2019L
YEAR_MAX <- 2025L

# ── Disaster type mapping: EM-DAT → GDIS vocabulary ───────────────────────────
# "Mass movement (wet)" subtypes (e.g. Landslide (wet)) → "landslide" in GDIS
# All other EM-DAT types map 1-to-1 (lowercased)
DISASTER_TYPE_MAP <- c(
  "Flood"               = "flood",
  "Storm"               = "storm",
  "Earthquake"          = "earthquake",
  "Extreme temperature" = "extreme temperature",
  "Mass movement (wet)" = "landslide",
  "Mass movement (dry)" = "mass movement (dry)",
  "Drought"             = "drought",
  "Volcanic activity"   = "volcanic activity"
)

KEEP_DISASTER_TYPES <- names(DISASTER_TYPE_MAP)

# ── GDIS output column order (non-spatial; geometry added separately) ──────────
GDIS_COLS <- c(
  "disasterno", "id", "geo_id", "country", "iso3", "gwno",
  "geolocation", "level", "adm1", "adm2", "adm3",
  "location", "historical", "hist_country", "disastertype"
)

