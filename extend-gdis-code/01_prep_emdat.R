# 01_prep_emdat.R
# Read EM-DAT xlsx, filter to scope (region, years, disaster types),
# standardise column names, recode disaster types to GDIS vocabulary.
# Output: data/cache/emdat_clean.rds

source("scripts/00_setup.R")
library(readxl)

# ── Read ───────────────────────────────────────────────────────────────────────
raw <- read_excel(EMDAT_FILE, sheet = "EM-DAT Data", col_types = "text")

# ── Rename to snake_case ───────────────────────────────────────────────────────
emdat <- raw |>
  rename(
    disasterno        = `DisNo.`,
    disaster_type_raw = `Disaster Type`,
    disaster_subtype  = `Disaster Subtype`,
    iso3              = ISO,
    country           = Country,
    region            = Region,
    location_raw      = Location,
    start_year        = `Start Year`,
    gadm_admin_units  = `GADM Admin Units`,
    admin_units       = `Admin Units`
  ) |>
  select(
    disasterno, disaster_type_raw, disaster_subtype,
    iso3, country, region, location_raw, start_year,
    gadm_admin_units, admin_units
  ) |>
  mutate(start_year = as.integer(start_year))

# ── Filter region ──────────────────────────────────────────────────────────────
if (!is.null(REGION_FILTER)) {
  emdat <- emdat |> filter(region == REGION_FILTER)
  cat("Region filter applied:", REGION_FILTER, "—", nrow(emdat), "events\n")
}

# ── Filter years ───────────────────────────────────────────────────────────────
emdat <- emdat |>
  filter(start_year >= YEAR_MIN, start_year <= YEAR_MAX)

# ── Filter disaster types ──────────────────────────────────────────────────────
emdat <- emdat |>
  filter(disaster_type_raw %in% KEEP_DISASTER_TYPES)

# ── Recode to GDIS vocabulary ──────────────────────────────────────────────────
emdat <- emdat |>
  mutate(disastertype = recode(disaster_type_raw, !!!DISASTER_TYPE_MAP))

# ── Drop rows with no location info at all ─────────────────────────────────────
# (no location string AND no pre-coded units — nothing to geocode)
emdat <- emdat |>
  filter(
    !(is.na(location_raw) | location_raw == "") |
    !(is.na(gadm_admin_units) | gadm_admin_units == "")
  )

cat("EM-DAT events after filtering:", nrow(emdat), "\n")
cat("Year range:", min(emdat$start_year), "–", max(emdat$start_year), "\n")
cat("Disaster types:\n")
print(count(emdat, disastertype, sort = TRUE))
cat("Events with GADM Admin Units:", sum(!is.na(emdat$gadm_admin_units) & emdat$gadm_admin_units != ""), "\n")
cat("Events with Admin Units only:", sum(
  (is.na(emdat$gadm_admin_units) | emdat$gadm_admin_units == "") &
  !(is.na(emdat$admin_units) | emdat$admin_units == "")
), "\n")
cat("Events with Location string only:", sum(
  (is.na(emdat$gadm_admin_units) | emdat$gadm_admin_units == "") &
  (is.na(emdat$admin_units) | emdat$admin_units == "") &
  !(is.na(emdat$location_raw) | emdat$location_raw == "")
), "\n")

saveRDS(emdat, file.path(CACHE_DIR, "emdat_clean.rds"))
cat("Saved: data/cache/emdat_clean.rds\n")
