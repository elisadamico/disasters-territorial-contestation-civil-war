# 07_output.R
# Export the assembled GDIS extension in three formats:
#   - CSV (no geometry; centroid_lon / centroid_lat as columns)
#   - GeoPackage (with polygon geometry)
#   - RDS (R-native, full sf object)
# Also writes a QC markdown report.
# Output: output/gdis-extension-2019-2025.*

source("scripts/00_setup.R")
library(sf)

ext <- readRDS(file.path(CACHE_DIR, "gdis_extension_raw.rds"))

# ── CSV (drop geometry, keep centroid columns) ─────────────────────────────────
csv_out <- ext |>
  st_drop_geometry() |>
  select(
    disasterno, id, geo_id, country, iso3, gwno,
    geolocation, level, adm1, adm2, adm3,
    location, historical, hist_country, disastertype,
    centroid_lon, centroid_lat
  )

write_csv(csv_out, file.path(OUTPUT_DIR, "gdis-extension-2019-2025.csv"))
cat("Saved CSV:", nrow(csv_out), "rows\n")

# ── GeoPackage ─────────────────────────────────────────────────────────────────
gpkg_out <- ext |>
  select(
    disasterno, id, geo_id, country, iso3, gwno,
    geolocation, level, adm1, adm2, adm3,
    location, historical, hist_country, disastertype,
    centroid_lon, centroid_lat,
    geom
  )

gpkg_path <- file.path(OUTPUT_DIR, "gdis-extension-2019-2025.gpkg")
st_write(gpkg_out, gpkg_path, layer = "gdis_extension_2019_2025",
         delete_dsn = TRUE, quiet = TRUE)
cat("Saved GeoPackage\n")

# ── RDS ────────────────────────────────────────────────────────────────────────
saveRDS(ext, file.path(OUTPUT_DIR, "gdis-extension-2019-2025.rds"))
cat("Saved RDS\n")

# ── QC report ─────────────────────────────────────────────────────────────────
df <- st_drop_geometry(ext)

n_events    <- n_distinct(df$disasterno)
n_locations <- nrow(df)
pct_geocoded <- round(n_events / n_distinct(readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))$disasterno) * 100, 1)

by_year <- df |>
  mutate(year = str_extract(disasterno, "^[0-9]{4}") |> as.integer()) |>
  count(year, name = "n_locations")

by_type <- df |> count(disastertype, sort = TRUE)
by_level <- df |> count(level, sort = FALSE)

# Compare to GDIS 1960-2018 coverage (~89.5%)
report <- glue::glue("
# QC Report — GDIS Extension 2019–2025
Generated: {Sys.time()}

## Summary
- Events in EM-DAT (2019–2025, eligible types): {n_distinct(readRDS(file.path(CACHE_DIR, 'emdat_clean.rds'))$disasterno)}
- Events geocoded: {n_events} ({pct_geocoded}%)
- Total location rows: {n_locations}

## Locations by year
{paste(capture.output(print(as.data.frame(by_year))), collapse = '\n')}

## Locations by disaster type
{paste(capture.output(print(as.data.frame(by_type))), collapse = '\n')}

## Locations by administrative level
{paste(capture.output(print(as.data.frame(by_level))), collapse = '\n')}

## Notes
- Geometry source: GADM (via geodata R package, version downloaded {Sys.Date()})
- Disaster type mapping follows GDIS codebook (Rosvold & Buhaug 2021)
- disasterno format: YYYY-NNNN-ISO3 (new EM-DAT format post-Feb 2021)
- historical = 0 for all rows (no dissolved states in 2019–2025 scope)
- This dataset is a standalone extension; not appended to GDIS 1960–2018
")

writeLines(report, file.path(OUTPUT_DIR, "qc_report.md"))
cat("Saved QC report\n")
cat("\n--- Quick summary ---\n")
cat("Events geocoded:", n_events, "/",
    n_distinct(readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))$disasterno),
    paste0("(", pct_geocoded, "%)\n"))
cat("Location rows:", n_locations, "\n")
