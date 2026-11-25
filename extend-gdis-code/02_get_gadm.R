# 02_get_gadm.R
# Download GADM polygons (levels 1–3) for every country present in EM-DAT.
# Builds a flat lookup table: one row per admin unit with GID, names, geometry,
# and centroid. Used by tier-2 string matching and final assembly.
# Output: data/cache/gadm_lookup.rds  (sf object)

source("scripts/00_setup.R")
library(sf)
library(geodata)

emdat <- readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))

iso3_list <- sort(unique(emdat$iso3))
iso3_list <- iso3_list[!is.na(iso3_list)]
cat("Countries to download:", length(iso3_list), "\n")

# ── Helper: download one country-level and return tidy sf tibble ───────────────
get_one <- function(iso, lev) {
  tryCatch({
    sv  <- geodata::gadm(country = iso, level = lev, path = GADM_DIR, version = "latest")
    sf_obj <- sf::st_as_sf(sv) |> sf::st_make_valid()

    if (lev == 1) {
      sf_obj |>
        select(gid = GID_1, adm1 = NAME_1) |>
        mutate(iso3 = iso, level = 1L, adm2 = NA_character_, adm3 = NA_character_)
    } else if (lev == 2) {
      sf_obj |>
        select(gid = GID_2, adm1 = NAME_1, adm2 = NAME_2) |>
        mutate(iso3 = iso, level = 2L, adm3 = NA_character_)
    } else {
      sf_obj |>
        select(gid = GID_3, adm1 = NAME_1, adm2 = NAME_2, adm3 = NAME_3) |>
        mutate(iso3 = iso, level = 3L)
    }
  }, error = function(e) {
    message("  Skipping ", iso, " level ", lev, ": ", conditionMessage(e))
    NULL
  })
}

# ── Download all countries × 3 levels ─────────────────────────────────────────
# pmap over iso × level combinations; cache means each country downloads once.
combos <- expand_grid(iso = iso3_list, lev = 1:3)

lookup_list <- map2(combos$iso, combos$lev, function(iso, lev) {
  cat("  ", iso, "level", lev, "\n")
  get_one(iso, lev)
})

lookup <- bind_rows(lookup_list)

# ── Add centroid columns (lon/lat of polygon centroid) ─────────────────────────
# st_centroid on the polygon geometry; reproject not needed (already WGS84)
suppressWarnings({
  centroids <- sf::st_centroid(lookup)
})
coords <- sf::st_coordinates(centroids)
lookup <- lookup |>
  mutate(
    centroid_lon = coords[, 1],
    centroid_lat = coords[, 2]
  )

cat("Lookup rows:", nrow(lookup), "\n")
cat("Levels:", table(lookup$level), "\n")

saveRDS(lookup, file.path(CACHE_DIR, "gadm_lookup.rds"))
cat("Saved: data/cache/gadm_lookup.rds\n")
