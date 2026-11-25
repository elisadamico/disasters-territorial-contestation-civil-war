# 03b_geocode_nominatim.R
# Geocode events missing GADM codes via Nominatim (OpenStreetMap).
# Pipeline:
#   1. Tokenise each raw location string into individual place name tokens
#   2. Query Nominatim once per unique (token, country) pair → lat/lon
#   3. Point-in-polygon join against GADM polygons → GID + admin names
# Nominatim's 1 req/sec policy is respected automatically by tidygeocoder.
# Output: data/cache/tier1b_coded.rds  (same schema as tier1_coded.rds)

source("scripts/00_setup.R")
library(sf)
library(readxl)
library(tidygeocoder)

missing <- read_excel(file.path(ROOT, "data", "emdat_missing_gadm.xlsx"))
lookup  <- readRDS(file.path(CACHE_DIR, "gadm_lookup.rds"))

cat("Events to geocode:", nrow(missing), "\n")

# ── 1. Tokenise location strings ───────────────────────────────────────────────
# Strip geographic type words that clutter the place name but keep directionals
# (North/South/East/West are often part of the actual admin name)
strip_type_words <- function(x) {
  x |>
    str_replace_all(
      "(?i)\\b(district|province|region|division|chiefdom|municipality|
               governorate|department|county|prefecture|emirate|
               village|city area|state)s?\\b",
      " "
    ) |>
    str_replace_all("[.;,()]+$", "") |>
    str_replace_all("\\s+",      " ") |>
    str_trim()
}

tokenise <- function(loc_str) {
  if (is.na(loc_str) || loc_str == "") return(character(0))

  # Pull content inside parentheses as additional candidate tokens
  parens <- str_extract_all(loc_str, "(?<=\\()[^)]+(?=\\))")[[1]]

  # Split the main string after removing parenthetical blocks
  main <- loc_str |>
    str_remove_all("\\([^)]*\\)") |>
    str_split("[,;]| and | & ") |>
    pluck(1)

  c(main, parens) |>
    map_chr(strip_type_words) |>
    str_trim() |>
    discard(~ nchar(.x) < 3) |>
    unique()
}

tokens_tbl <- missing |>
  select(disasterno, iso3, country, disastertype, start_year, location_raw) |>
  mutate(token = map(location_raw, tokenise)) |>
  unnest(token) |>
  filter(!is.na(token), nchar(token) >= 3)

cat("Total tokens:", nrow(tokens_tbl), "\n")

# ── 2. Deduplicate and send to Nominatim ───────────────────────────────────────
# One query per unique (token × country) — avoids re-querying the same place
# that appears across multiple disaster events in the same country.
unique_queries <- tokens_tbl |>
  distinct(iso3, country, token) |>
  mutate(query = paste0(token, ", ", country))

cat("Unique Nominatim queries:", nrow(unique_queries), "\n")
cat("Estimated time at 1 req/sec:", ceiling(nrow(unique_queries) / 60), "min\n\n")

geocoded <- unique_queries |>
  geocode(
    address      = query,
    method       = "osm",
    lat          = "lat",
    long         = "lon",
    full_results = FALSE
  )

n_found <- sum(!is.na(geocoded$lat))
cat("\nNominatim returned coordinates:", n_found, "/", nrow(geocoded), "queries\n")

# ── 3. Point-in-polygon against GADM ──────────────────────────────────────────
geocoded_sf <- geocoded |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# For each country: try levels 2, 1, 3 in that order; keep the most
# disaggregated match per query point.
iso_list <- unique(geocoded_sf$iso3)

gadm_matches <- map(iso_list, function(iso) {
  pts   <- geocoded_sf |> filter(iso3 == iso)
  polys <- lookup      |> filter(iso3 == iso)

  if (nrow(polys) == 0 || nrow(pts) == 0) return(NULL)

  level_results <- map(c(2L, 1L, 3L), function(lev) {
    sub <- polys |> filter(level == lev)
    if (nrow(sub) == 0) return(NULL)

    joined <- st_join(
      pts,
      sub |> select(gid, level, adm1, adm2, adm3),
      join = st_intersects,
      left = FALSE
    )
    if (nrow(joined) == 0) return(NULL)
    st_drop_geometry(joined)
  }) |>
    compact() |>
    bind_rows()

  if (nrow(level_results) == 0) return(NULL)

  # One row per query: keep most disaggregated level
  level_results |>
    group_by(query) |>
    slice_max(level, n = 1, with_ties = FALSE) |>
    ungroup()
}) |>
  compact() |>
  bind_rows()

cat("Points assigned to a GADM unit:", nrow(gadm_matches), "\n")

# ── 4. Join GADM assignments back to the full tokens table ────────────────────
tier1b_coded <- tokens_tbl |>
  left_join(
    gadm_matches |> select(iso3, token, gid, level, adm1, adm2, adm3),
    by = c("iso3", "token")
  ) |>
  filter(!is.na(gid)) |>
  mutate(location = token) |>
  # One row per (disaster × GADM unit): keep most disaggregated
  group_by(disasterno, gid) |>
  slice_max(level, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(disasterno, iso3, country, disastertype, start_year,
         location_raw, location, gid, level, adm1, adm2, adm3)

cat("\nTier 1b coded rows:", nrow(tier1b_coded), "\n")
cat("Unique disasters coded:", n_distinct(tier1b_coded$disasterno),
    "/", nrow(missing), "\n")
cat("Still unresolved:",
    nrow(missing) - n_distinct(tier1b_coded$disasterno), "\n")

saveRDS(tier1b_coded, file.path(CACHE_DIR, "tier1b_coded.rds"))
cat("Saved: data/cache/tier1b_coded.rds\n")
