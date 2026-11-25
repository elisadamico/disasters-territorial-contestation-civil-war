# 06_assemble.R
# Combine Tier 1, Tier 2, and (if present) the filled Tier 3 review CSV.
# Apply GDIS deduplication rule, join GADM geometry and centroids,
# add gwno codes, format all columns to match the GDIS schema.
# Output: data/cache/gdis_extension_raw.rds  (sf object)

source("scripts/00_setup.R")
library(sf)
library(countrycode)

tier1   <- readRDS(file.path(CACHE_DIR, "tier1_coded.rds"))
tier1b_path <- file.path(CACHE_DIR, "tier1b_coded.rds")
tier1b  <- if (file.exists(tier1b_path)) readRDS(tier1b_path) else NULL
tier2   <- readRDS(file.path(CACHE_DIR, "tier2_coded.rds"))
lookup  <- readRDS(file.path(CACHE_DIR, "gadm_lookup.rds"))

if (!is.null(tier1b)) cat("Tier 1b (Nominatim) loaded:", nrow(tier1b), "rows\n")

# ── Optionally load filled Tier 3 ─────────────────────────────────────────────
tier3_path <- file.path(ROOT, "data", "tier3_manual_review_filled.csv")
if (file.exists(tier3_path)) {
  tier3_raw <- read_csv(tier3_path, col_types = cols(.default = "c")) |>
    filter(is.na(drop) | drop != "1")

  # Ensure columns exist regardless of which version of the review CSV was used
  for (col in c("coord_lat", "coord_lon", "adm1_manual", "adm2_manual", "adm3_manual")) {
    if (!col %in% names(tier3_raw)) tier3_raw[[col]] <- NA_character_
  }

  has_coords <- !is.na(tier3_raw$coord_lat)   & tier3_raw$coord_lat   != "" &
                !is.na(tier3_raw$coord_lon)   & tier3_raw$coord_lon   != ""
  has_names  <- !is.na(tier3_raw$adm1_manual) & tier3_raw$adm1_manual != ""
  tier3_raw  <- tier3_raw[has_coords | has_names, ]

  lookup_flat <- st_drop_geometry(lookup)

  # ── Path A: reviewer provided coordinates → point-in-polygon ────────────────
  tier3_coord_rows <- tier3_raw |>
    filter(!is.na(coord_lat) & coord_lat != "" &
           !is.na(coord_lon) & coord_lon != "") |>
    mutate(coord_lat = as.numeric(coord_lat),
           coord_lon = as.numeric(coord_lon))

  if (nrow(tier3_coord_rows) > 0) {
    pts_sf <- tier3_coord_rows |>
      st_as_sf(coords = c("coord_lon", "coord_lat"), crs = 4326, remove = FALSE)

    tier3_coord_coded <- map(unique(tier3_coord_rows$iso3), function(iso) {
      pts   <- pts_sf |> filter(iso3 == iso)
      polys <- lookup  |> filter(iso3 == iso)
      if (nrow(polys) == 0 || nrow(pts) == 0) return(NULL)

      # Try levels from finest to coarsest; concat all, keep most disaggregated
      level_results <- map(c(3L, 2L, 1L), function(lev) {
        sub <- polys |> filter(level == lev)
        if (nrow(sub) == 0) return(NULL)
        joined <- st_join(pts, sub |> select(gid, level, adm1, adm2, adm3),
                          join = st_intersects, left = FALSE)
        if (nrow(joined) == 0) return(NULL)
        st_drop_geometry(joined)
      }) |> compact() |> bind_rows()

      if (nrow(level_results) == 0) return(NULL)

      level_results |>
        group_by(disasterno, location_token) |>
        slice_max(level, n = 1, with_ties = FALSE) |>
        ungroup()
    }) |> compact() |> bind_rows()

    tier3_coord_coded <- tier3_coord_coded |>
      mutate(start_year = as.integer(start_year)) |>
      select(disasterno, iso3, country, disastertype, start_year,
             location_raw, location = location_token, gid, level, adm1, adm2, adm3)

    cat("Tier 3 coordinate-coded rows:", nrow(tier3_coord_coded), "\n")
  } else {
    tier3_coord_coded <- NULL
  }

  # ── Path B: reviewer provided admin names → name lookup ─────────────────────
  tier3_name_rows <- tier3_raw |>
    filter(is.na(coord_lat) | coord_lat == "" | is.na(coord_lon) | coord_lon == "") |>
    filter(!is.na(adm1_manual) & adm1_manual != "")

  tier3_name_coded <- pmap(
    list(
      disasterno   = tier3_name_rows$disasterno,
      iso3         = tier3_name_rows$iso3,
      country      = tier3_name_rows$country,
      disastertype = tier3_name_rows$disastertype,
      start_year   = tier3_name_rows$start_year,
      location_raw = tier3_name_rows$location_raw,
      adm1_m       = tier3_name_rows$adm1_manual,
      adm2_m       = tier3_name_rows$adm2_manual,
      adm3_m       = tier3_name_rows$adm3_manual,
      location_tok = tier3_name_rows$location_token
    ),
    function(disasterno, iso3, country, disastertype, start_year,
             location_raw, adm1_m, adm2_m, adm3_m, location_tok) {

      if (!is.na(adm3_m) && adm3_m != "") {
        lev <- 3L; name_col <- "adm3"; name_val <- adm3_m
      } else if (!is.na(adm2_m) && adm2_m != "") {
        lev <- 2L; name_col <- "adm2"; name_val <- adm2_m
      } else {
        lev <- 1L; name_col <- "adm1"; name_val <- adm1_m
      }

      matched <- lookup_flat |>
        filter(iso3 == !!iso3, level == lev) |>
        filter(str_to_lower(.data[[name_col]]) == str_to_lower(name_val)) |>
        slice(1)

      if (nrow(matched) == 0) {
        message("No GADM match for ", disasterno, " / ", name_val)
        return(NULL)
      }

      tibble(
        disasterno   = disasterno,
        iso3         = iso3,
        country      = country,
        disastertype = disastertype,
        start_year   = as.integer(start_year),
        location_raw = location_raw,
        location     = location_tok,
        gid          = matched$gid,
        level        = lev,
        adm1         = matched$adm1,
        adm2         = matched$adm2,
        adm3         = matched$adm3
      )
    }
  ) |> compact() |> bind_rows()

  cat("Tier 3 name-coded rows:", nrow(tier3_name_coded), "\n")

  tier3_coded <- bind_rows(tier3_coord_coded, tier3_name_coded)
  cat("Tier 3 total coded rows:", nrow(tier3_coded), "\n")

} else {
  message("No filled Tier 3 file found — skipping Tier 3. Run 05 and fill in manually first.")
  tier3_coded <- NULL
}

# ── Bind all tiers ─────────────────────────────────────────────────────────────
# Harmonise columns before binding
harmonise <- function(df) {
  df |>
    mutate(
      across(c(starts_with("adm"), location, gid), as.character),
      level      = as.integer(level),
      start_year = as.integer(start_year)
    ) |>
    select(disasterno, iso3, country, disastertype, start_year,
           location_raw, location, gid, level,
           any_of(c("adm1", "adm2", "adm3")))
}

all_coded <- bind_rows(
  harmonise(tier1),
  if (!is.null(tier1b))     harmonise(tier1b),
  harmonise(tier2),
  if (!is.null(tier3_coded)) harmonise(tier3_coded)
)

cat("Total coded rows before deduplication:", nrow(all_coded), "\n")

# ── GDIS deduplication rule ────────────────────────────────────────────────────
# When multiple rows for the same disaster have overlapping admin units
# (e.g. a level-1 observation and a level-2 observation in the same adm1),
# keep only the most disaggregated (highest level number).
all_coded <- all_coded |>
  group_by(disasterno, gid) |>
  slice_max(level, n = 1, with_ties = FALSE) |>
  ungroup()

# Within a disaster, if a level-2 row shares its adm1 with a level-1 row,
# drop the level-1 row (the level-2 is already more precise for that region).
# Identify level-1 gids that are "covered" by a finer-grained entry in the same disaster.
level1_superseded <- all_coded |>
  filter(level == 1) |>
  semi_join(
    all_coded |> filter(level > 1) |> select(disasterno, adm1),
    by = c("disasterno", "adm1")
  )

all_coded <- all_coded |>
  anti_join(level1_superseded, by = c("disasterno", "gid"))

cat("Total coded rows after deduplication:", nrow(all_coded), "\n")

# ── Join GADM geometry and names ───────────────────────────────────────────────
# For tier1, adm1/adm2/adm3 may already be filled from the lookup in script 03.
# For tier2, adm columns came from the matching step.
# Fill any remaining NAs from the lookup table.
lookup_names <- lookup |>
  select(gid, adm1_lkp = adm1, adm2_lkp = adm2, adm3_lkp = adm3,
         centroid_lon, centroid_lat)

all_coded <- all_coded |>
  left_join(st_drop_geometry(lookup_names), by = "gid") |>
  mutate(
    adm1 = coalesce(adm1, adm1_lkp),
    adm2 = coalesce(adm2, adm2_lkp),
    adm3 = coalesce(adm3, adm3_lkp)
  ) |>
  select(-adm1_lkp, -adm2_lkp, -adm3_lkp)

# Attach geometry
coded_sf <- all_coded |>
  left_join(lookup |> select(gid), by = "gid") |>
  st_as_sf()

# ── Build GDIS-schema columns ──────────────────────────────────────────────────
coded_sf <- coded_sf |>
  mutate(
    # geolocation = most disaggregated name
    geolocation = case_when(
      level == 3 ~ adm3,
      level == 2 ~ adm2,
      TRUE        ~ adm1
    ),

    # gwno via countrycode package
    gwno = countrycode(iso3, origin = "iso3c", destination = "gwn",
                       warn = FALSE),

    # All 2019-2025 countries exist; historical = 0
    historical  = 0L,
    hist_country = NA_character_,

    # Unique sequential id per disaster (same across location rows)
    # Unique sequential geo_id per row
    location_raw = NULL   # drop internal field; `location` is the token
  )

# Assign id: sequential integer per unique disasterno
disno_ids <- coded_sf |>
  st_drop_geometry() |>
  distinct(disasterno) |>
  mutate(id = row_number())

coded_sf <- coded_sf |>
  left_join(disno_ids, by = "disasterno") |>
  mutate(geo_id = row_number())

# Final column selection and ordering
coded_sf <- coded_sf |>
  select(
    disasterno, id, geo_id, country, iso3, gwno,
    geolocation, level, adm1, adm2, adm3,
    location, historical, hist_country, disastertype,
    centroid_lon, centroid_lat,
    geometry
  )

cat("Final rows:", nrow(coded_sf), "\n")
cat("Unique disasters:", n_distinct(coded_sf$disasterno), "\n")
cat("\nLevel distribution:\n")
print(table(coded_sf$level))
cat("\nDisaster type distribution:\n")
print(table(coded_sf$disastertype))

saveRDS(coded_sf, file.path(CACHE_DIR, "gdis_extension_raw.rds"))
cat("Saved: data/cache/gdis_extension_raw.rds\n")
