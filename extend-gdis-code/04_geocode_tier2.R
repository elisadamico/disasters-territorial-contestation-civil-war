# 04_geocode_tier2.R
# Tier 2 geocoding: events not covered by Tier 1 (no GADM Admin Units JSON).
# Strategy:
#   A) If Admin Units JSON is filled: use the admin names to look up GADM GIDs
#      via exact / near-exact matching.
#   B) Otherwise: parse the Location string into tokens, fuzzy-match each
#      against GADM admin names (levels 1 then 2) within the same country.
# Events where no confident match is found go to tier3_unresolved.
# Output: data/cache/tier2_coded.rds, data/cache/tier3_unresolved.rds
gc()
source("scripts/00_setup.R")
library(sf)
library(jsonlite)
library(stringdist)

emdat   <- readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))
tier1   <- readRDS(file.path(CACHE_DIR, "tier1_coded.rds"))
lookup  <- readRDS(file.path(CACHE_DIR, "gadm_lookup.rds")) |> st_drop_geometry()

# ── Events not covered by Tier 1 ──────────────────────────────────────────────
coded_disno <- unique(tier1$disasterno)
uncoded     <- emdat |> filter(!disasterno %in% coded_disno)
cat("Tier 2 input events:", nrow(uncoded), "\n")

# ── Helper: match a single name string against GADM for one country ────────────
# Returns: tibble(gid, level, adm1, adm2, adm3, match_score) or NULL
MATCH_THRESHOLD <- 0.15   # max OSA distance (normalised); lower = stricter

match_name_to_gadm <- function(name, iso, lookup) {
  candidates <- lookup |> filter(iso3 == iso)
  if (nrow(candidates) == 0) return(NULL)

  name_clean <- str_to_lower(str_trim(name))

  # Try each level; compare against the most specific name at that level
  try_level <- function(lev, name_col) {
    sub <- candidates |>
      filter(level == lev, !is.na(.data[[name_col]])) |>
      mutate(
        target  = str_to_lower(.data[[name_col]]),
        dist    = stringdist(name_clean, target, method = "osa") / pmax(nchar(target), nchar(name_clean))
      ) |>
      arrange(dist) |>
      slice(1)
    if (nrow(sub) == 0 || sub$dist[1] > MATCH_THRESHOLD) return(NULL)
    sub |> select(gid, level, adm1, adm2, adm3, match_score = dist)
  }

  # Prefer level 2 over level 1 (more precise)
  result <- try_level(2, "adm2") %||% try_level(1, "adm1") %||% try_level(3, "adm3")
  result
}

# ── Path A: Admin Units JSON → extract admin names → match to GADM GID ─────────
parse_admin_names <- function(json_str) {
  tryCatch({
    if (is.na(json_str) || json_str == "") return(character(0))
    parsed <- fromJSON(json_str)
    name_col <- intersect(names(parsed), c("adm3_name", "adm2_name", "adm1_name"))[1]
    if (is.na(name_col)) return(character(0))
    as.character(parsed[[name_col]])
  }, error = function(e) character(0))
}

has_admin_json <- uncoded |>
  filter(!is.na(admin_units), admin_units != "")

path_a_results <- pmap(
  list(
    disasterno   = has_admin_json$disasterno,
    iso3         = has_admin_json$iso3,
    json_str     = has_admin_json$admin_units,
    country      = has_admin_json$country,
    disastertype = has_admin_json$disastertype,
    start_year   = has_admin_json$start_year,
    location_raw = has_admin_json$location_raw
  ),
  function(disasterno, iso3, json_str, country, disastertype, start_year, location_raw) {
    names_vec <- parse_admin_names(json_str)
    if (length(names_vec) == 0) return(NULL)

    rows <- map(names_vec, function(nm) {
      match <- match_name_to_gadm(nm, iso3, lookup)
      if (is.null(match)) return(NULL)
      match |> mutate(
        disasterno   = disasterno,
        iso3         = iso3,
        country      = country,
        disastertype = disastertype,
        start_year   = start_year,
        location_raw = location_raw,
        location     = nm,
        source       = "admin_json"
      )
    }) |> compact()
    if (length(rows) == 0) return(NULL)
    bind_rows(rows)
  }
) |>
  compact() |>
  bind_rows()

cat("Path A coded rows:", nrow(path_a_results), "\n")

# ── Path B: parse Location string → tokens → fuzzy match ─────────────────────
# Remove common geographic suffixes that confuse matching
strip_geo_suffixes <- function(x) {
  x |>
    str_replace_all("(?i)\\b(province|state|district|county|counties|region|department|division|prefecture|governorate|municipality|municipalities)\\b", "") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

tokenise_location <- function(loc_str) {
  # Split on commas, semicolons, " and ", parentheses
  loc_str |>
    str_split("[,;()]| and | & ") |>
    pluck(1) |>
    str_trim() |>
    discard(~ nchar(.x) < 3)
}

no_admin_json <- uncoded |>
  filter(is.na(admin_units) | admin_units == "") |>
  filter(!is.na(location_raw), location_raw != "")

path_b_results <- pmap(
  list(
    disasterno   = no_admin_json$disasterno,
    iso3         = no_admin_json$iso3,
    country      = no_admin_json$country,
    disastertype = no_admin_json$disastertype,
    start_year   = no_admin_json$start_year,
    location_raw = no_admin_json$location_raw
  ),
  function(disasterno, iso3, country, disastertype, start_year, location_raw) {
    tokens <- tokenise_location(location_raw) |> strip_geo_suffixes()
    tokens <- tokens[nchar(tokens) >= 3]
    if (length(tokens) == 0) return(NULL)

    rows <- map(tokens, function(tok) {
      match <- match_name_to_gadm(tok, iso3, lookup)
      if (is.null(match)) return(NULL)
      match |> mutate(
        disasterno   = disasterno,
        iso3         = iso3,
        country      = country,
        disastertype = disastertype,
        start_year   = start_year,
        location_raw = location_raw,
        location     = tok,
        source       = "location_string"
      )
    }) |> compact()
    if (length(rows) == 0) return(NULL)
    bind_rows(rows)
  }
) |>
  compact() |>
  bind_rows()

cat("Path B coded rows:", nrow(path_b_results), "\n")

# ── Combine paths A and B ──────────────────────────────────────────────────────
tier2_all <- bind_rows(path_a_results, path_b_results) |>
  select(disasterno, iso3, country, disastertype, start_year,
         location_raw, location, gid, level, adm1, adm2, adm3, match_score, source)

# Deduplicate: keep highest-precision match per (disasterno, gid)
tier2_coded <- tier2_all |>
  group_by(disasterno, gid) |>
  slice_min(match_score, n = 1, with_ties = FALSE) |>
  ungroup()

cat("Tier 2 unique coded rows:", nrow(tier2_coded), "\n")

# ── Identify unresolved events (no match in either path) ──────────────────────
coded_in_t2 <- unique(tier2_coded$disasterno)
unresolved   <- uncoded |>
  filter(!disasterno %in% coded_in_t2) |>
  select(disasterno, iso3, country, disastertype, start_year, location_raw)

cat("Tier 3 (unresolved):", nrow(unresolved), "events\n")

saveRDS(tier2_coded,  file.path(CACHE_DIR, "tier2_coded.rds"))
saveRDS(unresolved,   file.path(CACHE_DIR, "tier3_unresolved.rds"))
cat("Saved: data/cache/tier2_coded.rds\n")
cat("Saved: data/cache/tier3_unresolved.rds\n")
