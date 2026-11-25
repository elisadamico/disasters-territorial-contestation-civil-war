# 05_geocode_tier3_export.R
# Export unresolved events from Tier 2 as a CSV for manual review.
# Reviewer can either:
#   (a) fill coord_lat / coord_lon → point-in-polygon lookup in 06_assemble.R, OR
#   (b) fill adm1_manual (+ adm2/adm3 if known) → name-based lookup in 06_assemble.R
# Set drop = 1 to exclude a row entirely.
# After filling, save as data/tier3_manual_review_filled.csv.
# Output: data/tier3_manual_review.csv

source("scripts/00_setup.R")

unresolved <- readRDS(file.path(CACHE_DIR, "tier3_unresolved.rds"))

# ── Expand: one row per location token for events with a location string ───────
tokenise_location <- function(loc_str) {
  if (is.na(loc_str) || loc_str == "") return(NA_character_)
  loc_str |>
    str_split("[,;()]| and | & ") |>
    pluck(1) |>
    str_trim() |>
    discard(~ nchar(.x) < 3) |>
    (\(x) if (length(x) == 0) NA_character_ else x)()
}

review <- unresolved |>
  mutate(location_token = map(location_raw, tokenise_location)) |>
  unnest(location_token) |>
  mutate(
    coord_lat    = "",   # decimal degrees, WGS84 — fill OR fill adm1_manual below
    coord_lon    = "",   # decimal degrees, WGS84
    adm1_manual  = "",   # GADM adm1 name (used only if coords are blank)
    adm2_manual  = "",
    adm3_manual  = "",
    drop         = "",   # set to 1 to exclude this row
    notes        = ""
  ) |>
  select(
    disasterno, country, iso3, start_year, disastertype,
    location_token, location_raw,
    coord_lat, coord_lon,
    adm1_manual, adm2_manual, adm3_manual, drop, notes
  )

out_path <- file.path(ROOT, "data", "tier3_manual_review.csv")
write_csv(review, out_path)

cat("Tier 3 review CSV written:", nrow(review), "rows\n")
cat("Path:", out_path, "\n")
cat("\nInstructions:\n")
cat("  1. Open data/tier3_manual_review.csv\n")
cat("  2. For each row, use ONE of two methods:\n")
cat("       (a) Fill coord_lat + coord_lon (decimal degrees, WGS84) — easiest via Google Maps\n")
cat("       (b) Fill adm1_manual (required), adm2_manual / adm3_manual if known\n")
cat("     Coordinates take priority when both are filled.\n")
cat("  3. Set drop = 1 if the location is too vague or cannot be identified\n")
cat("  4. Save as data/tier3_manual_review_filled.csv\n")
cat("  5. Run 06_assemble.R\n")
