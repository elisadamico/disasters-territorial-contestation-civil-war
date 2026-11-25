# 03_geocode_tier1.R
# Tier 1 geocoding: events where EM-DAT already provides GADM GID codes
# in the "GADM Admin Units" JSON field.
# Each entry in the JSON array becomes one location observation.
# Output: data/cache/tier1_coded.rds

source("scripts/00_setup.R")
library(jsonlite)



emdat  <- readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))
lookup <- readRDS(file.path(CACHE_DIR, "gadm_lookup.rds"))

na_gadm <- emdat |> 
  filter(is.na(gadm_admin_units))
#writexl::write_xlsx(na_gadm, "data/emdat_missing_gadm.xlsx")
# ── Keep only events with GADM Admin Units filled ─────────────────────────────
tier1_raw <- emdat |>
  filter(!is.na(gadm_admin_units), gadm_admin_units != "")

cat("Tier 1 events (have GADM JSON):", nrow(tier1_raw), "\n")

# ── Parse GADM Admin Units JSON → one row per GID ─────────────────────────────
# The JSON can mix levels in one array: some rows have gid_1, others gid_2.
# fromJSON() produces a wide data frame with both columns, leaving NAs where a
# level is absent for that row. We coalesce across all three gid/name columns
# so every non-null entry is captured regardless of level.
parse_gadm_json <- function(disasterno, gadm_json) {
  tryCatch({
    parsed <- fromJSON(gadm_json)
    if (is.null(parsed) || !is.data.frame(parsed) || nrow(parsed) == 0) return(NULL)

    # Helper: extract column as character, or NA vector if absent
    g <- function(col) {
      if (col %in% names(parsed)) as.character(parsed[[col]])
      else rep(NA_character_, nrow(parsed))
    }

    tibble(
      disasterno = disasterno,
      gid        = coalesce(g("gid_3"), g("gid_2"), g("gid_1")),
      level      = case_when(
        !is.na(g("gid_3")) ~ 3L,
        !is.na(g("gid_2")) ~ 2L,
        !is.na(g("gid_1")) ~ 1L,
        TRUE                ~ NA_integer_
      ),
      location   = coalesce(g("name_3"), g("name_2"), g("name_1"))
    ) |>
      filter(!is.na(gid))
  }, error = function(e) NULL)
}

tier1_parsed_df <- map2(
  tier1_raw$disasterno,
  tier1_raw$gadm_admin_units,
  parse_gadm_json
) |>
  compact() |>
  bind_rows()

cat("Tier 1 parsed rows:", nrow(tier1_parsed_df), "\n")

# ── Join event metadata ────────────────────────────────────────────────────────
tier1_coded <- tier1_parsed_df |>
  left_join(
    tier1_raw |> select(disasterno, iso3, country, disastertype, start_year, location_raw),
    by = "disasterno"
  ) |>
  # Fill missing location label from GID-derived GADM name (via lookup)
  left_join(
    sf::st_drop_geometry(lookup) |>
      select(gid, adm1_gadm = adm1, adm2_gadm = adm2, adm3_gadm = adm3),
    by = "gid"
  ) |>
  mutate(
    location = case_when(
      !is.na(location) & location != "" ~ location,
      level == 3 ~ adm3_gadm,
      level == 2 ~ adm2_gadm,
      TRUE        ~ adm1_gadm
    )
  ) |>
  select(disasterno, iso3, country, disastertype, start_year,
         location_raw, location, gid, level)

cat("Coverage: unique disasters:", n_distinct(tier1_coded$disasterno), "/", nrow(tier1_raw), "\n")

saveRDS(tier1_coded, file.path(CACHE_DIR, "tier1_coded.rds"))
