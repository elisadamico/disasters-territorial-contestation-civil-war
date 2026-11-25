# Spatial join: map IDU events × locations to GADM ADM1 / ADM2 / ADM3
#
# Strategy: locations_coordinates / locations_name / locations_accuracy are
# aligned semicolon-separated parallel arrays — one element per affected
# location per event. This script explodes them into long format (one row per
# location), joins each individual coordinate to GADM, and outputs the result.
#
# Outputs:
#   idmc_africa_idu_2010_2024_adm_long.csv — one row per location per event
#
# figure_original  : the raw displacement figure (same for every location row
#                    of a given event — do NOT sum across locations of one event)
# figure_allocated : figure_original / n_valid_locations for that event
#                    (sums correctly across all location rows of one event)
#
# Diagnostic objects (inspect in R after running):
#   exceptions_no_locations    — events with no parseable coordinate string
#   exceptions_length_mismatch — events where the three parallel arrays differ in length
#   exceptions_bad_coords      — location elements whose coordinate string failed to parse
#   exceptions_invalid_coords  — parsed but geometrically invalid (null island / out of range)
#   exceptions_unknown_acc     — accuracy strings outside the known 5-value vocabulary
#   exceptions_unmatched_join  — location rows with no ADM1 after nearest-feature fallback

library(sf)
library(dplyr)
library(readr)
library(purrr)

# ── 1. Load ────────────────────────────────────────────────────────────────────

idu <- read_csv("idmc_africa_idu_2010_2024.csv", show_col_types = FALSE)
message(sprintf("Loaded %d events", nrow(idu)))

# ── 2. Parse parallel arrays → long format ─────────────────────────────────────

exceptions_no_locations    <- list()
exceptions_length_mismatch <- list()
exceptions_bad_coords      <- list()
exceptions_invalid_coords  <- list()
exceptions_unknown_acc     <- list()

split_semi <- function(x) {
  if (is.na(x) || trimws(x) %in% c("", "NA", "null")) return(character(0))
  trimws(strsplit(x, ";", fixed = TRUE)[[1]])
}

# Format confirmed as "lat, lon" across all single-location rows (no mismatches).
parse_coord_pair <- function(s) {
  nums <- as.numeric(regmatches(s, gregexpr("[-+]?[0-9]*\\.?[0-9]+", s))[[1]])
  if (length(nums) >= 2) return(nums[1:2])   # [lat, lon]
  c(NA_real_, NA_real_)
}

is_valid_coord <- function(lat, lon) {
  !is.na(lat) && !is.na(lon) &&
  !(lat == 0 && lon == 0) &&
  abs(lat) <= 90 && abs(lon) <= 180
}

# Five exhaustive accuracy tokens found in the data.
acc_rank_vocab <- c(
  "Country/territory (AM0)"                = 0L,
  "State/Region/Province (ADM1)"           = 1L,
  "District/Zone/Department (ADM2)"        = 2L,
  "County/City/town/Village/Woreda (ADM3)" = 3L,
  "Point"                                  = 4L
)

long_list <- vector("list", nrow(idu))

for (i in seq_len(nrow(idu))) {
  row <- idu[i, , drop = FALSE]
  eid <- row$id

  coord_strs <- split_semi(row$locations_coordinates)
  name_strs  <- split_semi(row$locations_name)
  acc_strs   <- split_semi(row$locations_accuracy)

  n_c <- length(coord_strs)
  n_n <- length(name_strs)
  n_a <- length(acc_strs)

  # No coordinate string at all
  if (n_c == 0) {
    exceptions_no_locations[[length(exceptions_no_locations) + 1L]] <- list(
      id         = eid,
      reason     = "no_coordinate_string",
      raw_coords = row$locations_coordinates,
      raw_names  = row$locations_name,
      raw_accs   = row$locations_accuracy
    )
    next
  }

  # Parallel arrays misaligned
  if (n_c != n_n || n_c != n_a) {
    exceptions_length_mismatch[[length(exceptions_length_mismatch) + 1L]] <- list(
      id         = eid,
      n_coords   = n_c, n_names = n_n, n_accs = n_a,
      raw_coords = row$locations_coordinates,
      raw_names  = row$locations_name,
      raw_accs   = row$locations_accuracy
    )
    # Proceed with the shortest aligned prefix
    n_use      <- min(n_c, n_n, n_a)
    coord_strs <- coord_strs[seq_len(n_use)]
    name_strs  <- name_strs[seq_len(n_use)]
    acc_strs   <- acc_strs[seq_len(n_use)]
  } else {
    n_use <- n_c
  }

  loc_list <- vector("list", n_use)

  for (j in seq_len(n_use)) {

    parsed <- parse_coord_pair(coord_strs[j])
    lat <- parsed[1];  lon <- parsed[2]

    if (is.na(lat) || is.na(lon)) {
      exceptions_bad_coords[[length(exceptions_bad_coords) + 1L]] <- list(
        id             = eid,
        location_index = j - 1L,
        raw_coord      = coord_strs[j]
      )
      next
    }

    if (!is_valid_coord(lat, lon)) {
      exceptions_invalid_coords[[length(exceptions_invalid_coords) + 1L]] <- list(
        id             = eid,
        location_index = j - 1L,
        lat = lat, lon = lon,
        reason = if (lat == 0 && lon == 0) "null_island" else "out_of_range"
      )
      next
    }

    acc  <- acc_strs[j]
    rank <- unname(acc_rank_vocab[acc])

    if (is.na(rank)) {
      exceptions_unknown_acc[[length(exceptions_unknown_acc) + 1L]] <- list(
        id             = eid,
        location_index = j - 1L,
        acc_string     = acc
      )
      # Still keep the location row; acc_rank will be NA
    }

    loc_list[[j]] <- tibble(
      id                = eid,
      location_index    = j - 1L,
      location_name     = name_strs[j],
      location_accuracy = acc,
      acc_rank          = rank,
      location_lat      = lat,
      location_lon      = lon
    )
  }

  loc_df <- bind_rows(compact(loc_list))

  if (nrow(loc_df) == 0) {
    exceptions_no_locations[[length(exceptions_no_locations) + 1L]] <- list(
      id         = eid,
      reason     = "all_coords_invalid",
      raw_coords = row$locations_coordinates
    )
    next
  }

  # Allocate figure evenly across the valid locations of this event
  n_valid                 <- nrow(loc_df)
  loc_df$figure_original  <- row$figure
  loc_df$figure_allocated <- row$figure / n_valid

  long_list[[i]] <- loc_df
}

long_base <- bind_rows(compact(long_list))

message(sprintf(
  "Long table: %d location rows from %d events",
  nrow(long_base), n_distinct(long_base$id)
))
message(sprintf(
  "Parse exceptions — no_locations: %d | length_mismatch: %d | bad_coords: %d | invalid_coords: %d | unknown_acc: %d",
  length(exceptions_no_locations), length(exceptions_length_mismatch),
  length(exceptions_bad_coords),   length(exceptions_invalid_coords),
  length(exceptions_unknown_acc)
))

# ── 3. Attach event-level metadata ─────────────────────────────────────────────
# Drop fields that are now superseded by per-location columns or figure columns.
# Rename lat/lon to centroid_* so they're distinguishable from location_lat/lon.

event_meta <- idu |>
  rename(centroid_lat = latitude, centroid_lon = longitude) |>
  select(-figure,                # → figure_original / figure_allocated
         -locations_coordinates, # → location_lat / location_lon
         -locations_name,        # → location_name
         -locations_accuracy)    # → location_accuracy / acc_rank

long_joined <- long_base |>
  left_join(event_meta, by = "id")

# ── 4. Build sf points ─────────────────────────────────────────────────────────

long_sf <- long_joined |>
  st_as_sf(coords = c("location_lon", "location_lat"), crs = 4326, remove = FALSE)

# ── 5. Download / load GADM GeoPackages ────────────────────────────────────────
# One GeoPackage per country contains all ADM levels; cached after first download.

gadm_dir <- "gadm_cache"
dir.create(gadm_dir, showWarnings = FALSE)

iso3_needed <- sort(unique(long_joined$iso3))
message(sprintf(
  "Loading GADM GeoPackages for %d countries (downloaded once, then cached)...",
  length(iso3_needed)
))

load_gadm_gpkg <- function(iso) {
  message("  ", iso, "...")
  gpkg_path <- file.path(gadm_dir, sprintf("gadm41_%s.gpkg", iso))

  if (!file.exists(gpkg_path)) {
    url <- sprintf(
      "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_%s.gpkg", iso
    )
    ok <- tryCatch({
      download.file(url, gpkg_path, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning("Download failed for ", iso, ": ", conditionMessage(e))
      FALSE
    })
    if (!ok) return(NULL)
  }

  layers <- tryCatch(st_layers(gpkg_path)$name, error = function(e) character(0))
  # Layers are named "ADM_ADM_0", "ADM_ADM_1", … in GADM 4.1 GeoPackages.
  adm_layers <- grep("^ADM_ADM_[0-9]+$", layers, value = TRUE)
  if (length(adm_layers) == 0) {
    warning("No ADM layers found in ", gpkg_path, " (available: ", paste(layers, collapse = ", "), ")")
    return(NULL)
  }
  best_layer <- adm_layers[which.max(as.integer(sub("ADM_ADM_", "", adm_layers)))]

  g <- tryCatch(
    st_read(gpkg_path, layer = best_layer, quiet = TRUE),
    error = function(e) {
      warning("Read failed for ", iso, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(g)) return(NULL)

  for (col in c("GID_1","NAME_1","GID_2","NAME_2","GID_3","NAME_3")) {
    if (!col %in% names(g)) g[[col]] <- NA_character_
  }
  g |> select(GID_1, NAME_1, GID_2, NAME_2, GID_3, NAME_3)
}

adm_list <- map(iso3_needed, load_gadm_gpkg)
adm      <- bind_rows(Filter(Negate(is.null), adm_list))
message(sprintf("GADM loaded: %d polygons across %d countries", nrow(adm), length(iso3_needed)))

# ── 6. Spatial join ────────────────────────────────────────────────────────────

message("Joining location points to GADM polygons (st_within)...")
joined <- st_join(long_sf, adm, join = st_within, left = TRUE)

# A point exactly on a shared boundary can match multiple polygons — keep first.
joined <- joined |>
  group_by(id, location_index) |>
  slice(1) |>
  ungroup()

# Nearest-feature fallback for coastal / border points
unmatched_mask <- is.na(joined$NAME_1)
n_unmatched    <- sum(unmatched_mask)

if (n_unmatched > 0) {
  message(sprintf("Snapping %d unmatched locations to nearest polygon...", n_unmatched))
  nearest_idx <- st_nearest_feature(joined[unmatched_mask, ], adm)
  fill_cols   <- c("GID_1","NAME_1","GID_2","NAME_2","GID_3","NAME_3")
  joined[unmatched_mask, fill_cols] <- adm[nearest_idx, fill_cols] |> st_drop_geometry()
}

# Catch anything still unmatched (should not occur; kept as a safeguard)
exceptions_unmatched_join <- joined |>
  st_drop_geometry() |>
  filter(is.na(NAME_1)) |>
  select(id, location_index, location_name, location_accuracy,
         location_lat, location_lon, iso3, country)

if (nrow(exceptions_unmatched_join) > 0)
  warning(sprintf(
    "%d locations unmatched even after nearest-feature fallback — see exceptions_unmatched_join",
    nrow(exceptions_unmatched_join)
  ))

# ── 7. ADM2 / ADM3 coverage diagnostics ───────────────────────────────────────
# Missing NAME_2 / NAME_3 reflects genuine GADM coverage gaps (e.g. Libya has
# no ADM2; DRC and Nigeria have no ADM3), not spatial join failures.

joined_df <- st_drop_geometry(joined)

miss_adm2 <- joined_df |>
  filter(!is.na(NAME_1), is.na(NAME_2) | NAME_2 == "NA") |>
  count(iso3, country, name = "n_missing_adm2")

miss_adm3 <- joined_df |>
  filter(!is.na(NAME_1), is.na(NAME_3) | NAME_3 == "NA") |>
  count(iso3, country, name = "n_missing_adm3")

if (nrow(miss_adm2) > 0) {
  message("\nCountries missing ADM2 (GADM has no ADM2 for these):")
  print(miss_adm2, n = Inf)
}
if (nrow(miss_adm3) > 0) {
  message("\nCountries missing ADM3 (GADM has no ADM3 for these):")
  print(miss_adm3, n = Inf)
}

# ── 8. Tidy and save ───────────────────────────────────────────────────────────

result <- joined |>
  st_drop_geometry() |>
  rename(
    adm1_gid  = GID_1,  adm1_name = NAME_1,
    adm2_gid  = GID_2,  adm2_name = NAME_2,
    adm3_gid  = GID_3,  adm3_name = NAME_3
  ) |>
  select(
    id, location_index,
    location_name, location_accuracy, acc_rank,
    location_lat, location_lon,
    adm1_gid, adm1_name,
    adm2_gid, adm2_name,
    adm3_gid, adm3_name,
    figure_original, figure_allocated,
    everything()   # remaining event-level fields (centroid_lat/lon, iso3, country, …)
  )

write_csv(result, "idmc_africa_idu_2010_2024_adm_long.csv")

