source("scripts/00_setup.R")
library(jsonlite)
library(readxl)

t1    <- readRDS(file.path(CACHE_DIR, "tier1_coded.rds"))
emdat <- readRDS(file.path(CACHE_DIR, "emdat_clean.rds"))

# ── How many NA gids per disaster? ────────────────────────────────────────────
na_summary <- t1 |>
  group_by(disasterno) |>
  summarise(
    n_total = n(),
    n_na    = sum(is.na(gid)),
    n_ok    = sum(!is.na(gid))
  ) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))

cat("Disasters with at least one NA gid:", nrow(na_summary), "\n")
cat("Total NA gid rows:", sum(na_summary$n_na), "\n\n")
print(head(na_summary, 10))

# ── Inspect raw JSON for the worst offender ────────────────────────────────────
worst <- na_summary$disasterno[1]
cat("\n--- Raw JSON for", worst, "---\n")
raw <- emdat |> filter(disasterno == worst)
cat("GADM Admin Units JSON:\n", raw$gadm_admin_units, "\n\n")
cat("Admin Units JSON:\n", raw$admin_units, "\n\n")

# ── Parse and show what fromJSON returns ───────────────────────────────────────
cat("Parsed GADM JSON (fromJSON):\n")
parsed_gadm <- fromJSON(raw$gadm_admin_units)
print(parsed_gadm)
cat("\nParsed Admin JSON (fromJSON):\n")
if (!is.na(raw$admin_units) && raw$admin_units != "") {
  parsed_admin <- fromJSON(raw$admin_units)
  print(parsed_admin)
}

# ── Check: are null gids within the JSON common? ──────────────────────────────
cat("\n\n--- Checking null gid prevalence across all tier1 events ---\n")
tier1_raw <- emdat |> filter(!is.na(gadm_admin_units), gadm_admin_units != "")

null_gid_counts <- map_dfr(seq_len(nrow(tier1_raw)), function(i) {
  tryCatch({
    p <- fromJSON(tier1_raw$gadm_admin_units[i])
    gid_col <- intersect(names(p), c("gid_3", "gid_2", "gid_1"))[1]
    if (is.na(gid_col)) return(tibble(disasterno = tier1_raw$disasterno[i], n_entries = 0L, n_null = 0L))
    tibble(
      disasterno = tier1_raw$disasterno[i],
      n_entries  = nrow(p),
      n_null     = sum(is.na(p[[gid_col]]))
    )
  }, error = function(e) NULL)
}) |> compact() |> bind_rows()

cat("Events where GADM JSON has at least one null gid:",
    sum(null_gid_counts$n_null > 0), "\n")
cat("Total null gid entries across all events:",
    sum(null_gid_counts$n_null), "\n")
cat("Total non-null gid entries:",
    sum(null_gid_counts$n_entries - null_gid_counts$n_null), "\n")
