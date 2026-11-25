# Fetch IDMC displacement data for Africa, 2010-2024
# API docs: https://www.internal-displacement.org/database/api-documentation/

library(httr2)
library(dplyr)
library(tidyr)
library(readr)
library(countrycode)  # install.packages("countrycode") if needed

# ----- Config ----------------------------------------------------------------

IDMC_API_KEY <- trimws(Sys.getenv("IDMC_API_KEY"))  # or assign directly
BASE_URL     <- "https://helix-tools-api.idmcdb.org/external-api"
START_YEAR   <- 2010
END_YEAR     <- 2024

if (nchar(IDMC_API_KEY) == 0) {
  stop(
    "IDMC_API_KEY not set.\n",
    "Option 1: add IDMC_API_KEY=your_key to ~/.Renviron, then restart R.\n",
    "Option 2: assign directly: IDMC_API_KEY <- \"your_key_here\""
  )
}
message(sprintf("Using API key: %s... (%d chars)",
                substr(IDMC_API_KEY, 1, 6), nchar(IDMC_API_KEY)))

# African ISO3 codes via countrycode package
african_iso3 <- countrycode::codelist |>
  filter(continent == "Africa") |>
  pull(iso3c) |>
  na.omit() |>
  unique()

# ----- Helper: paginated GIDD GET --------------------------------------------
# GIDD endpoints return: {count, next, previous, results, last_updated}
# release_environment=RELEASE is required or results come back empty.

idmc_get <- function(endpoint, params = list(), page_size = 500) {
  all_results <- list()
  offset      <- 0

  repeat {
    resp <- request(paste0(BASE_URL, endpoint)) |>
      req_url_query(!!!params,
                    client_id            = IDMC_API_KEY,
                    release_environment  = "RELEASE",
                    format               = "json",
                    limit                = page_size,
                    offset               = offset) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop(sprintf("API error %d on %s:\n%s",
                   resp_status(resp), endpoint, resp_body_string(resp)))
    }

    body    <- resp_body_json(resp, simplifyVector = TRUE)
    results <- body$results

    if (is.null(results) || length(results) == 0) break
    all_results <- c(all_results, list(as.data.frame(results)))

    if (is.null(body$`next`)) break
    offset <- offset + page_size
  }

  bind_rows(all_results)
}

# ----- Helper: IDU dump fetch ------------------------------------------------
# IDU endpoints (idus/all/, idus/last-180-days/) return a 302 redirect to a
# pre-generated flat JSON array on S3. httr2 follows the redirect automatically.

idmc_get_idu <- function(endpoint) {
  resp <- request(paste0(BASE_URL, endpoint)) |>
    req_url_query(client_id = IDMC_API_KEY, format = "json") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop(sprintf("API error %d on %s:\n%s",
                 resp_status(resp), endpoint, resp_body_string(resp)))
  }

  as.data.frame(resp_body_json(resp, simplifyVector = TRUE))
}

# ----- Fetch GIDD data -------------------------------------------------------

conflict_raw <- idmc_get("/gidd/conflicts/",
                         params = list(start_year = START_YEAR, end_year = END_YEAR))

disaster_raw <- idmc_get("/gidd/disasters/",
                         params = list(start_year = START_YEAR, end_year = END_YEAR))

conflict <- conflict_raw |> filter(iso3 %in% african_iso3)
disaster <- disaster_raw |> filter(iso3 %in% african_iso3)

# ----- Fetch IDU data --------------------------------------------------------

idu_raw <- idmc_get_idu("/idus/all/")


idu <- idu_raw |>
  filter(iso3 %in% african_iso3,
         year >= START_YEAR,
         year <= END_YEAR)


# ----- Coverage assessment ---------------------------------------------------

summarise_coverage <- function(df, label, year_col = "year") {
  df <- rename(df, .year = all_of(year_col))

  message("\n=== ", label, " — Temporal coverage ===")
  temp <- df |>
    group_by(year = .year) |>
    summarise(
      n_countries    = n_distinct(iso3),
      total_new_disp = sum(new_displacement, na.rm = TRUE),
      total_stock    = sum(total_displacement, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(year)
  print(temp, n = Inf)

  message("\n=== ", label, " — Spatial coverage ===")
  spat <- df |>
    group_by(iso3, country_name) |>
    summarise(years_with_data = n_distinct(.year), .groups = "drop") |>
    arrange(iso3)
  print(spat, n = Inf)

  missing <- setdiff(african_iso3, unique(df$iso3))
  message(sprintf("African countries with NO %s data: %d", tolower(label), length(missing)))
  if (length(missing) > 0) print(missing)

  list(temporal = temp, spatial = spat)
}

conflict_coverage <- summarise_coverage(conflict, "Conflict")
disaster_coverage <- summarise_coverage(disaster, "Disaster")

# IDU uses 'figure' and 'country' (no country_name)
message("\n=== IDU — Temporal coverage ===")
idu_temporal <- idu |>
  group_by(year, displacement_type) |>
  summarise(
    n_countries   = n_distinct(iso3),
    n_events      = n(),
    total_figures = sum(figure, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, displacement_type)
print(idu_temporal, n = Inf)

message("\n=== IDU — Spatial coverage ===")
idu_spatial <- idu |>
  group_by(iso3, country) |>
  summarise(
    years_with_data = n_distinct(year),
    n_events        = n(),
    .groups = "drop"
  ) |>
  arrange(iso3)
print(idu_spatial, n = Inf)

idu_missing <- setdiff(african_iso3, unique(idu$iso3))
message(sprintf("African countries with NO IDU data: %d", length(idu_missing)))
if (length(idu_missing) > 0) print(idu_missing)

# Country x year presence matrix (GIDD combined)
message("\n=== GIDD combined country x year coverage matrix ===")
coverage_matrix <- bind_rows(
  conflict |> mutate(type = "conflict"),
  disaster |> mutate(type = "disaster")
) |>
  distinct(iso3, year, type) |>
  mutate(present = TRUE) |>
  pivot_wider(
    names_from  = c(type, year),
    values_from = present,
    values_fill = FALSE
  ) |>
  arrange(iso3)
print(coverage_matrix, n = Inf)

# ----- Save ------------------------------------------------------------------

write_csv(conflict,                        "idmc_africa_conflict_2010_2024.csv")
write_csv(disaster,                        "idmc_africa_disaster_2010_2024.csv")
write_csv(idu,                             "idmc_africa_idu_2010_2024.csv")
write_csv(conflict_coverage$temporal,      "idmc_africa_conflict_temporal_coverage.csv")
write_csv(disaster_coverage$temporal,      "idmc_africa_disaster_temporal_coverage.csv")
write_csv(idu_temporal,                    "idmc_africa_idu_temporal_coverage.csv")
write_csv(conflict_coverage$spatial,       "idmc_africa_conflict_spatial_coverage.csv")
write_csv(disaster_coverage$spatial,       "idmc_africa_disaster_spatial_coverage.csv")
write_csv(idu_spatial,                     "idmc_africa_idu_spatial_coverage.csv")
write_csv(coverage_matrix,                 "idmc_africa_coverage_matrix.csv")

message("\nDone. Files saved to working directory.")
