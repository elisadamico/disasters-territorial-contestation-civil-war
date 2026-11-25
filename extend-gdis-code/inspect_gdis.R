suppressMessages(library(sf))
# Get aggregate stats without loading geometry
gdis_agg <- st_read(
  "D:/data/gdis/pend-gdis-1960-2018-disasterlocations.gpkg",
  query = "SELECT MAX(CAST(id AS INTEGER)) as max_id, MAX(geo_id) as max_geo_id, COUNT(*) as n FROM GPKG",
  quiet = TRUE
)
print(gdis_agg)

gdis_types <- st_read(
  "D:/data/gdis/pend-gdis-1960-2018-disasterlocations.gpkg",
  query = "SELECT disastertype, COUNT(*) as n FROM GPKG GROUP BY disastertype",
  quiet = TRUE
)
print(gdis_types)

gdis_level <- st_read(
  "D:/data/gdis/pend-gdis-1960-2018-disasterlocations.gpkg",
  query = "SELECT level, COUNT(*) as n FROM GPKG GROUP BY level",
  quiet = TRUE
)
print(gdis_level)
