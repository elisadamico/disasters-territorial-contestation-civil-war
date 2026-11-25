# Disasters, Territorial Contestation, and the Geographic Reallocation of Civil War

Analysis code for the paper of the same name (EITM small-group project, with Dagmar, Nguyen, and Sun). The paper itself is `SunFramedVersion.tex`.

Measures territorial control using ACLED's own territorial-takeover event codes (non-state armed group gains/loses control of territory from government forces), then tests how disasters shift the geography of territorial contestation using the Callaway-Sant'Anna estimator.

Data is not included (EM-DAT, ACLED, GDIS/EM-DAT geocoding, IDMC displacement, GADM boundaries), only the processing and estimation scripts.

## Layout

| path | contents |
|---|---|
| `DRC-code/` | DRC-specific replication, data prep, and Callaway-Sant'Anna / Sun-Abraham estimation |
| `Global-code/` | cross-country version: ACLED processing, monthly panel construction, main models, robustness |
| `Global-code/PSRM-robustness/` | disaster-severity robustness checks for a PSRM revision |
| `extend-gdis-code/` | extends the GDIS disaster dataset using EM-DAT, with multi-tier geocoding (GADM matching, Nominatim, manual review) |
| `migration-idmc-code/` | fetches and spatially joins IDMC displacement data to admin units |
| `temporal_*_analysis.R` | temporal stability / threshold-crossing checks on the main results |
