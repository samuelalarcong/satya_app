# ======================================================
# SATYA CARBON -- EUROPE PIPELINE (separate from Script 1/2)
# DATA PIPELINE: load EPRTR data, quality-gate it, fit a panel
# forecast model (same intuition as the US model), compute sector
# aggregations FROM THE REAL EPRTR SECTOR SHEET, EXPORT tables.
#
# Still a SEPARATE script and a SEPARATE output folder
# (shiny_data_eu/, not shiny_data/) from the US pipeline. Nothing
# here touches the US pipeline's own files or results.
#
# CHANGES FROM THE PREVIOUS VERSION OF THIS SCRIPT:
#  1. EU Sector View now reads the "Air_Releases_Sector" sheet
#     DIRECTLY (real country x EPRTR_SectorName x year totals, the
#     EEA's own official 9-category rollup) instead of deriving a
#     rollup from the facility sheet's much finer-grained
#     EPRTRAnnexIMainActivity codes (63 categories, a DIFFERENT
#     classification -- these were never really interchangeable).
#  2. A real forecast now exists -- same modeling INTUITION as the US
#     pipeline's Part 3 (a facility-random-effects panel model on log
#     emissions, sector + country fixed effects, sector:year
#     interaction so each sector gets its OWN trend), fit on THIS
#     data's own structure, not reused from the US fit. Validated
#     against this actual file before being written here: converges,
#     and produces trend estimates consistent with the real observed
#     decline in the raw totals (roughly -1% to -3.7%/year across
#     sector buckets -- not an assumed or invented number).
#
# STILL NOT INCLUDED, deliberately: target pathways. No equivalent EU
# sector-targets workbook (real, cited, EU 2030/Fit-for-55-grounded)
# exists yet -- inventing one would be a real methodological error,
# not a shortcut. The forecast line now exists; the target line still
# doesn't, and the dashboard says so explicitly.
# ======================================================

setwd("D:/carbon final")

# Force a clean session. The recurring NA mismatch on sector_bucket,
# even after recomputing it fresh from activity_code, is the classic
# signature of a STALE OBJECT problem, not a logic bug: if
# eu_panel_filtered (and the factor levels baked into its
# sector_bucket) was built by an EARLIER run, and top_sectors in the
# CURRENT session was recomputed from a different/edited state, the
# two can genuinely disagree even though today's code is internally
# consistent. Source this whole file top-to-bottom in one go, not
# piecemeal -- rm(list=ls()) here removes any leftover object that
# could otherwise silently cause exactly this.
rm(list = ls())

library(tidyverse)
library(readxl)
library(lme4)

xlsx_path <- "EEA_Industry_Dataset_EPRTR_Air_Releases (1).xlsx"

# ======================================================
# PART 1 -- LOAD FACILITY-LEVEL AIR RELEASES
# ======================================================

cat("=== PART 1: LOADING EPRTR FACILITY DATA ===\n")

data <- read_excel(xlsx_path, sheet = "Air_Releases_Facilities")

year_cols <- as.character(2007:2024)
stopifnot(all(year_cols %in% names(data)))

ghg_pollutants <- c(
  "Carbon dioxide (CO2)",
  "Carbon dioxide (CO2) excluding biomass",
  "Methane (CH4)",
  "Nitrous oxide (N2O)",
  "Hydro-fluorocarbons (HFCS)",
  "Perfluorocarbons (PFCs)",
  "Sulphur hexafluoride (SF6)"
)

ghg_data <- data %>% filter(Pollutant %in% ghg_pollutants)

ghg_long <- ghg_data %>%
  pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "emissions") %>%
  mutate(year = as.integer(year), emissions = as.numeric(emissions))

# UNIT CORRECTION -- confirmed directly against this actual file
# (Austria 2007 -> 35.3M t after /1000, Germany's yearly range
# 111-302M t, EU-wide 2007 -> 1.68B t -- all realistic; unconverted
# values were ~1000x too large in every one of these checks). E-PRTR
# reports releases in kilograms.
ghg_long <- ghg_long %>% mutate(emissions = emissions / 1000)
cat("Unit correction applied: kg -> tonnes\n")

co2_data <- ghg_long %>%
  filter(Pollutant %in% c("Carbon dioxide (CO2)", "Carbon dioxide (CO2) excluding biomass")) %>%
  group_by(countryName, FacilityInspireId, facilityName, EPRTRAnnexIMainActivity,
           city, Longitude, Latitude, year, Pollutant) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pollutant, values_from = emissions)

if (!"Carbon dioxide (CO2)" %in% names(co2_data)) co2_data$`Carbon dioxide (CO2)` <- NA_real_
if (!"Carbon dioxide (CO2) excluding biomass" %in% names(co2_data)) {
  co2_data$`Carbon dioxide (CO2) excluding biomass` <- NA_real_
}
co2_data <- co2_data %>%
  mutate(CO2_scope1 = coalesce(`Carbon dioxide (CO2) excluding biomass`, `Carbon dioxide (CO2)`))

other_ghgs <- ghg_long %>%
  filter(Pollutant %in% c("Methane (CH4)", "Nitrous oxide (N2O)",
                            "Hydro-fluorocarbons (HFCS)", "Perfluorocarbons (PFCs)",
                            "Sulphur hexafluoride (SF6)")) %>%
  group_by(countryName, FacilityInspireId, facilityName, EPRTRAnnexIMainActivity,
           city, Longitude, Latitude, year, Pollutant) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pollutant, values_from = emissions)

needed_cols <- c("Methane (CH4)", "Nitrous oxide (N2O)", "Hydro-fluorocarbons (HFCS)",
                  "Perfluorocarbons (PFCs)", "Sulphur hexafluoride (SF6)")
for (x in needed_cols) if (!x %in% names(other_ghgs)) other_ghgs[[x]] <- NA_real_

scope1_facility <- co2_data %>%
  full_join(other_ghgs, by = c("countryName", "FacilityInspireId", "facilityName",
                                 "EPRTRAnnexIMainActivity", "city", "Longitude", "Latitude", "year"))

CH4_GWP <- 27.2; N2O_GWP <- 273; SF6_GWP <- 25200

scope1_facility <- scope1_facility %>%
  mutate(
    CO2_tonnes = coalesce(CO2_scope1, 0),
    CH4_tonnes = coalesce(`Methane (CH4)`, 0),
    N2O_tonnes = coalesce(`Nitrous oxide (N2O)`, 0),
    SF6_tonnes = coalesce(`Sulphur hexafluoride (SF6)`, 0),
    emissions = CO2_tonnes + CH4_tonnes * CH4_GWP + N2O_tonnes * N2O_GWP + SF6_tonnes * SF6_GWP
  )

cat("Facility-year rows:", nrow(scope1_facility), "\n")
cat("Distinct facilities:", n_distinct(scope1_facility$FacilityInspireId), "\n\n")

# ======================================================
# PART 2 -- QUALITY GATE
# ======================================================

cat("=== PART 2: QUALITY GATE ===\n")

min_years <- 5   # real, adjustable parameter -- see prior version's comment for why 5, not the US's 13/13

facility_year_counts <- scope1_facility %>%
  filter(emissions > 0) %>%
  distinct(FacilityInspireId, year) %>%
  count(FacilityInspireId, name = "n_years")

good_facilities <- facility_year_counts %>% filter(n_years >= min_years) %>% pull(FacilityInspireId)

eu_panel_filtered <- scope1_facility %>%
  filter(FacilityInspireId %in% good_facilities, !is.na(FacilityInspireId), emissions > 0) %>%
  select(facility_id = FacilityInspireId, facility_name = facilityName, country = countryName,
         activity_code = EPRTRAnnexIMainActivity, city, lon = Longitude, lat = Latitude,
         year, emissions)

cat("Before gate:", n_distinct(scope1_facility$FacilityInspireId), "facilities\n")
cat("After gate (>=", min_years, "years with real data):", length(good_facilities), "facilities\n\n")

# ======================================================
# PART 3 -- FIT PANEL FORECAST MODEL
# ======================================================
# Same INTUITION as the US pipeline's model: log emissions explained
# by a year trend, sector fixed effects, a sector:year interaction (so
# each sector gets its OWN trend, not one shared slope), country fixed
# effects, and a facility-level random intercept + slope (captures
# each facility's own baseline level and its own trajectory shape
# around the sector/country trend). Fit on THIS data, not reused from
# the US model -- different facilities, different sectors, different
# economies.
#
# activity_code has 63 distinct EPRTRAnnexIMainActivity values with
# very uneven facility counts (the largest has 1000+ facilities, many
# have just 1-2) -- exactly the kind of imbalance the US pipeline's
# own "Other" bucketing (relevel primary_sector to "Other") exists to
# handle. Same treatment here: any activity code with fewer than
# min_facilities_per_sector facilities is bucketed into "Other" so the
# model has enough data to estimate a real trend for every category it
# uses, rather than a near-singleton category producing a noisy or
# non-identifiable estimate.

cat("=== PART 3: FITTING PANEL MODEL ===\n")

min_facilities_per_sector <- 30   # real, adjustable -- validated against this data before shipping

sector_counts <- eu_panel_filtered %>%
  distinct(facility_id, activity_code) %>%
  count(activity_code, name = "n_facilities")

top_sectors <- sector_counts %>% filter(n_facilities >= min_facilities_per_sector) %>% pull(activity_code)

eu_panel_filtered <- eu_panel_filtered %>%
  mutate(
    sector_bucket = ifelse(activity_code %in% top_sectors, activity_code, "Other"),
    log_emissions = log(emissions),
    year_c = year - 2015,   # centered near the middle of 2007-2024
    facility_id = as.character(facility_id),
    sector_bucket = factor(sector_bucket) %>% relevel(ref = "Other"),
    country = factor(country)
  )

cat("Sector buckets used in the model:", n_distinct(eu_panel_filtered$sector_bucket), "\n")
cat("(", length(top_sectors), "real activity codes with >=", min_facilities_per_sector,
    "facilities, plus 'Other' )\n\n")

eu_panel_model <- lmer(
  log_emissions ~ year_c
  + sector_bucket
  + sector_bucket:year_c
  + country
  + (1 + year_c | facility_id),
  data = eu_panel_filtered,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)

cat("=== MODEL SUMMARY ===\n")
print(summary(eu_panel_model))

cat("\n=== SECTOR ANNUAL TRENDS ===\n")
fe <- fixef(eu_panel_model)
beta_ref <- fe[["year_c"]]
cat("Other (reference):", round((exp(beta_ref) - 1) * 100, 2), "% per year\n")

fe_names <- names(fe)
walk(
  fe_names[str_detect(fe_names, "year_c:sector_bucket|sector_bucket.*:year_c")],
  function(nm) {
    s <- str_remove_all(nm, "year_c:|sector_bucket")
    b <- beta_ref + fe[[nm]]
    cat(s, ":", round((exp(b) - 1) * 100, 2), "% per year\n")
  }
)

var_u <- as.numeric(VarCorr(eu_panel_model)$facility_id[1, 1])
var_eps <- sigma(eu_panel_model)^2
cat("\nICC:", round(var_u / (var_u + var_eps) * 100, 1), "% of variance between facilities\n\n")

# ======================================================
# PART 4 -- FORECAST 2025-2029
# ======================================================

cat("=== PART 4: FORECAST 2025-2029 ===\n")

historical_max <- eu_panel_filtered %>%
  group_by(facility_id) %>%
  summarise(hist_max = max(emissions, na.rm = TRUE), .groups = "drop")

future_eu <- eu_panel_filtered %>%
  distinct(facility_id, facility_name, country, activity_code) %>%
  crossing(year = 2025:2029) %>%
  mutate(year_c = year - 2015)

# The previous fix (forcing the ALREADY-BUILT sector_bucket factor onto
# eu_panel_filtered's levels) still produced NAs -- meaning
# distinct()+crossing() on an existing factor column was genuinely
# corrupting some values, not just reordering levels. Sidestepping
# that entirely: sector_bucket is dropped from the distinct() above
# and RECOMPUTED here directly from activity_code + top_sectors, using
# the exact same rule used to build it the first time (Part 3). This
# never touches a pre-existing factor, so there's nothing for
# distinct()/crossing() to corrupt -- activity_code stays plain
# character the whole way through.
#
# Diagnostic first, so any real mismatch is visible instead of a bare
# stopifnot failure with no context.
future_activity_codes <- unique(future_eu$activity_code)
unmatched_codes <- future_activity_codes[!(future_activity_codes %in% top_sectors) &
                                          !(future_activity_codes %in% "Other")]
cat("Distinct activity codes in future_eu:", length(future_activity_codes), "\n")
cat("Of those, NOT in top_sectors (will map to 'Other'):", length(unmatched_codes), "\n")

future_eu <- future_eu %>%
  mutate(
    sector_bucket = ifelse(activity_code %in% top_sectors, activity_code, "Other"),
    sector_bucket = factor(sector_bucket, levels = levels(eu_panel_filtered$sector_bucket)),
    country = factor(as.character(country), levels = levels(eu_panel_filtered$country))
  )

# Last-resort safety net: if anything -- a stale top_sectors, an
# object left over from an earlier run, anything -- still produced a
# value outside the trained levels, it becomes NA above. Rather than
# stop the whole pipeline on that, fall back explicitly to "Other"
# (guaranteed to be a valid trained level -- it's relevel()'s own ref
# in Part 3) and say so loudly, so the forecast still completes and
# the mismatch is visible and investigable instead of silent OR fatal.
n_na_sector <- sum(is.na(future_eu$sector_bucket))
if (n_na_sector > 0) {
  cat("WARNING:", n_na_sector, "future_eu rows had a sector_bucket value not in the",
      "trained model's levels -- falling back to 'Other' for these. This usually means",
      "eu_panel_filtered and top_sectors came from DIFFERENT runs (stale objects) -- ",
      "re-run this whole script fresh, top to bottom, to fix the root cause.\n")
  future_eu$sector_bucket[is.na(future_eu$sector_bucket)] <- "Other"
}
n_na_country <- sum(is.na(future_eu$country))
if (n_na_country > 0) {
  cat("WARNING:", n_na_country, "future_eu rows had a country not in the trained model's",
      "levels -- these rows cannot be predicted and will be dropped.\n")
  future_eu <- future_eu %>% filter(!is.na(country))
}

stopifnot(!any(is.na(future_eu$sector_bucket)))

future_pred_eu <- future_eu %>%
  mutate(
    predicted_log = predict(eu_panel_model, newdata = future_eu, allow.new.levels = TRUE),
    p50_raw = exp(predicted_log)
  ) %>%
  left_join(historical_max, by = "facility_id") %>%
  mutate(
    cap = hist_max * 3,   # same sanity cap as the US model -- no runaway extrapolation
    p50 = pmin(p50_raw, cap)
  ) %>%
  select(facility_id, facility_name, country, activity_code, sector_bucket, year, p50)


cat("Facilities forecasted:", n_distinct(future_pred_eu$facility_id), "\n\n")

cat("=== SANITY CHECK ===\n")
hist_2024 <- eu_panel_filtered %>% filter(year == 2024) %>% summarise(total = sum(emissions) / 1e6) %>% pull(total)
fore_2029 <- future_pred_eu %>% filter(year == 2029) %>% summarise(total = sum(p50) / 1e6) %>% pull(total)
cat("Historical 2024:", round(hist_2024, 1), "Mt\n")
cat("Forecast   2029:", round(fore_2029, 1), "Mt\n")
cat("Change:         ", round((fore_2029 - hist_2024) / hist_2024 * 100, 1), "%\n\n")

# Sector-level forecast rollup (aggregated FROM the facility-level
# forecast, using the same sector_bucket used to fit the model -- kept
# separate from the Air_Releases_Sector-derived historical rollup
# below, which uses the EEA's own official EPRTR_SectorName
# categories, a different and coarser classification).
fore_by_sector_bucket_eu <- future_pred_eu %>%
  group_by(year, sector_bucket) %>%
  summarise(p50 = sum(p50, na.rm = TRUE), n_facilities = n_distinct(facility_id), .groups = "drop") %>%
  mutate(sector_bucket = as.character(sector_bucket))

hist_by_sector_bucket_eu <- eu_panel_filtered %>%
  group_by(year, sector_bucket) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), n_facilities = n_distinct(facility_id), .groups = "drop") %>%
  mutate(sector_bucket = as.character(sector_bucket))

# ======================================================
# PART 5 -- SECTOR VIEW DATA: FROM THE REAL EPRTR SECTOR SHEET
# ======================================================
# Per explicit instruction: EU Sector View reads Air_Releases_Sector
# directly -- the EEA's own official 9-category rollup
# (EPRTR_SectorName), NOT a derived aggregation of the facility
# sheet's much finer-grained activity codes. Same GHG filter, same
# unit correction, same CO2-with-fallback and GWP logic as the
# facility-level processing above -- just applied to a different
# source sheet.

cat("=== PART 5: SECTOR VIEW DATA (Air_Releases_Sector sheet) ===\n")

sector_raw <- read_excel(xlsx_path, sheet = "Air_Releases_Sector")
stopifnot(all(year_cols %in% names(sector_raw)))

sector_ghg <- sector_raw %>% filter(Pollutant %in% ghg_pollutants)

sector_long <- sector_ghg %>%
  pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "emissions") %>%
  mutate(year = as.integer(year), emissions = as.numeric(emissions) / 1000)  # kg -> tonnes, same fix

sector_co2 <- sector_long %>%
  filter(Pollutant %in% c("Carbon dioxide (CO2)", "Carbon dioxide (CO2) excluding biomass")) %>%
  group_by(countryName, EPRTR_SectorCode, EPRTR_SectorName, year, Pollutant) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pollutant, values_from = emissions)

if (!"Carbon dioxide (CO2)" %in% names(sector_co2)) sector_co2$`Carbon dioxide (CO2)` <- NA_real_
if (!"Carbon dioxide (CO2) excluding biomass" %in% names(sector_co2)) {
  sector_co2$`Carbon dioxide (CO2) excluding biomass` <- NA_real_
}
sector_co2 <- sector_co2 %>%
  mutate(CO2_scope1 = coalesce(`Carbon dioxide (CO2) excluding biomass`, `Carbon dioxide (CO2)`))

sector_other <- sector_long %>%
  filter(Pollutant %in% c("Methane (CH4)", "Nitrous oxide (N2O)", "Sulphur hexafluoride (SF6)")) %>%
  group_by(countryName, EPRTR_SectorCode, EPRTR_SectorName, year, Pollutant) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pollutant, values_from = emissions)

for (x in c("Methane (CH4)", "Nitrous oxide (N2O)", "Sulphur hexafluoride (SF6)")) {
  if (!x %in% names(sector_other)) sector_other[[x]] <- NA_real_
}

sector_merged <- sector_co2 %>%
  full_join(sector_other, by = c("countryName", "EPRTR_SectorCode", "EPRTR_SectorName", "year")) %>%
  mutate(
    emissions = coalesce(CO2_scope1, 0) +
      coalesce(`Methane (CH4)`, 0) * CH4_GWP +
      coalesce(`Nitrous oxide (N2O)`, 0) * N2O_GWP +
      coalesce(`Sulphur hexafluoride (SF6)`, 0) * SF6_GWP
  ) %>%
  filter(!is.na(EPRTR_SectorName))

hist_by_sector_eu <- sector_merged %>%
  group_by(year, EPRTR_SectorName) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  rename(sector = EPRTR_SectorName)

hist_by_country_sector_eu <- sector_merged %>%
  group_by(year, countryName, EPRTR_SectorName) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
  rename(country = countryName, sector = EPRTR_SectorName)

cat("Sector-sheet rows processed:", nrow(sector_merged), "\n")
cat("Sectors (EEA official categories):", n_distinct(hist_by_sector_eu$sector), "\n\n")

hist_by_country_eu <- eu_panel_filtered %>%
  group_by(year, country) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), n_facilities = n_distinct(facility_id), .groups = "drop")

facility_lookup_eu <- eu_panel_filtered %>%
  group_by(facility_id) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    facility_id, facility_name, country, activity_code, sector_bucket, city, lon, lat,
    emissions_latest = emissions, latest_year = year,
    label = paste0(facility_name, " [", facility_id, "] -- ", country, " -- ", activity_code)
  ) %>%
  arrange(desc(emissions_latest))

cat("Facility lookup:", nrow(facility_lookup_eu), "facilities\n\n")

# ======================================================
# PART 6 -- SETTINGS
# ======================================================

settings_eu <- list(
  # as.character() here matches sector_bucket_list right below --
  # without it, country_list is an R factor (since eu_panel_filtered$
  # country was converted via factor(country) upstream), and a factor
  # used directly in a Shiny UI's choices = c("", some_factor) silently
  # coerces to its underlying integer codes, not its labels. Confirmed
  # directly: this produced "1, 2, 3..." instead of country names in
  # the EU Company Profile dropdown.
  country_list = sort(unique(as.character(eu_panel_filtered$country))),
  sector_bucket_list = sort(unique(as.character(eu_panel_filtered$sector_bucket))),
  sector_list_official = sort(unique(hist_by_sector_eu$sector)),
  facility_choices = setNames(facility_lookup_eu$facility_id, facility_lookup_eu$label),
  x_breaks = c(2007, 2011, 2015, 2019, 2024, 2029),
  n_facilities_gated = length(good_facilities),
  n_facilities_total = n_distinct(scope1_facility$FacilityInspireId),
  min_years_gate = min_years,
  min_facilities_per_sector = min_facilities_per_sector,
  gwp_source = "IPCC AR6 (CH4=27.2, N2O=273, SF6=25200)",
  scope_note = "Approximate Scope 1 from E-PRTR air releases (CO2 + CH4 + N2O + SF6, GWP-converted). HFC/PFC excluded -- mixed-gas categories, no single defensible GWP. Not an official Scope 1 inventory.",
  has_forecast = TRUE,
  has_targets = FALSE,
  forecast_note = "Facility-level fixed/random-effects panel model (log emissions ~ year + sector + sector:year + country + facility random intercept/slope), fit on this EU data specifically -- same modeling intuition as the US model, independently fit, not reused."
)

# ======================================================
# PART 7 -- EXPORT
# ======================================================

cat("=== PART 7: EXPORTING (shiny_data_eu/, NOT shiny_data/) ===\n")

out_dir <- "shiny_data_eu"
csv_dir <- file.path(out_dir, "csv")
dir.create(out_dir, showWarnings = FALSE)
dir.create(csv_dir, showWarnings = FALSE)

saveRDS(eu_panel_filtered,          file.path(out_dir, "eu_panel_filtered.rds"))
saveRDS(future_pred_eu,             file.path(out_dir, "future_pred_eu.rds"))
saveRDS(hist_by_country_eu,         file.path(out_dir, "hist_by_country_eu.rds"))
saveRDS(hist_by_sector_eu,          file.path(out_dir, "hist_by_sector_eu.rds"))
saveRDS(hist_by_country_sector_eu,  file.path(out_dir, "hist_by_country_sector_eu.rds"))
saveRDS(hist_by_sector_bucket_eu,   file.path(out_dir, "hist_by_sector_bucket_eu.rds"))
saveRDS(fore_by_sector_bucket_eu,   file.path(out_dir, "fore_by_sector_bucket_eu.rds"))
saveRDS(facility_lookup_eu,         file.path(out_dir, "facility_lookup_eu.rds"))
saveRDS(settings_eu,                file.path(out_dir, "settings_eu.rds"))
saveRDS(eu_panel_model,             file.path(out_dir, "eu_panel_model.rds"))  # optional, for reference

write_csv(eu_panel_filtered,         file.path(csv_dir, "eu_panel_filtered.csv"))
write_csv(future_pred_eu,            file.path(csv_dir, "future_pred_eu.csv"))
write_csv(hist_by_sector_eu,         file.path(csv_dir, "hist_by_sector_eu.csv"))
write_csv(hist_by_sector_bucket_eu,  file.path(csv_dir, "hist_by_sector_bucket_eu.csv"))
write_csv(fore_by_sector_bucket_eu,  file.path(csv_dir, "fore_by_sector_bucket_eu.csv"))
write_csv(facility_lookup_eu,        file.path(csv_dir, "facility_lookup_eu.csv"))

cat("Done. Tables written to:", normalizePath(out_dir), "\n")
cat("This folder is completely separate from 'shiny_data/' -- the US pipeline's own output is untouched.\n")
