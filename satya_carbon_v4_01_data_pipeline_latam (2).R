# ======================================================
# SATYA CARBON -- LATIN AMERICA PIPELINE (Chile + Brazil)
# DATA PIPELINE: load real company-reported emissions for BOTH Chile
# and Brazil, stack them into one regional panel (a "country" column
# distinguishes them, same idea as how the Europe pipeline treats
# countryName as a column within one EPRTR panel, not as separate
# regions), fit per-company forecasts, compute a real sector rollup
# where one genuinely exists, and export tables for the dashboard.
#
# REPLACES the previous Chile-only pipeline
# (satya_carbon_v4_01_data_pipeline_chile.R). Object/file names are
# kept as *_chile (chile_panel_filtered, future_pred_chile, etc.)
# purely for continuity with the Shiny app's existing chile_rcp_*
# code, which already reads these exact names -- the DATA inside them
# now covers both countries, not just Chile. Brazil was previously a
# placeholder ("Brazil is still synthetic") in the app; this is what
# replaces that placeholder with real data.
#
# GENUINELY DIFFERENT SOURCES, kept honestly distinct rather than
# forced into one shape, confirmed by direct inspection of both files:
#  - CHILE (chile_ghg_emissions.xlsx): 8 companies, exactly 3 years
#    each (2023-2025), complete, no missing values. Each of the 8
#    companies is in its OWN distinct sector (Minería, Forestal,
#    Energía, ...) -- a "sector total" here would just be that one
#    company's own number relabeled, so Chile still gets no sector
#    rollup or Industry benchmark, same reasoning as before, unchanged
#    by adding Brazil.
#  - BRAZIL (Brazil_Companies_Emissions_Data.xlsx): 115 companies,
#    SASB/SICS sector-classified, 2018-2023, RAGGED panel (companies
#    report in different years -- 5 companies have only 1 year, 42
#    have all 6; see the file's own footer note: "Filtered to
#    companies in Brazil jurisdiction where Sector (SICS) is not
#    'Information Not Available' (115 of 874 total Brazil companies
#    qualified)"). Source: Climate Data Utility, scraped via Firecrawl
#    (per the file's own footer -- not CDP, not a national regulator
#    filing, so treated as company-disclosed data of unverified
#    provenance, same caveat any scraped source deserves). SICS
#    sectors have real company counts (Infrastructure=30, Consumer
#    Goods=14, Transportation=13, ... down to Services=3) -- large
#    enough for a genuine sector rollup, unlike Chile.
#  - Brazil's "—" (em dash) means "not disclosed for that year" per
#    the file's own footer -- NOT zero. Handled by coercing to numeric
#    with as.numeric() (which turns "—" into NA, exactly the intended
#    meaning), never by treating it as 0.
#  - Brazil reports Scope 2 TWO ways (Location-Based and Market-Based).
#    Location-based is used as the single "scope2" column here, for
#    consistency with every other region in this app (all of which use
#    location-based Scope 2, e.g. Australia's NGER figure) -- and
#    because Market-Based is missing for 243 of 507 rows (48%) vs. just
#    4 of 507 (0.8%) for Location-Based, so it's also the far more
#    complete of the two. Market-based is NOT carried into the app at
#    all -- introducing a second, differently-missing Scope 2 series
#    used nowhere else in this app would be a one-off inconsistency,
#    not a real feature.
#  - Company identity: Brazil's own "Company ID" column is confirmed
#    (by direct inspection) to be a stable 1:1 key with Company Name --
#    no renaming/drift issue like Australia's NGER had, so no
#    entity_key reconciliation step is needed here.
# ======================================================

setwd("D:/carbon final")
rm(list = ls())

library(tidyverse)
library(readxl)

chile_path  <- "chile_ghg_emissions.xlsx"
brazil_path <- "Brazil_Companies_Emissions_Data.xlsx"

# ======================================================
# PART 1 -- LOAD CHILE (unchanged from the original single-country
# pipeline -- same file, same shape, same reasoning)
# ======================================================

cat("=== PART 1: LOADING CHILE DATA ===\n")

# skip = 3: title row, two blank rows before the real header --
# confirmed by direct inspection.
cl_raw <- read_excel(chile_path, sheet = 1, skip = 3)

chile_clean <- cl_raw %>%
  rename(
    company_name = Empresa, sector = Sector, region = `Región`, year = `Año`,
    scope1 = `Alcance 1 (tCO2e)`, scope2 = `Alcance 2 (tCO2e)`, scope3 = `Alcance 3 (tCO2e)`
  ) %>%
  mutate(
    scope1 = as.numeric(scope1), scope2 = as.numeric(scope2), scope3 = as.numeric(scope3),
    year = as.integer(year),
    country = "Chile",
    company_id = paste0("CL-", as.integer(factor(company_name)))
  ) %>%
  filter(!is.na(scope1) | !is.na(scope2) | !is.na(scope3)) %>%
  select(country, company_id, company_name, sector, region, year, scope1, scope2, scope3)

cat("Chile companies:", n_distinct(chile_clean$company_id), "| rows:", nrow(chile_clean), "\n")

# ======================================================
# PART 2 -- LOAD BRAZIL (new)
# ======================================================

cat("=== PART 2: LOADING BRAZIL DATA ===\n")

br_raw <- read_excel(brazil_path, sheet = "Brazil Emissions Data")

# Drop the trailing footer/notes rows -- confirmed by direct inspection
# that the last 3 rows of this sheet are source-citation text, not
# data, identifiable by a missing Company ID (every real data row has
# one).
br_data_rows <- br_raw %>% filter(!is.na(`Company ID`))
cat("Brazil raw data rows (footer excluded):", nrow(br_data_rows), "\n")

brazil_clean <- br_data_rows %>%
  rename(
    company_name = `Company Name`, sector = `Sector (SICS)`, year = `Reporting Year`,
    scope1_raw = `Scope 1 (tCO2e)`, scope2_raw = `Scope 2 Location-Based (tCO2e)`,
    scope3_raw = `Scope 3 (tCO2e)`
  ) %>%
  mutate(
    # as.numeric() turns the "—" (not-disclosed) cells into NA directly
    # -- exactly the source's own stated meaning, never treated as 0.
    scope1 = as.numeric(scope1_raw), scope2 = as.numeric(scope2_raw), scope3 = as.numeric(scope3_raw),
    year = as.integer(year),
    country = "Brazil",
    company_id = paste0("BR-", as.integer(`Company ID`)),
    region = NA_character_  # Brazil's source has no sub-national region field, unlike Chile's
  ) %>%
  filter(!is.na(scope1) | !is.na(scope2) | !is.na(scope3)) %>%
  select(country, company_id, company_name, sector, region, year, scope1, scope2, scope3)

cat("Brazil companies:", n_distinct(brazil_clean$company_id), "| rows:", nrow(brazil_clean), "\n")

# ======================================================
# PART 3 -- STACK INTO ONE REGIONAL PANEL + SECTOR BUCKETING
# ======================================================

cat("=== PART 3: STACKING AND SECTOR BUCKETING ===\n")

# Chile and Brazil use COMPLETELY DIFFERENT sector taxonomies (Chile:
# 8 ad-hoc Spanish labels, one per company; Brazil: SASB/SICS, a real
# standard with multiple companies per sector). No crosswalk is
# invented between them -- sector stays exactly as each source states
# it, and bucketing is computed once across the stacked data, which
# naturally sorts itself out: every Chilean sector has n=1 company so
# none of them clear the threshold below (all become "Other"), while
# Brazil's real SICS sectors mostly do.
chile_panel_filtered <- bind_rows(chile_clean, brazil_clean) %>%
  arrange(country, company_id, year)

sector_counts <- chile_panel_filtered %>% distinct(sector, company_id) %>% count(sector)
top_sectors <- sector_counts %>% filter(n >= 5) %>% pull(sector)
cat("Sectors clearing the >=5-company bucketing threshold:\n")
print(sector_counts %>% filter(sector %in% top_sectors) %>% arrange(desc(n)))

chile_panel_filtered <- chile_panel_filtered %>%
  mutate(
    sector_bucket = if_else(sector %in% top_sectors, sector, "Other"),
    sector_bucket = factor(sector_bucket),
    country = factor(country)
  ) %>%
  select(country, company_id, company_name, sector, sector_bucket, region, year, scope1, scope2, scope3)

cat("Combined panel -- companies:", n_distinct(chile_panel_filtered$company_id),
    "| rows:", nrow(chile_panel_filtered), "\n")
cat("By country:\n")
print(chile_panel_filtered %>% distinct(country, company_id) %>% count(country))

# ======================================================
# PART 4 -- PER-COMPANY LINEAR FORECAST (Scope 1 only)
# Same approach for BOTH countries -- a simple linear trend fit to
# each company's own real points, same reasoning as before: Chile has
# only 3 years/company at most; Brazil's panel is ragged (1-6 years/
# company) and, while 115 companies is enough that a real mixed-
# effects model COULD be attempted, a plain per-company fit is used
# here for consistency with how every other small/medium-panel region
# in this app (Chile, Australia) is already handled, and because it
# needs no distributional assumptions and degrades gracefully for
# companies with very few years. Companies with fewer than 2 years of
# Scope 1 data get no forecast at all -- a line cannot be fit through
# one point, and nothing is fabricated to work around that.
# ======================================================

cat("=== PART 4: FITTING PER-COMPANY LINEAR FORECASTS ===\n")

last_year <- max(chile_panel_filtered$year)
forecast_years <- (last_year + 1):(last_year + 3)

fit_company_linear <- function(df, scope_col) {
  vals <- df[[scope_col]]
  yrs <- df$year
  # group_modify() requires .f to ALWAYS return a data frame -- a plain
  # NULL for the <2-points case throws "The result of `.f` must be a
  # data frame." (the exact bug hit and fixed in the Australia
  # pipeline). A zero-row tibble is the correct "no forecast for this
  # company" signal instead.
  if (sum(!is.na(vals)) < 2) return(tibble(year = integer(0), p50 = numeric(0)))
  fit <- lm(vals ~ yrs)
  pred <- predict(fit, newdata = data.frame(yrs = forecast_years))
  # Floor at zero -- a naive linear trend can dip below zero for a
  # fast-declining company; emissions can't be negative.
  pred <- pmax(pred, 0)
  tibble(year = forecast_years, p50 = pred)
}

future_pred_chile <- chile_panel_filtered %>%
  group_by(company_id, company_name, country, sector) %>%
  group_modify(~ fit_company_linear(.x, "scope1")) %>%
  ungroup()

cat("Companies with a forecast:", n_distinct(future_pred_chile$company_id),
    "of", n_distinct(chile_panel_filtered$company_id), "\n")

# ======================================================
# PART 5 -- AGGREGATIONS
# ======================================================

cat("=== PART 5: BUILDING AGGREGATIONS ===\n")

hist_by_country_chile <- chile_panel_filtered %>%
  group_by(country, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), .groups = "drop")

# Real sector rollup -- Brazil-only in practice (Chile's sectors never
# clear n>=3 since each is exactly 1 company), same >=3-reporting-
# companies floor used for every other region's hist_by_sector_* table
# (Africa, Asia, Australia) so no sector total ever discloses a single
# company's own figure.
hist_by_sector_chile <- chile_panel_filtered %>%
  group_by(sector, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), n_companies = n_distinct(company_id), .groups = "drop") %>%
  filter(n_companies >= 3)

cat("Sector-rollup rows (n>=3 companies):", nrow(hist_by_sector_chile),
    "across", n_distinct(hist_by_sector_chile$sector), "sectors -- all Brazil, confirmed:\n")
print(chile_panel_filtered %>% filter(sector %in% unique(hist_by_sector_chile$sector)) %>%
        distinct(country, sector) %>% count(country))

company_lookup_chile <- chile_panel_filtered %>%
  distinct(company_id, company_name, country, sector, sector_bucket, region) %>%
  arrange(country, company_name)

settings_chile <- list(
  country_list = sort(unique(as.character(chile_panel_filtered$country))),
  sector_list = sort(unique(as.character(chile_panel_filtered$sector))),
  sector_bucket_list = sort(unique(as.character(chile_panel_filtered$sector_bucket))),
  region_list = sort(unique(na.omit(as.character(chile_panel_filtered$region)))),
  data_years = sort(unique(chile_panel_filtered$year)),
  has_forecast = TRUE,          # per-company linear trend, both countries
  has_sector_data = TRUE,       # sector IS present for every company
  has_sector_benchmark = TRUE,  # NOW true -- Brazil's SICS sectors support a real rollup (Chile's still don't, individually)
  has_scope3 = TRUE
)

# ======================================================
# PART 6 -- EXPORT
# ======================================================

cat("=== PART 6: EXPORTING ===\n")

# New, clean output directory name reflecting the region now covers
# two countries, not just Chile -- replaces shiny_data_latam_chile.
out_dir <- "shiny_data_latam"
if (!dir.exists(out_dir)) dir.create(out_dir)

saveRDS(chile_panel_filtered,  file.path(out_dir, "chile_panel_filtered.rds"))
saveRDS(future_pred_chile,     file.path(out_dir, "future_pred_chile.rds"))
saveRDS(hist_by_country_chile, file.path(out_dir, "hist_by_country_chile.rds"))
saveRDS(hist_by_sector_chile,  file.path(out_dir, "hist_by_sector_chile.rds"))
saveRDS(company_lookup_chile,  file.path(out_dir, "company_lookup_chile.rds"))
saveRDS(settings_chile,        file.path(out_dir, "settings_chile.rds"))

cat("Done. Files written to", out_dir, "\n")
cat("NOTE: object/file names kept as *_chile for continuity with the app's existing code -- the data inside now covers Chile AND Brazil.\n")
