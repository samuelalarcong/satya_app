# ======================================================
# SATYA CARBON -- AUSTRALIA PIPELINE (NGER Registered Corporations)
# DATA PIPELINE: load real, MANDATORY-REPORTING emissions data
# (Australia's National Greenhouse and Energy Reporting scheme, via
# the Clean Energy Regulator), now THREE fiscal years (2021-22,
# 2022-23, 2023-24), clean it, fit a simple per-company forecast,
# export tables for the dashboard.
#
# GENUINELY DIFFERENT from every other regional pipeline so far,
# confirmed by direct inspection, not assumed from the filenames:
#  - THREE YEARS, not one -- this is an upgrade from the original
#    single-year (2023-24-only) version of this pipeline. CER
#    ("Greenhouse and energy information by registered corporation")
#    publishes this same table every fiscal year back to 2008-09;
#    2021-22 and 2022-23 were added here. Same exact column
#    structure confirmed across all 3 files by direct inspection
#    (title row, disclaimer paragraph, "Data as at" row, then the
#    real header at skip = 3, in every year).
#  - COMPANY IDENTITY IS BY "Identifying details" (ABN/ACN), NOT
#    company name. Company names genuinely DRIFT year to year in
#    this source -- confirmed by direct inspection, e.g. "SEVEN
#    GROUP HOLDINGS LIMITED" (2021-22/2022-23) reports the identical
#    ABN as "SGH LIMITED" (2023-24); "HANSON AUSTRALIA (HOLDINGS)
#    PROPRIETARY LIMITED" becomes "HEIDELBERG MATERIALS AUSTRALIA
#    GROUP HOLDINGS PTY LTD". Joining on company name across years
#    would have silently split 11 real companies into fake
#    duplicates. The ABN/ACN (whitespace-normalized) is the stable
#    key; company_id is built from it, and the MOST RECENT year's
#    name is kept as each company's canonical display name.
#  - NOT every company appears in all 3 years -- real churn (new
#    registrations, deregistrations, threshold changes), confirmed
#    by direct inspection: of 485 distinct companies across the 3
#    years, 330 appear in all 3, 66 in exactly 2, 89 in only 1.
#    Only companies with >=2 years of Scope 1 data get a forecast
#    (see Part 3) -- a single-year company still appears in the
#    panel and Company Profile, just with no forecast line, same as
#    this pipeline's original all-single-year behavior.
#  - SECTOR is still the SEPARATE, externally-researched GICS Sector
#    mapping (australia_gics_sector_mapping.csv) -- NGER's own table
#    has no industry classification field in any year. The mapping
#    was researched against the 2023-24 company list specifically,
#    so it's joined by name ONLY against each company's 2023-24-year
#    row (where one exists); the resulting sector is then propagated
#    to that same company's ABN/ACN across its other years, since
#    sector is a property of the company, not of the reporting year.
#    A company that deregistered before 2023-24 (and so was never in
#    the list the mapping was researched against) has no sector --
#    left as "Not classified" rather than guessed.
#  - NO SCOPE 3 AT ALL, in any year. NGER only mandates Scope 1 and
#    Scope 2 reporting -- there is no Scope 3 column to extract, for
#    anyone, in any year's file.
#  - This is MANDATORY regulatory reporting (like US GHGRP / EU
#    EPRTR), not voluntary CDP disclosure like the South Africa
#    source -- confirmed by the source document's own framing
#    ("reported to the Clean Energy Regulator").
# ======================================================

setwd("D:/carbon final")
rm(list = ls())

library(tidyverse)
library(readxl)

# One file per fiscal year -- add more (e.g. 2020-21, 2019-20) here by
# extending this named vector; everything downstream (panel, sector
# join, forecast, aggregations) works over however many years are
# listed, not a hardcoded 3.
australia_paths <- c(
  "2022" = "greenhouse-and-energy-information-registered-corporation-2021-22.xlsx",
  "2023" = "greenhouse-and-energy-information-registered-corporation-2022-23.xlsx",
  "2024" = "greenhouse-and-energy-information-registered-corporation-2023-24.xlsx"
)
australia_sector_path <- "australia_gics_sector_mapping.csv"
# The fiscal year whose company list the GICS sector mapping was
# researched against -- sector is joined against this year's names
# only, then propagated to every other year via entity_key (ABN/ACN).
australia_sector_research_year <- "2024"

# ======================================================
# PART 1 -- LOAD, CLEAN, AND STACK ALL YEARS
# ======================================================

cat("=== PART 1: LOADING AUSTRALIA NGER DATA (", length(australia_paths), "fiscal years ) ===\n")

load_one_year <- function(path, year_label) {
  # skip = 3: every year's file has a title row, a long disclaimer
  # paragraph, and a "Data as at" row before the real header --
  # confirmed by direct inspection of all 3 files, not assumed to
  # carry over from the original single-year version of this script.
  raw <- read_excel(path, sheet = 1, skip = 3)
  raw %>%
    rename(
      company_name = `Organisation name`,
      identifying_details = `Identifying details`,
      scope1 = `Total scope 1 emissions (t CO2-e)`,
      scope2 = `Total scope 2 emissions (t CO2-e)`,
      net_energy_gj = `Net energy consumed (GJ)`
    ) %>%
    mutate(
      scope1 = as.numeric(scope1), scope2 = as.numeric(scope2),
      scope3 = NA_real_,  # genuinely does not exist in this source, any year
      country = "Australia",
      year = as.integer(year_label),
      company_name = trimws(company_name),
      # Stable cross-year identity key -- the ABN/ACN, whitespace
      # normalized. Company NAMES drift year to year in this source
      # (see header notes); this does not.
      entity_key = str_replace_all(trimws(identifying_details), "\\s+", "")
    ) %>%
    filter(!is.na(scope1) | !is.na(scope2)) %>%
    select(country, entity_key, company_name, year, scope1, scope2, scope3, net_energy_gj)
}

au_stacked <- map2(australia_paths, names(australia_paths), ~ load_one_year(.x, .y)) %>%
  bind_rows()

cat("Stacked rows across all years:", nrow(au_stacked), "\n")
cat("Distinct companies (by ABN/ACN) across all years:", n_distinct(au_stacked$entity_key), "\n")
year_counts <- au_stacked %>% distinct(entity_key, year) %>% count(entity_key, name = "n_years")
cat("Companies with 3 years:", sum(year_counts$n_years == length(australia_paths)),
    "| 2 years:", sum(year_counts$n_years == 2), "| 1 year:", sum(year_counts$n_years == 1), "\n")

# Canonical company_id and display name -- the most recent year's name
# is used as the canonical display name for each entity_key (matches
# how the GICS sector mapping was researched, and is simply the most
# current name on record).
entity_canonical <- au_stacked %>%
  arrange(entity_key, desc(year)) %>%
  distinct(entity_key, .keep_all = TRUE) %>%
  select(entity_key, company_name) %>%
  arrange(entity_key) %>%
  mutate(company_id = paste0("AU-", row_number()))

au_stacked <- au_stacked %>%
  select(-company_name) %>%
  left_join(entity_canonical, by = "entity_key")

# ======================================================
# PART 2 -- SECTOR JOIN (GICS mapping, researched against the
# australia_sector_research_year company list, propagated to every
# year of the same company via entity_key) AND BUCKETING
# ======================================================

cat("=== PART 2: JOINING GICS SECTOR AND BUCKETING ===\n")

au_sector_map <- read_csv(australia_sector_path, show_col_types = FALSE) %>%
  transmute(
    company_name_key = toupper(trimws(`Organisation name`)),
    sector = trimws(`GICS Sector`)
  ) %>%
  distinct(company_name_key, .keep_all = TRUE)

research_year_names <- au_stacked %>%
  filter(year == as.integer(australia_sector_research_year)) %>%
  transmute(entity_key, company_name_key = toupper(trimws(company_name))) %>%
  left_join(au_sector_map, by = "company_name_key") %>%
  distinct(entity_key, sector)

n_research_year_rows <- au_stacked %>% filter(year == as.integer(australia_sector_research_year)) %>% nrow()
cat("Companies with a matched GICS sector (via", australia_sector_research_year, "names):",
    sum(!is.na(research_year_names$sector)), "of", n_research_year_rows, "\n")
unmatched_names <- au_stacked %>%
  filter(year == as.integer(australia_sector_research_year)) %>%
  inner_join(research_year_names %>% filter(is.na(sector)), by = "entity_key")
if (nrow(unmatched_names) > 0) {
  cat("NOT matched to the sector mapping (left as \"Not classified\"):\n")
  print(unmatched_names$company_name)
}

# Propagate sector to every year of the same company via entity_key --
# a company only present in 2021-22/2022-23 (deregistered before
# 2023-24) was never in the list the mapping was researched against,
# so genuinely has no sourced sector -- "Not classified", not guessed.
au_with_sector <- au_stacked %>%
  left_join(research_year_names, by = "entity_key") %>%
  mutate(sector = if_else(is.na(sector), "Not classified", sector))

# >=5 companies to get its own bucket -- re-checked against this
# (now larger, multi-year) dataset's own actual sector counts, same
# threshold and reasoning as the original single-year version, not
# assumed to still hold without checking.
sector_counts <- au_with_sector %>% distinct(sector, entity_key) %>% count(sector)
top_sectors <- sector_counts %>% filter(n >= 5) %>% pull(sector)

australia_panel_filtered <- au_with_sector %>%
  mutate(
    sector_bucket = if_else(sector %in% top_sectors, sector, "Other"),
    sector_bucket = factor(sector_bucket),
    country = factor(country)
  ) %>%
  select(country, company_id, company_name, sector, sector_bucket, year, scope1, scope2, scope3, net_energy_gj) %>%
  arrange(company_id, year)

cat("Final panel rows:", nrow(australia_panel_filtered), "\n")

# ======================================================
# PART 3 -- PER-COMPANY LINEAR FORECAST (Scope 1 only -- same
# approach and same reasoning as the Chile pipeline: too few years
# per company (at most 3, often 1-2) to fit a real mixed-effects
# panel model the way US/Asia do; each company instead gets its own
# simple linear trend fit to its own real points, honestly labeled
# as such. Companies with fewer than 2 years of Scope 1 data get no
# forecast at all -- a line cannot be fit through one point, and
# nothing is fabricated to work around that.)
# ======================================================

cat("=== PART 3: FITTING PER-COMPANY LINEAR FORECASTS ===\n")

last_year <- max(australia_panel_filtered$year)
forecast_years <- (last_year + 1):(last_year + 3)

fit_company_linear <- function(df, scope_col) {
  vals <- df[[scope_col]]
  yrs <- df$year
  # group_modify() requires .f to ALWAYS return a data frame -- a plain
  # NULL for the <2-points case throws "The result of `.f` must be a
  # data frame." (confirmed by actually running this). A zero-row
  # tibble with the right columns is the correct "no forecast for this
  # company" signal instead: group_modify keeps it as zero rows for
  # that group, so nothing is fabricated and the group still legally
  # disappears from the final output.
  if (sum(!is.na(vals)) < 2) return(tibble(year = integer(0), p50 = numeric(0)))
  fit <- lm(vals ~ yrs)
  pred <- predict(fit, newdata = data.frame(yrs = forecast_years))
  # Floor at zero -- a naive linear trend can dip below zero for a
  # fast-declining company; emissions can't be negative.
  pred <- pmax(pred, 0)
  tibble(year = forecast_years, p50 = pred)
}

future_pred_australia <- australia_panel_filtered %>%
  group_by(company_id, company_name, country, sector) %>%
  group_modify(~ fit_company_linear(.x, "scope1")) %>%
  ungroup()

cat("Companies with a forecast:", n_distinct(future_pred_australia$company_id),
    "of", n_distinct(australia_panel_filtered$company_id), "\n")
cat("Forecast rows:", nrow(future_pred_australia), "\n")

# ======================================================
# PART 4 -- AGGREGATIONS
# ======================================================

cat("=== PART 4: BUILDING AGGREGATIONS ===\n")

hist_by_country_australia <- australia_panel_filtered %>%
  group_by(country, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), .groups = "drop")

# Now a real 3-year (per available year) sector trend, not a
# single-year snapshot -- small sectors (<3 companies in a given
# year) filtered out so no benchmark accidentally republishes one
# company's own number, same convention as Asia/Africa.
hist_by_sector_australia <- australia_panel_filtered %>%
  group_by(sector, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), n_companies = n_distinct(company_id), .groups = "drop") %>%
  filter(n_companies >= 3)

company_lookup_australia <- australia_panel_filtered %>%
  distinct(company_id, company_name, country, sector, sector_bucket) %>%
  arrange(company_name)

settings_australia <- list(
  country_list = "Australia",
  sector_list = sort(unique(as.character(australia_panel_filtered$sector))),
  sector_bucket_list = sort(unique(as.character(australia_panel_filtered$sector_bucket))),
  data_years = sort(unique(australia_panel_filtered$year)),
  last_hist_year = last_year,
  has_forecast = TRUE,    # per-company linear trend (Scope 1 only), not a mixed model -- see header notes
  has_sector_data = TRUE, # externally-researched GICS Sector, joined in above -- Industry benchmark/Target available
  has_scope3 = FALSE      # explicit flag -- NGER doesn't report Scope 3 for any company, any year
)

# ======================================================
# PART 5 -- EXPORT
# ======================================================

cat("=== PART 5: EXPORTING ===\n")

out_dir <- "shiny_data_australia"
if (!dir.exists(out_dir)) dir.create(out_dir)

saveRDS(australia_panel_filtered,   file.path(out_dir, "australia_panel_filtered.rds"))
saveRDS(future_pred_australia,      file.path(out_dir, "future_pred_australia.rds"))
saveRDS(hist_by_country_australia,  file.path(out_dir, "hist_by_country_australia.rds"))
saveRDS(hist_by_sector_australia,   file.path(out_dir, "hist_by_sector_australia.rds"))
saveRDS(company_lookup_australia,   file.path(out_dir, "company_lookup_australia.rds"))
saveRDS(settings_australia,         file.path(out_dir, "settings_australia.rds"))

cat("Done. Files written to", out_dir, "\n")
cat("NOTE: future_pred_australia.rds now exists -- per-company linear forecast, Scope 1 only,",
    "for companies with >=2 years of data.\n")
