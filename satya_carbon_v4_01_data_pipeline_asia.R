# ======================================================
# SATYA CARBON -- ASIA PIPELINE (separate from Script 1 / EU pipeline)
# DATA PIPELINE: load real company-reported emissions for India and
# Singapore, clean and quality-gate them, fit a panel forecast model
# (same intuition as the US/EU models), compute country/sector
# aggregations, export tables for the dashboard.
#
# A SEPARATE script and a SEPARATE output folder (shiny_data_asia/,
# not shiny_data/ or shiny_data_eu/) -- nothing here touches the US
# or EU pipelines' own files or results.
#
# SOURCE DATA, AS ACTUALLY UPLOADED AND INSPECTED (not assumed):
#  - India.xlsx: 18,626 rows, LONG format (one row per company x
#    reporting-period x emission-type), 1,281 distinct companies,
#    covering reporting periods from 2019 through 2026. type_of_
#    emissions mixes ABSOLUTE emissions ("Scope 1 Emissions", "Total
#    Scope 1 Emissions", etc.) with a large number of INTENSITY/ratio
#    metrics ("...Per Rupee Of Turnover", "...Intensity", etc.) under
#    dozens of inconsistent unit strings. Only the absolute-emissions
#    rows are used here -- confirmed by direct inspection that 97.4%
#    of those rows use one of a small set of tCO2e-equivalent unit
#    phrasings; the rest (garbage units like "not applicable", "nil",
#    or genuinely different pollutant units like micrograms/m3) are
#    dropped, not guessed at.
#  - Singapore_Companies_Emissions_Data.xlsx: 79 rows, WIDE format
#    (one row per company x year), 18 distinct companies, 2018-2024,
#    already in tCO2e. Scope 2 has both Location-Based and Market-
#    Based columns; Location-Based is used (67 of 79 rows populated,
#    vs 40 for Market-Based -- meaningfully more complete).
#
# QUALITY GATE: >=3 distinct years of Scope 1 data per company --
# chosen after checking actual per-company coverage in both files
# (996 of 1,309 Indian companies clear this bar; 13 of 18 Singaporean
# companies do). This is NOT the US pipeline's 13-year full-history
# gate -- that bar doesn't exist to clear here, since neither source
# spans anywhere near that many years. A shorter, real, data-driven
# bar was used instead of forcing an inappropriate one.
#
# STILL NOT INCLUDED, deliberately: target pathways. No equivalent
# sourced, cited sector-target workbook exists for India or Singapore
# yet -- same reasoning as the EU pipeline. The forecast line exists;
# the target line doesn't, and the dashboard should say so explicitly.
# ======================================================

setwd("D:/carbon final")

# Force a clean session -- same reasoning as the EU pipeline's
# rm(list = ls()): source this whole file top-to-bottom in one go, not
# piecemeal, so no stale object from an earlier run can silently
# disagree with this run's own logic.
rm(list = ls())

library(tidyverse)
library(readxl)
library(lme4)

india_path <- "India.xlsx"
singapore_path <- "Singapore_Companies_Emissions_Data.xlsx"

# ======================================================
# PART 1 -- LOAD AND CLEAN INDIA DATA
# ======================================================

cat("=== PART 1: LOADING INDIA DATA ===\n")

india_raw <- read_excel(india_path, sheet = 1)
cat("India raw rows:", nrow(india_raw), "\n")

# Absolute-emissions type_of_emissions values only -- excludes every
# intensity/ratio metric (per rupee, per employee, per tonne of
# product, etc.), of which there are dozens of variants in this file.
india_abs_types <- c(
  "Scope 1 Emissions", "Total Scope 1 Emissions",
  "Scope 2 Emissions", "Total Scope 2 Emissions",
  "Scope 3 Emissions", "Total Scope 3 Emissions"
)

# Unit -> multiplier to tCO2e. A WHITELIST, not a catch-all: any unit
# string not in this list is dropped rather than guessed at (this
# file has 150+ distinct unit strings across the full data, most of
# them intensity units or data-quality junk like "not applicable",
# "nil", or genuinely different pollutants like micrograms/m3).
# Confirmed by direct inspection: this whitelist covers 97.4% of the
# absolute-emissions rows.
india_unit_mult <- tibble::tribble(
  ~unit, ~mult,
  "value in metric tonne of co2 equivalent", 1,
  "value in tonne of co2 equivalent", 1,
  "value in tonne co2 equivalent", 1,
  "value in metric tonnes of co2 equivalent", 1,
  "value in tonne of co2", 1,
  "value in tonne", 1,
  "value in metric tonnes", 1,
  "value in tonne of co equivalent", 1,
  "value in co2 equivalent", 1,
  "value in in metric tonnes of co2 equivalent", 1,
  "value in mt co2 e", 1,
  "value in tonne-co2", 1,
  "value in tonne co2 equivalentuivalent", 1,
  "value in tco2", 1,
  "value in metric tonnes of co2e", 1,
  "value in eq co2", 1,
  "value in tonnes of co2 equivalent", 1,
  "value in tonne per co2 equivalent", 1,
  "value in tonne of co2 per year", 1,
  "value in kilogram co2 equivalent", 0.001,
  "value in million metric tonne of co2 equivalent", 1e6,
  "value in thousand metric tonnes of co2 equivalent", 1000,
  "value in thousand tonne of co2 equivalent", 1000,
  "value in kilo tonne of co2 equivalent", 1000,
  "value in million tonne of co2 equivalent", 1e6,
  "value in million metric tonnes of co2 equivalent", 1e6,
  "value in million tonnes of co2 equivalent", 1e6,
  "value in kilotonne co2 equivalent", 1000,
  "value in kilotonne of co2 equivalent", 1000,
  "value in kilo tonnes of co2 equivalent", 1000
)

india_metric_map <- c(
  "Scope 1 Emissions" = "scope1", "Total Scope 1 Emissions" = "scope1",
  "Scope 2 Emissions" = "scope2", "Total Scope 2 Emissions" = "scope2",
  "Scope 3 Emissions" = "scope3", "Total Scope 3 Emissions" = "scope3"
)

india_clean <- india_raw %>%
  filter(type_of_emissions %in% india_abs_types) %>%
  inner_join(india_unit_mult, by = "unit") %>%
  mutate(
    metric = india_metric_map[type_of_emissions],
    tco2e = value * mult,
    # end_date is an Excel serial date; origin "1899-12-30" is the
    # standard Excel epoch. Using the END of the reporting period as
    # the year label (a fiscal year ending March 2023 is labeled
    # 2023) -- consistent with how India's fiscal-year corporate
    # reporting is usually referenced.
    year = year(as.Date(end_date, origin = "1899-12-30")),
    # Sector capitalization is inconsistent in the source (e.g.
    # "Consumer discretionary Products" vs "Consumer Discretionary
    # Products") -- title-case normalization collapses these
    # duplicates without inventing new categories.
    sector = str_to_title(sector),
    company_id = cin_number,
    company_name = str_trim(company_name),
    country = "India"
  ) %>%
  filter(!is.na(tco2e), tco2e >= 0, !is.na(company_id), !is.na(year)) %>%
  # SANITY CEILING -- a real, confirmed data-quality problem in the
  # source file, not a unit-detection gap: several companies have
  # physically impossible values even with unambiguous units (e.g.
  # Larsen & Toubro Limited reports 615,000,000,000 tCO2e -- over 15x
  # GLOBAL annual emissions -- from a construction/engineering firm,
  # explicitly labeled "metric tonne of co2 equivalent", multiplier
  # x1). Almost certainly a PDF-extraction error upstream of this
  # file, not something fixable by better unit parsing. 1 billion
  # tCO2e is used as the ceiling -- confirmed by direct inspection to
  # correctly EXCLUDE the impossible entries (615B, 99B, 85B, 16B,
  # 1.8B tCO2e) while RETAINING India's largest legitimate real
  # emitter in this data, NTPC Limited (a coal power utility, real
  # values up to ~327M tCO2e). This does not guarantee every
  # remaining value is fully clean -- a few mid-size companies show
  # values that look high for their apparent scale (e.g. a food
  # company at ~400M tCO2e) but aren't impossible enough to
  # confidently exclude by an automated rule; flagged here rather
  # than silently left unaddressed.
  filter(tco2e < 1e9) %>%
  select(country, company_id, company_name, sector, year, metric, tco2e)

cat("India clean absolute-emissions rows:", nrow(india_clean), "\n")

# Long -> wide: one row per company x year, one column per scope.
# Where a company has more than one row for the same company/year/
# metric (a genuine duplicate report), the MAX is used -- a
# conservative choice that doesn't silently sum duplicate reports of
# the same real quantity.
india_wide <- india_clean %>%
  group_by(country, company_id, company_name, sector, year, metric) %>%
  summarise(tco2e = max(tco2e, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric, values_from = tco2e)

if (!"scope2" %in% names(india_wide)) india_wide$scope2 <- NA_real_
if (!"scope3" %in% names(india_wide)) india_wide$scope3 <- NA_real_

cat("India company x year rows:", nrow(india_wide), "\n")

# ======================================================
# PART 2 -- LOAD AND CLEAN SINGAPORE DATA
# ======================================================

cat("=== PART 2: LOADING SINGAPORE DATA ===\n")

singapore_raw <- read_excel(singapore_path, sheet = 1)
cat("Singapore raw rows:", nrow(singapore_raw), "\n")

singapore_wide <- singapore_raw %>%
  rename(
    company_name = `Company Name`, company_id_raw = `Company ID`,
    sector = `Sector (SICS)`, year = `Reporting Year`,
    scope1 = `Scope 1 (tCO2e)`,
    scope2_loc = `Scope 2 Location-Based (tCO2e)`,
    scope2_mkt = `Scope 2 Market-Based (tCO2e)`,
    scope3 = `Scope 3 (tCO2e)`
  ) %>%
  filter(!is.na(company_name), !is.na(year)) %>%
  mutate(
    country = "Singapore",
    company_id = paste0("SG-", company_id_raw),
    # Location-based preferred: meaningfully more complete than
    # market-based in this file (67 vs 40 non-missing rows, confirmed
    # by direct inspection) -- falls back to market-based only where
    # location-based itself is missing.
    scope2 = coalesce(scope2_loc, scope2_mkt),
    year = as.integer(year)
  ) %>%
  select(country, company_id, company_name, sector, year, scope1, scope2, scope3)

cat("Singapore company x year rows:", nrow(singapore_wide), "\n")

# ======================================================
# PART 3 -- COMBINE, QUALITY GATE
# ======================================================

cat("=== PART 3: COMBINING AND GATING ===\n")

asia_panel_raw <- bind_rows(india_wide, singapore_wide) %>%
  filter(!is.na(scope1), scope1 > 0)

# Quality gate: >= 3 distinct years of Scope 1 data per company.
# Chosen from this data's OWN actual coverage (checked directly, not
# assumed) -- neither source spans anywhere near the US pipeline's
# 13-year window, so that bar was never appropriate here. 3 years is
# the shortest span that still supports a real trend estimate rather
# than a two-point line.
quality_gate_min_years <- 3

company_year_counts <- asia_panel_raw %>%
  group_by(company_id) %>%
  summarise(n_years = n_distinct(year), .groups = "drop")

gated_companies <- company_year_counts %>% filter(n_years >= quality_gate_min_years) %>% pull(company_id)

asia_panel_filtered <- asia_panel_raw %>%
  filter(company_id %in% gated_companies) %>%
  arrange(country, company_id, year)

cat("Companies passing quality gate:", length(gated_companies),
    "of", nrow(company_year_counts), "\n")
cat("Final panel rows:", nrow(asia_panel_filtered), "\n")
cat("By country:\n")
print(asia_panel_filtered %>% distinct(country, company_id) %>% count(country))

# Sector bucketing -- >=10 companies to get its own bucket, else
# "Other" (relevel as reference, same convention as the US/EU
# pipelines' sector_bucket construction).
sector_counts <- asia_panel_filtered %>% distinct(sector, company_id) %>% count(sector)
top_sectors <- sector_counts %>% filter(n >= 10) %>% pull(sector)

asia_panel_filtered <- asia_panel_filtered %>%
  mutate(
    sector_bucket = if_else(sector %in% top_sectors, sector, "Other"),
    sector_bucket = factor(sector_bucket),
    sector_bucket = relevel(sector_bucket, ref = "Other"),
    country = factor(country)
  )

# ======================================================
# PART 4 -- FIT PANEL FORECAST MODEL
# ======================================================

cat("=== PART 4: FITTING FORECAST MODEL ===\n")

model_data <- asia_panel_filtered %>%
  filter(scope1 > 0) %>%
  mutate(log_emissions = log(scope1), year_c = year - median(year))

asia_model <- lmer(
  log_emissions ~ year_c + sector_bucket + sector_bucket:year_c + country + (1 + year_c | company_id),
  data = model_data, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)

cat("Model converged:", is.null(asia_model@optinfo$conv$lme4$code), "\n")

# Forecast 3 years beyond the panel's own last year (kept short
# deliberately -- these are thin, real per-company histories, and a
# long-horizon extrapolation from 3-6 data points would overstate
# confidence). Capped at 3x each company's own historical max, same
# safety convention as the US/EU pipelines.
last_year <- max(asia_panel_filtered$year)
forecast_years <- (last_year + 1):(last_year + 3)

company_meta <- asia_panel_filtered %>%
  distinct(company_id, company_name, country, sector, sector_bucket)

future_grid <- company_meta %>%
  tidyr::crossing(year = forecast_years) %>%
  mutate(year_c = year - median(model_data$year))

future_grid$pred_log <- predict(asia_model, newdata = future_grid, allow.new.levels = TRUE)
future_grid$p50 <- exp(future_grid$pred_log)

hist_max <- asia_panel_filtered %>% group_by(company_id) %>% summarise(hist_max = max(scope1), .groups = "drop")
future_pred_asia <- future_grid %>%
  left_join(hist_max, by = "company_id") %>%
  mutate(p50 = pmin(p50, hist_max * 3)) %>%
  select(company_id, company_name, country, sector, year, p50)

cat("Forecast rows:", nrow(future_pred_asia), "\n")

# ======================================================
# PART 5 -- AGGREGATIONS
# ======================================================

cat("=== PART 5: BUILDING AGGREGATIONS ===\n")

hist_by_country_asia <- asia_panel_filtered %>%
  group_by(country, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), .groups = "drop")

hist_by_sector_asia <- asia_panel_filtered %>%
  group_by(sector, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), n_companies = n_distinct(company_id), .groups = "drop") %>%
  filter(n_companies >= 3)  # avoid publishing a single-company "sector" total

company_lookup_asia <- asia_panel_filtered %>%
  distinct(company_id, company_name, country, sector, sector_bucket) %>%
  arrange(company_name)

settings_asia <- list(
  country_list = sort(unique(as.character(asia_panel_filtered$country))),
  sector_list = sort(unique(as.character(asia_panel_filtered$sector))),
  sector_bucket_list = sort(unique(as.character(asia_panel_filtered$sector_bucket))),
  quality_gate_min_years = quality_gate_min_years,
  last_hist_year = last_year
)

# ======================================================
# PART 6 -- EXPORT
# ======================================================

cat("=== PART 6: EXPORTING ===\n")

out_dir <- "shiny_data_asia"
if (!dir.exists(out_dir)) dir.create(out_dir)

saveRDS(asia_panel_filtered, file.path(out_dir, "asia_panel_filtered.rds"))
saveRDS(future_pred_asia,    file.path(out_dir, "future_pred_asia.rds"))
saveRDS(hist_by_country_asia, file.path(out_dir, "hist_by_country_asia.rds"))
saveRDS(hist_by_sector_asia,  file.path(out_dir, "hist_by_sector_asia.rds"))
saveRDS(company_lookup_asia,  file.path(out_dir, "company_lookup_asia.rds"))
saveRDS(settings_asia,        file.path(out_dir, "settings_asia.rds"))

cat("Done. Files written to", out_dir, "\n")
