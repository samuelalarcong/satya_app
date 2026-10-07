# ======================================================
# SATYA CARBON -- SCRIPT 1 / 2
# DATA PIPELINE: load raw data, fit model, forecast,
# compute target pathways, EXPORT tables for Shiny.
#
# Run this script LOCALLY, once (or whenever the source
# data changes). It does all the heavy lifting: reading
# the GHGRP workbook, fitting the lmer panel model,
# building the 2024-2028 forecast, and building the
# sector-target pathways.
#
# It does NOT launch any Shiny app. Its only job is to
# write out a set of clean, pre-computed tables into the
# "shiny_data/" folder. Script 2
# (satya_carbon_v4_02_shiny_app.R) reads that folder and
# only handles the dashboard -- no modeling, no xlsx
# parsing -- so it starts fast and can be deployed without
# lme4/readxl/janitor at all.
#
# v4 change carried over: adds official sector
# decarbonization TARGET pathways (per facility) alongside
# the observed history and the model forecast.
# Target data source: sector_decarbonization_
# targets_batch1.xlsx ("Model-Ready Targets" tab)
# ======================================================

setwd("C:/Users/samue/Desktop/carbon")

library(tidyverse)
library(readxl)
library(janitor)
library(lme4)

# ======================================================
# PART 1 -- LOAD GHGRP
# ======================================================

cat("=== PART 1: LOADING GHGRP ===\n")

ghgp_wide <- read_excel(
  "ghgp_data_by_year_2023.xlsx",
  sheet = "Direct Point Emitters",
  skip  = 3
) %>% clean_names()

ghgp_long <- ghgp_wide %>%
  select(
    facility_id,
    facility_name,
    state,
    sector = latest_reported_industry_type_sectors,
    starts_with("x20")
  ) %>%
  pivot_longer(
    cols      = starts_with("x20"),
    names_to  = "year",
    values_to = "emissions"
  ) %>%
  mutate(
    year      = as.integer(str_extract(year, "\\d{4}")),
    emissions = as.numeric(emissions)
  ) %>%
  filter(!is.na(emissions), emissions > 0)

ghgp_panel_complete <- ghgp_long %>%
  group_by(facility_id, facility_name,
           state, sector, year) %>%
  summarise(emissions = sum(emissions, na.rm=TRUE),
            .groups   = "drop") %>%
  mutate(
    primary_sector = str_split(sector, ",") %>%
      map_chr(1) %>%
      str_trim(),
    log_emissions  = log(emissions),
    year_c         = year - 2011,
    facility_id    = as.character(facility_id),
    primary_sector = factor(primary_sector) %>%
      relevel(ref = "Other"),
    state          = factor(state)
  ) %>%
  filter(!is.na(log_emissions),
         !is.na(primary_sector),
         !is.na(state),
         !is.na(facility_id))

cat("Total facilities:",
    n_distinct(ghgp_panel_complete$facility_id), "\n\n")

# ======================================================
# PART 2 -- FILTER: FULL 13-YEAR PANEL ONLY
# ======================================================

cat("=== PART 2: FILTER FULL 13-YEAR PANEL ===\n")

good_facilities <- ghgp_panel_complete %>%
  group_by(facility_id) %>%
  summarise(n_years = n_distinct(year),
            .groups = "drop") %>%
  filter(n_years == 13) %>%
  pull(facility_id)

ghgp_panel_filtered <- ghgp_panel_complete %>%
  filter(facility_id %in% good_facilities)

cat("Before:", n_distinct(ghgp_panel_complete$facility_id), "\n")
cat("After: ", length(good_facilities), "\n\n")

cat("=== BY SECTOR ===\n")
ghgp_panel_filtered %>%
  distinct(facility_id, primary_sector) %>%
  count(primary_sector, sort=TRUE) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  print()

# ======================================================
# PART 3 -- FIT PANEL MODEL
# ======================================================

cat("\n=== PART 3: FITTING PANEL MODEL ===\n")
cat("Facilities:", length(good_facilities), "\n")
cat("Years: 2011-2023 (all 13)\n\n")

panel_model <- lmer(
  log_emissions ~ year_c
  + primary_sector
  + primary_sector:year_c
  + state
  + (1 + year_c | facility_id),
  data    = ghgp_panel_filtered,
  REML    = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)

cat("=== MODEL SUMMARY ===\n")
print(summary(panel_model))

cat("\n=== SECTOR ANNUAL TRENDS ===\n")
fe       <- fixef(panel_model)
beta_ref <- fe[["year_c"]]
cat("Other (reference):",
    round((exp(beta_ref)-1)*100, 2), "% per year\n")

fe_names <- names(fe)
walk(
  fe_names[str_detect(fe_names,
                      "year_c:primary_sector|primary_sector.*:year_c")],
  function(nm) {
    s <- str_remove_all(nm, "year_c:|primary_sector")
    b <- beta_ref + fe[[nm]]
    cat(s, ":", round((exp(b)-1)*100, 2), "% per year\n")
  }
)

var_u   <- as.numeric(VarCorr(panel_model)$facility_id[1,1])
var_eps <- sigma(panel_model)^2
cat("\nICC:", round(var_u/(var_u+var_eps)*100, 1),
    "% of variance between facilities\n\n")

# ======================================================
# PART 4 -- FORECAST 2024-2028
# ======================================================

cat("=== PART 4: FORECAST 2024-2028 ===\n")

historical_max <- ghgp_panel_filtered %>%
  group_by(facility_id) %>%
  summarise(hist_max = max(emissions, na.rm=TRUE),
            .groups  = "drop")

future <- ghgp_panel_filtered %>%
  distinct(facility_id, facility_name,
           state, sector, primary_sector) %>%
  crossing(year = 2024:2028) %>%
  mutate(year_c = year - 2011)

future_pred <- future %>%
  mutate(
    predicted_log = predict(
      panel_model,
      newdata          = future,
      allow.new.levels = TRUE
    ),
    p50_raw = exp(predicted_log)
  ) %>%
  left_join(historical_max, by = "facility_id") %>%
  mutate(
    cap = hist_max * 3,
    p50 = pmin(p50_raw, cap)
  ) %>%
  select(facility_id, facility_name,
         primary_sector, state, year, p50)

cat("Facilities forecasted:",
    n_distinct(future_pred$facility_id), "\n\n")

cat("=== SANITY CHECK ===\n")
hist_2023 <- ghgp_panel_filtered %>%
  filter(year == 2023) %>%
  summarise(total = sum(emissions)/1e6) %>%
  pull(total)

fore_2028 <- future_pred %>%
  filter(year == 2028) %>%
  summarise(total = sum(p50)/1e6) %>%
  pull(total)

cat("Historical 2023:", round(hist_2023, 1), "Mt\n")
cat("Forecast   2028:", round(fore_2028, 1), "Mt\n")
cat("Change:         ",
    round((fore_2028-hist_2023)/hist_2023*100, 1),
    "%\n\n")

cat("=== BY SECTOR 2028 ===\n")
future_pred %>%
  filter(year == 2028) %>%
  mutate(primary_sector = as.character(primary_sector)) %>%
  group_by(primary_sector) %>%
  summarise(
    n_fac  = n_distinct(facility_id),
    mt2028 = round(sum(p50)/1e6, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mt2028)) %>%
  print()

# ======================================================
# PART 4B -- SECTOR DECARBONIZATION TARGET PATHWAYS
# ======================================================
# Methodology:
#  1. For each sector with a published target, derive the
#     implied constant annual rate that gets from
#     (baseline_year, 0% reduction) to (target_year, X%
#     reduction): rate = (1 - X)^(1/(target_year-baseline_year)) - 1
#  2. Anchor that rate at each facility's own 2023 actual
#     emissions (not the sector's official baseline value,
#     which GHGRP facilities rarely match exactly) and
#     project forward for 2024-2028.
#  3. This is a simplification -- it assumes a facility
#     already tracking its sector's official baseline would
#     need to follow this rate onward from today. Treat the
#     resulting line as "what compliance with the sector's
#     trajectory looks like from here," not as a facility-
#     specific commitment.

cat("=== PART 4B: LOADING SECTOR TARGETS ===\n")

target_raw <- read_excel(
  "sector_decarbonization_targets_batch1.xlsx",
  sheet = "Model-Ready Targets"
) %>% clean_names()

cat("Target sheet columns detected:",
    paste(names(target_raw), collapse = ", "), "\n")

# Robust to either the current plain header names (ghgrp_sector,
# reduction_fraction, ...) or the earlier multi-line-header version
# (ghgrp_sector_join_key_r_primary_sector, reduction_fraction_0_1_headline_pick,
# ...) - matches on column-name PREFIX so a stale cached copy of the workbook
# still works instead of erroring out.
detect_col <- function(df, prefix) {
  matches <- names(df)[str_starts(names(df), prefix)]
  if (length(matches) == 0) {
    stop(paste0(
      "Could not find a column starting with '", prefix, "' in the ",
      "'Model-Ready Targets' sheet. Columns found: ",
      paste(names(df), collapse = ", "),
      ". Re-download sector_decarbonization_targets_batch1.xlsx and make ",
      "sure it replaces (not sits alongside) any older copy in this folder."
    ))
  }
  matches[1]
}

col_sector     <- detect_col(target_raw, "ghgrp_sector")
col_reduction  <- detect_col(target_raw, "reduction_fraction")
col_citation   <- detect_col(target_raw, "citation_sheet_row")

target_raw <- target_raw %>%
  rename(
    ghgrp_sector        = all_of(col_sector),
    reduction_fraction  = all_of(col_reduction),
    citation_row        = all_of(col_citation)
  )

# -- Pull source citations from the "Sector Targets" detail tab --------
# "citation_row" points to the Excel row number (as it appears in the
# workbook, header included) of the specific source in "Sector Targets".
# Joining this in means every target the app shows can be traced back to
# its actual source organization, document, and URL -- not just a
# confidence label.
sector_targets_detail <- read_excel(
  "sector_decarbonization_targets_batch1.xlsx",
  sheet = "Sector Targets"
) %>%
  clean_names() %>%
  mutate(excel_row = row_number() + 1)  # +1: row 1 in the sheet is the header

col_source_org <- detect_col(sector_targets_detail, "source_organization")
col_source_doc <- detect_col(sector_targets_detail, "source_document")
col_url        <- detect_col(sector_targets_detail, "official_url")
col_subsector  <- detect_col(sector_targets_detail, "sub_sector")
col_metric     <- detect_col(sector_targets_detail, "metric")
col_scope      <- detect_col(sector_targets_detail, "scope")

sector_targets_detail <- sector_targets_detail %>%
  rename(
    source_organization = all_of(col_source_org),
    source_document      = all_of(col_source_doc),
    official_url          = all_of(col_url),
    sub_sector             = all_of(col_subsector),
    metric                  = all_of(col_metric),
    scope_coverage           = all_of(col_scope)
  ) %>%
  select(excel_row, source_organization, source_document, official_url,
         sub_sector, metric, scope_coverage)

target_lookup <- target_raw %>%
  filter(!is.na(reduction_fraction),
         !is.na(baseline_year),
         !is.na(target_year)) %>%
  mutate(
    annual_rate = (1 - reduction_fraction)^(1/(target_year - baseline_year)) - 1
  ) %>%
  left_join(sector_targets_detail, by = c("citation_row" = "excel_row")) %>%
  select(primary_sector = ghgrp_sector, baseline_year, target_year,
         reduction_fraction, annual_rate, confidence, caveat,
         source_organization, source_document, official_url,
         sub_sector, metric, scope_coverage)

cat("Target rows with a matched citation:",
    sum(!is.na(target_lookup$official_url)), "of", nrow(target_lookup), "\n")

cat("Sectors with a usable target pathway:", nrow(target_lookup),
    "of", length(unique(as.character(ghgp_panel_filtered$primary_sector))),
    "\n\n")

anchor_year <- 2023  # last observed year -- the trajectory's starting point

# -- Facility-level anchor (2023 actual emissions per facility) --
facility_anchor <- ghgp_panel_filtered %>%
  filter(year == anchor_year) %>%
  group_by(facility_id, primary_sector) %>%
  summarise(anchor_value = sum(emissions, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(primary_sector = as.character(primary_sector))

# -- Facility-level target trajectory, matched to the forecast horizon --
target_pred <- facility_anchor %>%
  inner_join(target_lookup, by = "primary_sector") %>%
  crossing(year = 2024:2028) %>%
  mutate(
    target = anchor_value * (1 + annual_rate) ^ (year - anchor_year)
  ) %>%
  select(facility_id, primary_sector, year, target,
         annual_rate, reduction_fraction, target_year, confidence)

cat("Facilities with a target pathway:",
    n_distinct(target_pred$facility_id), "of",
    length(good_facilities), "\n\n")

# -- Sector-level target trajectory (sum of facility-level targets) --
target_by_sector <- target_pred %>%
  group_by(year, primary_sector) %>%
  summarise(target = sum(target, na.rm = TRUE), .groups = "drop")

cat("=== TARGET vs MODEL FORECAST, 2028, BY SECTOR ===\n")
future_pred %>%
  filter(year == 2028) %>%
  mutate(primary_sector = as.character(primary_sector)) %>%
  group_by(primary_sector) %>%
  summarise(model_mt2028 = sum(p50) / 1e6, .groups = "drop") %>%
  left_join(
    target_by_sector %>% filter(year == 2028) %>%
      transmute(primary_sector, target_mt2028 = target / 1e6),
    by = "primary_sector"
  ) %>%
  mutate(gap_pct = round((model_mt2028 - target_mt2028) / target_mt2028 * 100, 1)) %>%
  arrange(desc(model_mt2028)) %>%
  print(n = 20)

# ======================================================
# PART 5 -- PRE-COMPUTE FOR SHINY
# ======================================================

cat("\n=== PART 5: PRE-COMPUTE ===\n")

# Sector aggregations
hist_by_sector <- ghgp_panel_filtered %>%
  group_by(year, primary_sector) %>%
  summarise(emissions = sum(emissions, na.rm=TRUE),
            .groups   = "drop") %>%
  mutate(primary_sector = as.character(primary_sector))

fore_by_sector <- future_pred %>%
  group_by(year, primary_sector) %>%
  summarise(p50     = sum(p50, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(primary_sector = as.character(primary_sector))

# Sector x state aggregations -- same idea, one level more specific.
# n_facilities carried through so the app can decide whether a given
# sector+state combination has enough facilities to be a meaningful
# benchmark, or whether it should fall back to the sector-wide one.
hist_by_sector_state <- ghgp_panel_filtered %>%
  group_by(year, primary_sector, state) %>%
  summarise(
    emissions    = sum(emissions, na.rm = TRUE),
    n_facilities = n_distinct(facility_id),
    .groups      = "drop"
  ) %>%
  mutate(primary_sector = as.character(primary_sector), state = as.character(state))

fore_by_sector_state <- future_pred %>%
  group_by(year, primary_sector, state) %>%
  summarise(
    p50          = sum(p50, na.rm = TRUE),
    n_facilities = n_distinct(facility_id),
    .groups      = "drop"
  ) %>%
  mutate(primary_sector = as.character(primary_sector), state = as.character(state))

cat("Sector x state aggregations: done (",
    n_distinct(hist_by_sector_state$state), "states)\n")

# Facility lookup
facility_lookup <- ghgp_panel_filtered %>%
  filter(year == 2023) %>%
  group_by(facility_id, facility_name,
           primary_sector) %>%
  summarise(emissions_2023 = sum(emissions, na.rm=TRUE),
            .groups        = "drop") %>%
  arrange(desc(emissions_2023)) %>%
  mutate(
    primary_sector = as.character(primary_sector),
    label = paste0(
      facility_name,
      " [", facility_id, "]",
      " -- ", primary_sector
    )
  )

cat("Sector aggregations: done\n")
cat("Facility lookup:     ", nrow(facility_lookup),
    "facilities\n\n")

# ======================================================
# PART 6 -- SETTINGS (static dashboard config)
# ======================================================

sector_colors <- c(
  "Power Plants"                = "#F39C12",
  "Petroleum and Natural Gas Systems" = "#3498DB",
  "Petroleum Product Suppliers" = "#2980B9",
  "Natural Gas and Natural Gas Liquids Suppliers" = "#1A5276",
  "Chemicals"                   = "#E74C3C",
  "Refineries"                  = "#9B59B6",
  "Metals"                      = "#1ABC9C",
  "Minerals"                    = "#E67E22",
  "Waste"                       = "#95A5A6",
  "Pulp and Paper"              = "#27AE60",
  "Injection of CO2"            = "#2ECC71",
  "Suppliers of CO2"            = "#48C9B0",
  "Industrial Gas Suppliers"    = "#F1948A",
  "Import and Export of Equipment Containing Fluorintaed GHGs" = "#BB8FCE",
  "Other"                       = "#BDC3C7"
)

target_color <- "#1B5E20"  # fixed dark green for every target line, so it reads
                           # consistently across all sector colors

x_breaks    <- c(2011, 2015, 2019, 2023, 2026, 2028)
sector_list <- sort(unique(hist_by_sector$primary_sector))

facility_choices <- setNames(
  facility_lookup$facility_id,
  facility_lookup$label
)

# ======================================================
# PART 7 -- EXPORT TABLES FOR SHINY
# ======================================================
# Everything the Shiny app needs to render the dashboard,
# with NO further modeling or xlsx parsing required.
# Two formats are written:
#   - shiny_data/*.rds  -> fast, type-safe, read by the app
#   - shiny_data/csv/*.csv -> human-readable tables, for
#     review in Excel or for other tools
# ======================================================

cat("=== PART 7: EXPORTING TABLES ===\n")

out_dir <- "shiny_data"
csv_dir <- file.path(out_dir, "csv")
dir.create(out_dir, showWarnings = FALSE)
dir.create(csv_dir, showWarnings = FALSE)

# Bundle small config/settings objects together
settings <- list(
  sector_colors    = sector_colors,
  target_color     = target_color,
  x_breaks         = x_breaks,
  sector_list      = sector_list,
  state_list       = sort(unique(as.character(ghgp_panel_filtered$state))),
  facility_choices = facility_choices,
  n_good_facilities = length(good_facilities),
  n_sectors_target  = n_distinct(target_lookup$primary_sector)
)

# Core data tables (RDS - preserves types, fast to load)
saveRDS(ghgp_panel_filtered, file.path(out_dir, "ghgp_panel_filtered.rds"))
saveRDS(future_pred,         file.path(out_dir, "future_pred.rds"))
saveRDS(target_pred,         file.path(out_dir, "target_pred.rds"))
saveRDS(target_lookup,       file.path(out_dir, "target_lookup.rds"))
saveRDS(hist_by_sector,      file.path(out_dir, "hist_by_sector.rds"))
saveRDS(fore_by_sector,      file.path(out_dir, "fore_by_sector.rds"))
saveRDS(hist_by_sector_state, file.path(out_dir, "hist_by_sector_state.rds"))
saveRDS(fore_by_sector_state, file.path(out_dir, "fore_by_sector_state.rds"))
saveRDS(target_by_sector,    file.path(out_dir, "target_by_sector.rds"))
saveRDS(facility_lookup,     file.path(out_dir, "facility_lookup.rds"))
saveRDS(settings,            file.path(out_dir, "settings.rds"))

# Optional: keep the fitted model object for reference /
# future diagnostics. Not needed by the Shiny app itself.
saveRDS(panel_model, file.path(out_dir, "panel_model.rds"))

# Human-readable CSV copies of the main tables
write_csv(ghgp_panel_filtered, file.path(csv_dir, "ghgp_panel_filtered.csv"))
write_csv(future_pred,         file.path(csv_dir, "future_pred.csv"))
write_csv(target_pred,         file.path(csv_dir, "target_pred.csv"))
write_csv(target_lookup,       file.path(csv_dir, "target_lookup.csv"))
write_csv(hist_by_sector,      file.path(csv_dir, "hist_by_sector.csv"))
write_csv(fore_by_sector,      file.path(csv_dir, "fore_by_sector.csv"))
write_csv(hist_by_sector_state, file.path(csv_dir, "hist_by_sector_state.csv"))
write_csv(fore_by_sector_state, file.path(csv_dir, "fore_by_sector_state.csv"))
write_csv(target_by_sector,    file.path(csv_dir, "target_by_sector.csv"))
write_csv(facility_lookup,     file.path(csv_dir, "facility_lookup.csv"))

cat("Done. Tables written to:", normalizePath(out_dir), "\n")
cat("  - RDS  (used by the Shiny app):", file.path(out_dir, "*.rds"), "\n")
cat("  - CSV  (for manual review):    ", file.path(csv_dir, "*.csv"), "\n\n")

cat("Next step: run satya_carbon_v4_02_shiny_app.R ",
    "(it only reads from '", out_dir, "', no modeling needed).\n", sep = "")
