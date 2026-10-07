# ======================================================
# SATYA CARBON -- SCRIPT 2 / 2
# SHINY APP: reads pre-computed tables from "shiny_data/"
# (written by satya_carbon_v4_01_data_pipeline.R) and
# renders the dashboard.
#
# This script does NOT read the GHGRP workbook, does NOT
# fit any model, and does NOT parse xlsx target files.
# It only reads shiny_data/*.rds, so it starts fast and
# can be deployed without lme4 / readxl / janitor.
#
# Run PART 1 of the data pipeline script first (or
# whenever the source data changes), then run this file.
#
# CHANGE LOG:
#   + New "New Company Intake" tab: lets companies with no
#     existing GHGRP history either (a) submit their own
#     emissions + target, or (b) get an interim sector-average
#     benchmark and guidance on getting emissions accounting done.
# ======================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(jsonlite)
library(lpSolve)
library(digest)
# Used for the facility/project map on the Portfolio Mix tab (state
# outlines via ggplot2::map_data("state") + state centroids below). A
# standard, commonly pre-installed package -- if missing, install with
# install.packages("maps").
library(maps)

# ---- Excel template upload (Company Profile's Scope 1/2/3 emissions
# template) ----
# NOT otherwise a dependency of this app -- loaded defensively, same
# pattern as plotly/rnaturalearth below: requireNamespace only, so a
# server without readxl installed just loses the upload feature
# specifically rather than failing to start.
has_readxl <- requireNamespace("readxl", quietly = TRUE)
has_writexl <- requireNamespace("writexl", quietly = TRUE)

# ---- Interactive globe preview (plotly) ----
# NOT currently a dependency anywhere else in this app -- added
# defensively, same pattern as rnaturalearth/sf below: requireNamespace
# only (never library()), so a server without plotly installed just
# doesn't get the interactive globe rather than failing to start.
has_plotly <- requireNamespace("plotly", quietly = TRUE)

# ---- World map (Phase 1 global geography -- for non-US facilities and
# projects) ----
# NOT a base/commonly-pre-installed package like maps -- if missing,
# install with: install.packages(c("rnaturalearth", "rnaturalearthdata",
# "sf")). Loaded defensively (requireNamespace, never library()) and
# wrapped in tryCatch so a machine without these installed degrades the
# WORLD map specifically -- falling back to a simple country-list
# summary instead of a rendered map -- rather than crashing the entire
# app at startup. The existing US county/state map (below) needs
# neither of these and is completely unaffected either way.
has_world_map_pkgs <- requireNamespace("rnaturalearth", quietly = TRUE) &&
  requireNamespace("sf", quietly = TRUE)

world_countries_sf <- NULL
if (has_world_map_pkgs) {
  world_countries_sf <- tryCatch(
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
    error = function(e) NULL
  )
  if (is.null(world_countries_sf)) has_world_map_pkgs <- FALSE
}

# Natural Earth's own "admin" name field doesn't always match the plain
# English country names used elsewhere in this app (most notably "United
# States" vs. Natural Earth's "United States of America") -- this table
# is the explicit, honest translation for every country actually used in
# global_country_list, rather than relying on a fuzzy string match that
# could silently mismatch to the wrong country.
country_name_to_ne_admin <- c(
  "United States" = "United States of America",
  "Germany" = "Germany", "France" = "France", "Netherlands" = "Netherlands",
  "Spain" = "Spain", "Italy" = "Italy", "Poland" = "Poland",
  "United Kingdom" = "United Kingdom",
  "India" = "India", "China" = "China", "Japan" = "Japan",
  "Singapore" = "Singapore", "Australia" = "Australia",
  "Brazil" = "Brazil", "Chile" = "Chile", "Argentina" = "Argentina", "Uruguay" = "Uruguay",
  "Saudi Arabia" = "Saudi Arabia", "United Arab Emirates" = "United Arab Emirates", "Qatar" = "Qatar",
  "South Africa" = "South Africa", "Morocco" = "Morocco"
)

# ======================================================
# PART 0 -- LOAD PRE-COMPUTED TABLES
# ======================================================

data_dir <- "shiny_data"

need <- c(
  "ghgp_panel_filtered.rds", "future_pred.rds", "target_pred.rds",
  "target_lookup.rds", "hist_by_sector.rds", "fore_by_sector.rds",
  "target_by_sector.rds", "facility_lookup.rds", "settings.rds"
)
missing <- need[!file.exists(file.path(data_dir, need))]
if (length(missing) > 0) {
  stop(
    "Missing files in '", data_dir, "': ", paste(missing, collapse = ", "),
    ". Run satya_carbon_v4_01_data_pipeline.R first."
  )
}

ghgp_panel_filtered <- readRDS(file.path(data_dir, "ghgp_panel_filtered.rds"))
future_pred         <- readRDS(file.path(data_dir, "future_pred.rds"))
target_pred          <- readRDS(file.path(data_dir, "target_pred.rds"))
target_lookup        <- readRDS(file.path(data_dir, "target_lookup.rds"))
hist_by_sector       <- readRDS(file.path(data_dir, "hist_by_sector.rds"))
fore_by_sector       <- readRDS(file.path(data_dir, "fore_by_sector.rds"))
target_by_sector     <- readRDS(file.path(data_dir, "target_by_sector.rds"))
facility_lookup      <- readRDS(file.path(data_dir, "facility_lookup.rds"))
settings             <- readRDS(file.path(data_dir, "settings.rds"))

# City/state on facility_lookup are a newer addition -- an older export
# won't have them, so the location charts need to degrade gracefully
# rather than error.
has_city_data <- all(c("city", "state") %in% names(facility_lookup))

# State-level tables are OPTIONAL, not in the hard `need` list above --
# an older shiny_data/ export (from before this feature) is missing them,
# and the app should degrade gracefully (state selector just won't do
# anything useful) rather than refuse to start entirely.
state_optional_files <- c("hist_by_sector_state.rds", "fore_by_sector_state.rds")
has_state_data <- all(file.exists(file.path(data_dir, state_optional_files)))

hist_by_sector_state <- if (has_state_data) readRDS(file.path(data_dir, "hist_by_sector_state.rds")) else NULL
fore_by_sector_state <- if (has_state_data) readRDS(file.path(data_dir, "fore_by_sector_state.rds")) else NULL

# Backtest tables are OPTIONAL too, same reasoning -- an older shiny_data/
# export (from before this feature existed) simply won't have a
# Backtesting tab with real content; degrade gracefully instead of refusing
# to start.
backtest_optional_files <- c("backtest_by_facility.rds", "backtest_by_sector.rds")
has_backtest_data <- all(file.exists(file.path(data_dir, backtest_optional_files)))

backtest_by_facility <- if (has_backtest_data) readRDS(file.path(data_dir, "backtest_by_facility.rds")) else NULL
backtest_by_sector   <- if (has_backtest_data) readRDS(file.path(data_dir, "backtest_by_sector.rds")) else NULL

# ---- Europe (EPRTR) tables -- COMPLETELY SEPARATE folder from the US
# shiny_data/ above, per explicit request: the US pipeline and its
# outputs must never be touched or put at risk by adding Europe.
# Written by the separate satya_carbon_v4_01_data_pipeline_europe.R
# script. Optional, same graceful-degradation pattern as every other
# optional data source in this app -- if shiny_data_eu/ doesn't exist
# yet, the EU tabs show a clear "not available" message instead of
# crashing the whole app.
# ---- Europe (EPRTR) tables -- COMPLETELY SEPARATE folder from the US
# shiny_data/ above, per explicit request: the US pipeline and its
# outputs must never be touched or put at risk by adding Europe.
# Written by the separate satya_carbon_v4_01_data_pipeline_europe.R
# script. Optional, same graceful-degradation pattern as every other
# optional data source in this app -- if shiny_data_eu/ doesn't exist
# yet, the EU tabs show a clear "not available" message instead of
# crashing the whole app.
#
# Two DIFFERENT sector groupings, deliberately not merged:
#  - hist_by_sector_eu: the EEA's own official ~9-category rollup,
#    read directly from the Air_Releases_Sector sheet (per explicit
#    instruction). Historical only -- that sheet has no model behind
#    it, so no forecast exists at this grain.
#  - hist_by_sector_bucket_eu / fore_by_sector_bucket_eu: the panel
#    model's own sector grouping (EPRTRAnnexIMainActivity activity
#    codes, bucketed to the ones with enough facilities to estimate a
#    real trend for). This is what the forecast is actually built on,
#    and it's a genuinely finer, different classification -- the EU
#    tabs label which is which rather than implying they're the same.
eu_data_dir <- "shiny_data_eu"
eu_needed_files <- c(
  "eu_panel_filtered.rds", "future_pred_eu.rds", "hist_by_country_eu.rds",
  "hist_by_sector_eu.rds", "hist_by_sector_bucket_eu.rds", "fore_by_sector_bucket_eu.rds",
  "facility_lookup_eu.rds", "settings_eu.rds"
)
has_eu_data <- all(file.exists(file.path(eu_data_dir, eu_needed_files)))

eu_panel_filtered        <- if (has_eu_data) readRDS(file.path(eu_data_dir, "eu_panel_filtered.rds"))        else NULL
future_pred_eu           <- if (has_eu_data) readRDS(file.path(eu_data_dir, "future_pred_eu.rds"))           else NULL
hist_by_country_eu       <- if (has_eu_data) readRDS(file.path(eu_data_dir, "hist_by_country_eu.rds"))       else NULL
hist_by_sector_eu        <- if (has_eu_data) readRDS(file.path(eu_data_dir, "hist_by_sector_eu.rds"))        else NULL
hist_by_sector_bucket_eu <- if (has_eu_data) readRDS(file.path(eu_data_dir, "hist_by_sector_bucket_eu.rds")) else NULL
fore_by_sector_bucket_eu <- if (has_eu_data) readRDS(file.path(eu_data_dir, "fore_by_sector_bucket_eu.rds")) else NULL
facility_lookup_eu       <- if (has_eu_data) readRDS(file.path(eu_data_dir, "facility_lookup_eu.rds"))       else NULL
settings_eu              <- if (has_eu_data) readRDS(file.path(eu_data_dir, "settings_eu.rds"))              else NULL

sector_list_eu_official <- if (has_eu_data) settings_eu$sector_list_official else character(0)
sector_bucket_list_eu   <- if (has_eu_data) settings_eu$sector_bucket_list else character(0)
country_list_eu         <- if (has_eu_data) settings_eu$country_list else character(0)
facility_choices_eu     <- if (has_eu_data) settings_eu$facility_choices else character(0)
eu_x_breaks             <- if (has_eu_data) settings_eu$x_breaks else c(2007, 2011, 2015, 2019, 2024, 2029)
eu_has_forecast         <- if (has_eu_data) isTRUE(settings_eu$has_forecast) else FALSE

# SBTi company-level target table -- OPTIONAL, same reasoning as above.
# Only exists once the pipeline has actually run the real company-name
# matching join; an older export just won't have an SBTi line available.
sbti_optional_files <- c("sbti_target_pred.rds", "sbti_company_lookup.rds", "sbti_sector_benchmark.rds")
has_sbti_data <- all(file.exists(file.path(data_dir, sbti_optional_files)))
sbti_target_pred      <- if (has_sbti_data) readRDS(file.path(data_dir, "sbti_target_pred.rds")) else NULL
sbti_company_lookup   <- if (has_sbti_data) readRDS(file.path(data_dir, "sbti_company_lookup.rds")) else NULL
sbti_sector_benchmark <- if (has_sbti_data) readRDS(file.path(data_dir, "sbti_sector_benchmark.rds")) else NULL

sbti_n_matched   <- settings$sbti_n_companies_matched
sbti_n_total     <- settings$sbti_n_companies_total
sbti_n_fac_match <- settings$sbti_n_facilities_matched

# SBTi sector + target REFERENCE table (Part 4C of the pipeline) -- the
# raw list of "which sectors does SBTi publish US targets for, and what
# are those targets," with no facility/company matching applied. This is
# separate from has_sbti_data above (which is about the matched,
# facility-level SBTi pathway) -- this one just needs the file to exist
# so it degrades gracefully on an older shiny_data/ export.
has_sbti_sector_targets <- file.exists(file.path(data_dir, "sbti_sector_targets.rds"))
sbti_sector_targets <- if (has_sbti_sector_targets) readRDS(file.path(data_dir, "sbti_sector_targets.rds")) else NULL

has_sbti_big_sector_summary <- file.exists(file.path(data_dir, "sbti_big_sector_summary.rds"))
sbti_big_sector_summary <- if (has_sbti_big_sector_summary) readRDS(file.path(data_dir, "sbti_big_sector_summary.rds")) else NULL

# SBTi vs GHGRP sector overlap (pipeline Part 4C.5) -- one row per sector
# name with in_sbti/in_ghgrp/venn_group, used to draw the Goals tab Venn
# diagram and the "which sectors are in which bucket" table underneath it.
has_sector_venn <- file.exists(file.path(data_dir, "sector_venn_membership.rds"))
sector_venn_membership <- if (has_sector_venn) readRDS(file.path(data_dir, "sector_venn_membership.rds")) else NULL

# Official GHGRP subpart/industry reference (pipeline Part 4C.45) --
# canonical EPA subpart list + facility_type + our own facility counts.
# Used for the Goals tab's "GHGRP Sectors" table instead of deriving the
# sector list solely from whatever's in facility_lookup.
has_ghgrp_industry_reference <- file.exists(file.path(data_dir, "ghgrp_industry_reference.rds"))
ghgrp_industry_reference <- if (has_ghgrp_industry_reference) readRDS(file.path(data_dir, "ghgrp_industry_reference.rds")) else NULL

# Live subpart presence check (pipeline Part 2.5) -- genuinely computed
# from the actual raw "subparts" data against the modeled facility
# population, not hardcoded. OPTIONAL: an older shiny_data/ export won't
# have this yet, so the tree below falls back to its hardcoded reference
# presence values if it's missing.
has_ghgrp_subpart_presence <- file.exists(file.path(data_dir, "ghgrp_subpart_presence.rds"))
ghgrp_subpart_presence <- if (has_ghgrp_subpart_presence) readRDS(file.path(data_dir, "ghgrp_subpart_presence.rds")) else NULL

# Actual-goals-used tables (pipeline Part 4C.55): our sector list with
# facility counts, the resolved goal source per sector (SBTi crosswalk
# match vs. sector-proxy fallback), and the subset that falls back to the
# non-SBTi source. All OPTIONAL -- an older shiny_data/ export won't have
# these yet.
has_ghgrp_actual_goals <- file.exists(file.path(data_dir, "ghgrp_actual_goals.rds"))
ghgrp_sector_counts  <- if (has_ghgrp_actual_goals) readRDS(file.path(data_dir, "ghgrp_sector_counts.rds"))  else NULL
ghgrp_actual_goals   <- if (has_ghgrp_actual_goals) readRDS(file.path(data_dir, "ghgrp_actual_goals.rds"))   else NULL
ghgrp_non_sbti_goals <- if (has_ghgrp_actual_goals) readRDS(file.path(data_dir, "ghgrp_non_sbti_goals.rds")) else NULL

# Hand-curated cross-reference: which official GHGRP subparts commonly
# co-occur under each of OUR primary_sector categories, with short display
# labels (deliberately shorter than ghgrp_industry_reference's full official
# names -- e.g. "Adipic Acid" not "Adipic Acid Production"). Not derivable
# automatically from primary_sector alone (that column already collapsed
# each facility down to just its first-reported subpart), so this is a
# manual mapping, not a computed one. Some subparts legitimately appear
# under more than one sector (e.g. "C" - Stationary Combustion - is common
# across Chemicals, Power Plants, and Other alike), sometimes with a
# different short label depending on context (HH is "Landfills" under
# Power Plants but "Municipal Landfills" under Waste).
# note_override: NA_character_ means the generic "NOT IN DATA" applies if
# absent (explicit NA_character_, not bare NA -- mixing bare NA/logical
# with a character string in the same tribble column is a known R pitfall
# that can produce an unusable column; being explicit avoids it entirely);
# a few rows (UU specifically -- a CO2 injection record, not a direct
# emitter) get a different, more accurate reason for their absence.
ghgrp_sector_subpart_map <- local({
  main_sector <- c(
    rep("Chemicals", 16), rep("Metals", 6), rep("Minerals", 5), rep("Power Plants", 4),
    rep("Waste", 3), rep("Petroleum and Natural Gas Systems", 15), rep("Other", 8)
  )
  subpart_letter <- c(
    "C","E","G","K","L","N","O","P","U","V","X","Y","Z","EE","BB","SS",
    "F","K","Q","R","T","GG",
    "H","N","S","CC","U",
    "C","D","DD","HH",
    "HH","II","TT",
    "W","W-OFFSH","W-ONSH","W-GB","W-PROC","W-NGTC","W-TRANS","W-UNSTG","W-LNGSTG","W-LNGIE","W-LDC","Y","P","X","UU",
    "C","FF","I","AA","BB","DD","SS","TT"
  )
  short_label <- c(
    "Stationary Combustion","Adipic Acid","Ammonia","Ferroalloy","Fluorinated GHG","Glass","HCFC-22",
    "Hydrogen","Carbonates","Nitric Acid","Petrochemicals","Refining","Phosphoric Acid","Titanium Dioxide",
    "Silicon Carbide","Electric Transmission Equipment",
    "Aluminum","Ferroalloy","Iron & Steel","Lead","Magnesium","Zinc",
    "Cement","Glass","Lime","Soda Ash","Carbonates",
    "Stationary Combustion","Electricity Generation","SF6 from Electrical Equipment","Landfills",
    "Municipal Landfills","Industrial Wastewater","Industrial Waste Landfills",
    "General","Offshore","Onshore","Gathering & Boosting","Processing","Transmission/Compression",
    "Transmission Pipelines","Underground Storage","LNG Storage","LNG Import/Export","Local Distribution",
    "Refining","Hydrogen","Petrochemicals","CO2 Injection",
    "Stationary Combustion","Coal Mines","Electronics","Pulp & Paper","Silicon Carbide",
    "SF6 from Electrical Equipment","Electric Transmission Equipment","Industrial Waste Landfills"
  )
  # Hardcoded directly from a confirmed, authoritative EPA subpart
  # reference (39 real direct-emitter subparts found in the data, plus a
  # handful of niche/supplier/CO2-injection subparts confirmed absent) --
  # NOT derived from a live join against ghgrp_industry_reference, since
  # that join was producing unreliable results (likely a sector-name
  # string-matching mismatch). If the underlying data changes, this list
  # needs updating by hand to match.
  present <- c(
    TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,
    TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE,TRUE,TRUE,
    TRUE,TRUE,FALSE,TRUE,
    TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,
    TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE
  )
  note_override <- rep(NA_character_, length(main_sector))
  note_override[main_sector == "Petroleum and Natural Gas Systems" & subpart_letter == "UU"] <- "NOT A DIRECT EMITTER"

  stopifnot(
    length(main_sector) == length(subpart_letter),
    length(main_sector) == length(short_label),
    length(main_sector) == length(present),
    length(main_sector) == length(note_override)
  )

  data.frame(main_sector, subpart_letter, short_label, present, note_override, stringsAsFactors = FALSE)
})

# Same normalization the pipeline used for its SBTi join -- duplicated
# here (not shared code between the two scripts) so a freshly-typed
# company name (e.g. in New Company Intake) can be matched live against
# sbti_company_lookup, the same way, with the same conservative
# exact-match-only discipline (no fuzzy matching).
normalize_company_name <- function(x) {
  x <- toupper(x)
  x <- gsub("[.,']", "", x)
  x <- gsub("\\b(INC|LLC|CO|CORP|CORPORATION|COMPANY|LTD|LP|PLC|GROUP|HOLDINGS|INCORPORATED)\\b", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

sector_colors    <- settings$sector_colors
target_color     <- settings$target_color
x_breaks         <- settings$x_breaks
sector_list      <- settings$sector_list
state_list       <- if (!is.null(settings$state_list)) settings$state_list else character(0)
facility_choices <- settings$facility_choices

# Quality gate metadata (what the pipeline actually applied globally) --
# falls back to NULL display if running against an older export that
# predates the gate.
qgate_threshold      <- settings$quality_gate_threshold
qgate_min_facilities <- settings$quality_gate_min_facilities
qgate_floor_forced   <- settings$quality_gate_floor_forced
qgate_n_kept         <- settings$quality_gate_n_kept
qgate_n_tested       <- settings$quality_gate_n_tested

last_hist_year <- max(ghgp_panel_filtered$year)
last_fore_year <- max(future_pred$year)

# ======================================================
# PART 0.3 -- DERIVE "COMPANY" FROM FACILITY NAME
# Simple heuristic (same pattern used in earlier project scripts): the
# first token of the uppercased facility name, e.g. "3M" from both
# "3M BROWNWOOD" and "3M Chemical Operations' Cordova Facility". This is
# NOT a verified corporate-ownership registry -- two facilities owned by
# the same real company but named differently (no shared first word)
# will NOT be grouped together, and this crude match can occasionally
# over-group unrelated facilities that happen to share a first word.
# Base R only (no stringr) to keep this script's lightweight-deploy
# design intact.
# ======================================================

extract_company <- function(name) {
  name_upper <- toupper(name)
  # regmatches(x, regexpr(pattern, x)) silently DROPS any element that
  # doesn't match the pattern at all, shortening the returned vector --
  # this never showed up for US facility names (always start with
  # A-Z0-9&), but a real European facility name that starts with
  # anything else (accented characters that don't uppercase into plain
  # ASCII, punctuation, etc.) triggers it, and the mismatched-length
  # vector then fails inside mutate()'s strict recycling. Using
  # regexpr() + substring() directly instead avoids the drop entirely:
  # substring() is properly vectorized and always returns one value per
  # input, so there's nothing for the ifelse() below to mismatch on.
  m <- regexpr("^[A-Z0-9&]+", name_upper)
  ifelse(m > 0, substring(name_upper, 1, attr(m, "match.length")), name_upper)
}

facility_lookup <- facility_lookup %>%
  mutate(company = extract_company(facility_name))

company_lookup <- facility_lookup %>%
  group_by(company) %>%
  summarise(
    n_facilities         = n_distinct(facility_id),
    total_emissions_2023 = sum(emissions_2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_facilities), desc(total_emissions_2023))

company_choices <- setNames(
  company_lookup$company,
  paste0(company_lookup$company, " (", company_lookup$n_facilities, " facilities)")
)

# Alphabetical version -- same underlying values (still the plain
# extracted company key, so typing/matching downstream is unaffected),
# just sorted A-Z instead of by facility count, for the Company Profile
# browse-your-own-company dropdown specifically.
company_lookup_alpha <- company_lookup %>% arrange(company)
company_choices_alpha <- setNames(
  company_lookup_alpha$company,
  paste0(company_lookup_alpha$company, " (", company_lookup_alpha$n_facilities,
         ifelse(company_lookup_alpha$n_facilities == 1, " facility)", " facilities)"))
)

# ---- EU company grouping -- reuses extract_company() directly, since
# that heuristic was never US-specific to begin with (first uppercase
# word of the facility name). Same role as company_lookup above, one
# level up: groups EU facilities into companies for EU Company
# Profile's real-company matching. Placed here, AFTER extract_company()
# is defined -- this is top-level script code, executed sequentially
# at source time, unlike the reactives inside server() which are
# lazily evaluated and can reference things defined later in the file.
if (has_eu_data) {
  facility_lookup_eu <- facility_lookup_eu %>% mutate(eu_company = extract_company(facility_name))
  eu_company_lookup <- facility_lookup_eu %>%
    group_by(eu_company) %>%
    summarise(n_facilities = n_distinct(facility_id),
              total_emissions_latest = sum(emissions_latest, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(n_facilities), desc(total_emissions_latest))
  eu_company_lookup_alpha <- eu_company_lookup %>% arrange(eu_company)
  eu_company_choices_alpha <- setNames(
    eu_company_lookup_alpha$eu_company,
    paste0(eu_company_lookup_alpha$eu_company, " (", eu_company_lookup_alpha$n_facilities,
           ifelse(eu_company_lookup_alpha$n_facilities == 1, " facility)", " facilities)"))
  )
} else {
  eu_company_lookup <- NULL
  eu_company_choices_alpha <- character(0)
}

# ======================================================
# PART 0.45 -- TARGET-MATCHING STRATEGY DIAGRAM (Overview tab)
# Self-contained inline SVG (plain hex colors, no CSS variables --
# this renders in a bare browser, not a design-system-aware surface).
# NOTE: the SBTi branch is the DESIGNED strategy, not yet implemented --
# the current pipeline (satya_carbon_v4_01_data_pipeline.R) does not
# read sbti_targets.xlsx or join it to any facility. Every facility with
# a target currently gets the sector-level proxy below, 100% of the time.
# The "~6%" figure is an early, unverified estimate from project notes,
# not a computed match rate -- labeled as such in the diagram itself.
# ======================================================

target_strategy_svg <- r"[
<svg viewBox="0 0 760 320" xmlns="http://www.w3.org/2000/svg" style="width:100%; height:auto; max-width:760px; display:block; margin:0 auto;">
  <defs>
    <marker id="arrowhead" markerWidth="8" markerHeight="8" refX="6" refY="4" orient="auto">
      <path d="M0,0 L8,4 L0,8 Z" fill="#5D6D7E"/>
    </marker>
  </defs>

  <rect x="300" y="10" width="160" height="46" rx="6" fill="#ECF0F1" stroke="#7F8C8D" stroke-width="1"/>
  <text x="380" y="38" text-anchor="middle" font-size="13" fill="#2C3E50">GHGRP facility</text>

  <line x1="380" y1="56" x2="380" y2="86" stroke="#5D6D7E" stroke-width="1.2" marker-end="url(#arrowhead)"/>

  <rect x="245" y="86" width="270" height="55" rx="6" fill="#FDEBD0" stroke="#E67E22" stroke-width="1"/>
  <text x="380" y="109" text-anchor="middle" font-size="12" fill="#7E5109">Does the parent company have a</text>
  <text x="380" y="126" text-anchor="middle" font-size="12" fill="#7E5109">validated SBTi target?</text>

  <line x1="295" y1="141" x2="150" y2="180" stroke="#5D6D7E" stroke-width="1.2" marker-end="url(#arrowhead)"/>
  <text x="200" y="163" text-anchor="middle" font-size="11" fill="#5D6D7E">yes</text>
  <rect x="20" y="180" width="260" height="70" rx="6" fill="#D5F5E3" stroke="#1B5E20" stroke-width="1"/>
  <text x="150" y="212" text-anchor="middle" font-size="12" fill="#0B5345" font-weight="bold">Use company-specific SBTi target</text>
  <text x="150" y="230" text-anchor="middle" font-size="11" fill="#0B5345">~6% of facilities</text>

  <line x1="465" y1="141" x2="610" y2="180" stroke="#5D6D7E" stroke-width="1.2" marker-end="url(#arrowhead)"/>
  <text x="560" y="163" text-anchor="middle" font-size="11" fill="#5D6D7E">no</text>
  <rect x="480" y="180" width="260" height="70" rx="6" fill="#D6EAF8" stroke="#2980B9" stroke-width="1"/>
  <text x="610" y="202" text-anchor="middle" font-size="12" fill="#154360" font-weight="bold">Use sector-level proxy target</text>
  <text x="610" y="219" text-anchor="middle" font-size="11" fill="#154360">~94% of facilities (all, today)</text>
  <text x="610" y="234" text-anchor="middle" font-size="10" fill="#154360">IEA / GCCA / IAI / UNEP / EPA, confidence-graded</text>

  <line x1="150" y1="250" x2="380" y2="285" stroke="#5D6D7E" stroke-width="1.2"/>
  <line x1="610" y1="250" x2="380" y2="285" stroke="#5D6D7E" stroke-width="1.2" marker-end="url(#arrowhead)"/>
  <rect x="255" y="285" width="250" height="24" rx="4" fill="#F4F6F7" stroke="#5D6D7E" stroke-width="1"/>
  <text x="380" y="301" text-anchor="middle" font-size="11" fill="#2C3E50">Facility's target pathway (2024-2028)</text>
</svg>
]"

# ======================================================
# PART 0.45 -- SCOPE 2/3 ESTIMATION RATIOS (v1, sector-ratio stopgap)
# Duplicated from the pipeline's Part 4C (not shared code between the
# two scripts) -- Hertwich, E.G. & Wood, R. (2018), "The growing
# importance of scope 3 greenhouse gas emissions from industry",
# Environmental Research Letters 13, 104013. GHGRP only measures Scope 1
# -- this estimates Scope 2/3 via published SECTOR-LEVEL indirect/direct
# ratios, not any facility- or company-specific calculation. "Other" and
# fluorinated-GHG-equipment are deliberately left unmapped (NA), not
# assigned a fabricated ratio.
scope23_ratio_table <- tribble(
  ~primary_sector, ~ipcc_bucket, ~scope2_multiplier, ~scope3_multiplier, ~scope23_confidence,
  "Petroleum and Natural Gas Systems", "Energy", 0.040, 0.290, "Good",
  "Power Plants", "Energy", 0.040, 0.290, "Good",
  "Refineries", "Energy", 0.040, 0.290, "Good",
  "Petroleum Product Suppliers", "Energy", 0.040, 0.290, "Good",
  "Natural Gas and Natural Gas Liquids Suppliers", "Energy", 0.040, 0.290, "Good",
  "Chemicals", "Industry", 0.137, 1.003, "Good",
  "Minerals", "Industry", 0.137, 1.003, "Good",
  "Metals", "Industry", 0.137, 1.003, "Good",
  "Pulp and Paper", "Industry", 0.137, 1.003, "Good",
  "Waste", "Industry", 0.137, 1.003, "Weak (no dedicated waste sector in the paper)",
  "Industrial Gas Suppliers", "Industry", 0.137, 1.003, "Reasonable",
  "Suppliers of CO2", "Energy", 0.040, 0.290, "Assumption (not explicitly modeled)",
  "Injection of CO2", "Energy", 0.040, 0.290, "Assumption",
  "Coal-based Liquid Fuel Supply", "Energy", 0.040, 0.290, "Assumption"
)

# Returns list(scope2_multiplier, scope3_multiplier, confidence) for a
# sector, or NULL if unmapped (Other, fluorinated-GHG-equipment) --
# callers must handle NULL explicitly (show "not estimated"), never
# silently substitute 0.
get_scope23_ratio <- function(sector) {
  row <- scope23_ratio_table %>% filter(primary_sector == sector)
  if (nrow(row) == 0) return(NULL)
  list(scope2_multiplier = row$scope2_multiplier[1], scope3_multiplier = row$scope3_multiplier[1],
       confidence = row$scope23_confidence[1])
}

# ======================================================
# PART 0.5 -- SHARED HELPER: forecast-vs-target "credits" bar chart
# Positive gap (forecast > target) = credits needed, shown in red.
# Negative gap (forecast < target) = surplus/banked, shown in green.
# ======================================================

make_credit_bar <- function(df, gap_col, y_label, x_breaks_arg = scales::pretty_breaks()) {
  ggplot(df, aes(x = year, y = .data[[gap_col]], fill = .data[[gap_col]] > 0)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "#27AE60"), guide = "none") +
    scale_x_continuous(breaks = x_breaks_arg) +
    scale_y_continuous(labels = comma) +
    labs(
      subtitle = "Credits needed (red, forecast above target) vs. surplus (green, forecast below target)",
      x = NULL, y = y_label
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.subtitle = element_text(color = "grey40", size = 10))
}

# Same funding mechanism as the Portfolio Mix Engine's per-category
# allocation (ideal recipe at the target mix, scaled down to fit budget),
# extracted as a standalone function so it can run identically against
# any of the 3 target scenarios (Current Goal / Industry Standard / SBTi)
# -- the mix strategy (weights, prices, budget) stays constant; only the
# gap being sized against changes.
compute_portfolio_outcome <- function(gap_tons, weights, prices, budget) {
  if (is.na(gap_tons)) {
    return(list(gap_tons = NA_real_, funded_tons = NA_real_, funded_cost = NA_real_, coverage_pct = NA_real_))
  }
  ideal_tons       <- gap_tons * weights
  ideal_cost       <- ideal_tons * prices[names(ideal_tons)]
  ideal_total_cost <- sum(ideal_cost)

  scale <- if (gap_tons <= 0 || ideal_total_cost == 0) 1 else min(1, budget / ideal_total_cost)
  funded_tons <- floor(ideal_tons * scale)   # floor, not round -- never overshoot budget
  funded_cost <- sum(round(funded_tons * prices[names(funded_tons)]))
  total_tons  <- sum(funded_tons)
  coverage_pct <- if (gap_tons > 0) total_tons / gap_tons * 100 else 100

  list(gap_tons = gap_tons, funded_tons = total_tons, funded_cost = funded_cost, coverage_pct = coverage_pct)
}

# NOTE: charts were briefly converted to interactive plotly (hover
# tooltips), but reverted back to static ggplot/renderPlot -- a real
# crash appeared on the New Company Intake trend chart after the plotly
# conversion, and rather than keep debugging the conversion layer, the
# call was made to eliminate it and keep every other feature from this
# session intact. All the plotOutput/renderPlot chart code below is
# otherwise unchanged from the plotly version, just without the plotly
# wrapping.

# ======================================================
# PART 0.55 -- SBTi TARGET-SETTING CALCULATOR
# Mirrors SBTi's own official Excel target-setting tool's "Section 1.
# Input data" form. Self-contained: real SBTi 1.5C pathway data for the
# Power and Cement sectors (from the workbook's Database sheet) + both
# the SDA (Sectoral Decarbonization Approach) and ACA (Absolute
# Contraction Approach) calculation engines.
#
# HONEST LIMITATION, stated up front: the Excel tool's Power/Cement
# sectors ask for Scope 2 emissions separately, but it has not been
# verified what method (SDA or ACA) the real workbook applies to that
# Scope 2 figure for Power/Cement companies specifically -- only the
# Scope 1 (generation/production) SDA formulas were confirmed directly
# from the workbook's 'Calculations' sheet. This applies ACA to Scope 2
# for ALL sectors (including Power/Cement) as a reasonable default, NOT
# a verified match to the real workbook's Scope 2 treatment for those
# two sectors.
# ======================================================

sbti_calc_years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
                      2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033, 2034, 2035, 2036, 2037,
                      2038, 2039, 2040, 2041, 2042, 2043, 2044, 2045, 2046, 2047, 2048, 2049,
                      2050, 2051, 2052, 2053, 2054, 2055, 2056, 2057, 2058, 2059, 2060)

sbti_calc_power_si_1p5c <- c(0.4641194060477229, 0.45548155007407654, 0.4471262605282671, 0.439039895334845,
                              0.4312096766806805, 0.42362362364038736, 0.4162704910076618, 0.3803419140017086,
                              0.3454836180058344, 0.31164848077694857, 0.27879210632052176, 0.2468726305433339,
                              0.2158505432979085, 0.1856885252281078, 0.15635129799925593, 0.127805486649072,
                              0.10001949293022003, 0.08999210288763036, 0.08046111890159496, 0.07139056947568255,
                              0.06274787665482795, 0.05450346506389743, 0.04663042377894154, 0.03910421288511427,
                              0.03190240797972947, 0.025004477016736547, 0.018391584815132048, 0.016345941942309084,
                              0.014364751563825984, 0.012445014844972645, 0.01058391614880365, 0.008778809257216302,
                              0.007027204817229386, 0.0053267588872187134, 0.0036752624722865995, 0.0020706319505218534,
                              0.000510900302903097, 0.0004612272758917212, 0.000412620202887868, 0.00036504513602110636,
                              0.00031846955381419805, 0.00027286228704560627, 0.0002281934491884574, 0.00018443437109967264,
                              0.00014155753965940484, 9.95365400847809e-05, 5.83460016638123e-05)

sbti_calc_power_sa_1p5c <- c(24990590032.822453, 25413166756.010384, 25835743479.19831, 26258320202.38624,
                              26680896925.57417, 27103473648.762215, 27526050371.9501, 27948627095.137997,
                              28371203818.326, 28793780541.5139, 29216357264.7019, 29638933987.8898,
                              30061510711.0778, 30484087434.265697, 30906664157.4536, 31329240880.641598,
                              31751817603.8295, 32578688921.0115, 33405560238.1935, 34232431555.375607,
                              35059302872.5576, 35886174189.7396, 36713045506.9216, 37539916824.1036,
                              38366788141.2856, 39193659458.4676, 40020530775.6496, 40671508859.66129,
                              41322486943.6729, 41973465027.6846, 42624443111.696304, 43275421195.708,
                              43926399279.7196, 44577377363.7313, 45228355447.743, 45879333531.7546,
                              46530311615.766304, 47040516911.755005, 47550722207.743805, 48060927503.7326,
                              48571132799.7213, 49081338095.710106, 49591543391.6988, 50101748687.6876,
                              50611953983.6763, 51122159279.66511, 51632364575.65381)

sbti_calc_cement_se_1p5c <- c(2461, 2461, 2461, 2461, 2461, 2461, 2334, 2358.818181818182, 2307.7272727272725,
                                2256.6363636363635, 2205.5454545454545, 2154.4545454545455, 2103.3636363636365,
                                2052.272727272727, 2001.1818181818182, 1950.090909090909, 1899, 1799.7, 1700.4,
                                1601.1, 1501.8, 1402.5, 1303.2, 1203.9, 1104.6, 1005.3000000000001, 906, 828.7,
                                751.4, 674.1, 596.8, 519.5, 442.20000000000005, 364.9, 287.6, 210.30000000000007,
                                133, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

sbti_calc_cement_sa_1p5c <- c(4215000000, 4215000000, 4215000000, 4215000000, 4215000000, 4215000000, 4054000000,
                                4222818181.818182, 4226727272.727273, 4230636363.636364, 4234545454.545455,
                                4238454545.454545, 4242363636.363636, 4246272727.272727, 4250181818.181818,
                                4254090909.090909, 4258000000, 4245100000.0000005, 4232200000, 4219300000,
                                4206399999.9999995, 4193500000, 4180600000.0000005, 4167700000, 4154800000,
                                4141899999.9999995, 4129000000, 4119300000, 4109600000.0000005, 4099899999.9999995,
                                4090200000, 4080500000, 4070800000, 4061100000, 4051400000, 4041700000, 4032000000,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

# ACA engine (Absolute Contraction Approach -- flat-rate, cross-sector)
sbti_aca_engine <- function(by_year, by_e, mry_year = by_year, mry_e = by_e, ty_year,
                              nz_ambition, nz_year, min_larr) {
  if (is.na(mry_e) || mry_year == by_year) mry_e <- by_e
  if (is.na(mry_year)) mry_year <- by_year

  dlarr_mry_to_nz <- nz_ambition / (nz_year - mry_year)
  initial_ambition <- dlarr_mry_to_nz * (ty_year - mry_year)
  initial_target_e <- mry_e * (1 - initial_ambition)
  converted_ambition <- (by_e - initial_target_e) / by_e
  larr_uncapped <- converted_ambition / (ty_year - by_year)
  larr_final <- max(min_larr, larr_uncapped)

  years <- seq.int(by_year, ty_year)
  trajectory <- by_e * (1 - larr_final * (years - by_year))
  list(larr_final = larr_final, trajectory = setNames(trajectory, years))
}

# SDA engine (Sectoral Decarbonization Approach -- intensity convergence)
sbti_sda_engine <- function(sector_years, sector_intensity, sector_activity,
                              by_year, by_activity, by_intensity, ty_year, converge_year = 2050) {
  idx_by <- match(by_year, sector_years)
  idx_conv <- match(converge_year, sector_years)
  idx_ty <- match(ty_year, sector_years)
  if (any(is.na(c(idx_by, idx_conv, idx_ty))))
    stop("base_year/target_year/converge_year out of pathway data range (", min(sector_years), "-", max(sector_years), ")")

  si_by <- sector_intensity[idx_by]; si_conv <- sector_intensity[idx_conv]; sa_by <- sector_activity[idx_by]
  d <- by_intensity - si_conv

  years_range <- sector_years[idx_by:idx_ty]
  si_t <- sector_intensity[idx_by:idx_ty]
  sa_t <- sector_activity[idx_by:idx_ty]

  p_t <- (si_t - si_conv) / (si_by - si_conv)
  company_activity_t <- by_activity * (sa_t / sa_by)
  company_intensity_t <- d * p_t + si_conv
  company_emissions_t <- company_intensity_t * company_activity_t

  list(years = years_range, trajectory = setNames(company_emissions_t, years_range))
}

# Main entry point -- mirrors the Excel form fields exactly.
sbti_calculate <- function(company_name = "",
                            target_setting_method = c("Sectoral Decarbonization Approach", "Absolute Contraction Approach"),
                            sda_sector = NA,
                            base_year, base_year_activity_output = NA,
                            base_year_s1_e, base_year_s2_e = NA,
                            target_year,
                            activity_projection_type = "Fixed market share",
                            most_recent_year = NA, mry_s1_e = NA, mry_s2_e = NA,
                            net_zero_year = 2050,
                            # ---- Scope 3 inputs, all optional -- Scope 3 target omitted entirely if base_year_s3_e is NA ----
                            base_year_s3_e = NA, mry_s3_e = NA, most_recent_year_s3 = NA,
                            base_year_s3 = NA, target_year_s3 = NA,
                            s3_method = c("Cross-sector ACA", "Economic intensity", "Physical intensity"),
                            s3_ambition = c("1.5C", "WB2C"),
                            s3_base_year_output = NA) {

  target_setting_method <- match.arg(target_setting_method)
  s3_method   <- match.arg(s3_method)
  s3_ambition <- match.arg(s3_ambition)

  if (activity_projection_type != "Fixed market share")
    stop("Only 'Fixed market share' activity projection is implemented.")

  # ---- Scope 1 ----
  if (target_setting_method == "Sectoral Decarbonization Approach") {
    if (is.na(sda_sector) || is.na(base_year_activity_output))
      stop("SDA requires sda_sector ('Power' or 'Cement') and base_year_activity_output.")

    if (tolower(sda_sector) == "power") {
      s1 <- sbti_sda_engine(sbti_calc_years, sbti_calc_power_si_1p5c, sbti_calc_power_sa_1p5c,
                              base_year, base_year_activity_output, base_year_s1_e / base_year_activity_output,
                              target_year, min(net_zero_year, 2050))
    } else if (tolower(sda_sector) == "cement") {
      cement_intensity <- (sbti_calc_cement_se_1p5c * 1e6) / sbti_calc_cement_sa_1p5c
      s1 <- sbti_sda_engine(sbti_calc_years, cement_intensity, sbti_calc_cement_sa_1p5c,
                              base_year, base_year_activity_output, base_year_s1_e / base_year_activity_output,
                              target_year, min(net_zero_year, 2050))
    } else {
      stop("sda_sector must be 'Power' or 'Cement'.")
    }
    scope1_years <- s1$years
    scope1_emissions <- as.numeric(s1$trajectory)

  } else {
    # ACA -- Scope 1 track: 90% net-zero ambition, 4.2% floor
    a1 <- sbti_aca_engine(base_year, base_year_s1_e, most_recent_year, mry_s1_e, target_year,
                            nz_ambition = 0.90, nz_year = net_zero_year, min_larr = 0.042)
    scope1_years <- as.numeric(names(a1$trajectory))
    scope1_emissions <- as.numeric(a1$trajectory)
  }

  scope1_path <- data.frame(year = scope1_years, scope1_emissions = scope1_emissions)

  # ---- Scope 2 (ACA always, per the honest limitation noted at top) ----
  scope2_path <- NULL
  if (!is.na(base_year_s2_e)) {
    a2 <- sbti_aca_engine(base_year, base_year_s2_e, most_recent_year, mry_s2_e, target_year,
                            nz_ambition = 1.00, nz_year = min(net_zero_year, 2040), min_larr = 0.042)
    scope2_path <- data.frame(year = as.numeric(names(a2$trajectory)),
                                scope2_emissions = as.numeric(a2$trajectory))
  }

  # ---- Scope 3 (optional -- entirely skipped if base_year_s3_e is NA) ----
  # Source: SBTi's own 'Scope 3 Tool' sheet, replicated formula-for-formula:
  #  - Cross-sector ACA: SAME sbti_aca_engine() used for Scope 1/2 above,
  #    just with Scope 3's OWN ambition/floor parameters, which are
  #    DIFFERENT from Scope 1/2's (confirmed directly against the Excel's
  #    'Calculations' sheet, not assumed):
  #      WB2C: 75% net-zero ambition, 2.5% minimum LARR (Calculations!D195, D197)
  #      1.5C: 90% net-zero ambition, 4.2% minimum LARR (Calculations!D221, D223)
  #    The Excel computes BOTH tracks side by side (Scope 3 Tool Section 2,
  #    rows 36-37); this app asks which one to use as ITS single target
  #    line via s3_ambition, matching the "one clean answer per scope"
  #    pattern already used for Scope 1/2, rather than showing two lines.
  #  - Economic/Physical intensity: fixed 7% annual reduction rate
  #    (Calculations!D247/D252, both literal constants in the Excel, not
  #    derived from anything), compounded from target_year back to
  #    whichever is LATER of base_year or 2020 -- replicated exactly,
  #    including that asymmetry, per Calculations!D248/D259's own IF logic.
  #  - Cement SDA (Scope 3 Tool Section 5) is NOT implemented here -- it
  #    reuses the full Scope 1+2 cement intensity pathway via an HLOOKUP
  #    against the same underlying SBTi cement dataset, which is a much
  #    larger, sector-specific lift; flagged as a gap, not fabricated.
  scope3_path <- NULL
  scope3_note <- NULL

  if (!is.na(base_year_s3_e)) {
    # Scope 3's own base/target year -- genuinely separate from Scope
    # 1&2's, confirmed via the Excel's named ranges (base_year_S3 points
    # to 'Scope 3 Tool'!$D$23, target_year_S3 to $D$24 -- distinct cells
    # from 'Scope 1&2 Tool'!$D$24/$D$28). Falls back to the shared
    # base_year/target_year if not explicitly provided.
    by3 <- if (!is.na(base_year_s3)) base_year_s3 else base_year
    ty3 <- if (!is.na(target_year_s3)) target_year_s3 else target_year

    if (s3_method == "Cross-sector ACA") {
      s3_params <- if (s3_ambition == "WB2C") {
        list(nz_ambition = 0.75, min_larr = 0.025)
      } else {
        list(nz_ambition = 0.90, min_larr = 0.042)
      }
      # Scope 3's own MRY year -- also genuinely separate from Scope
      # 1&2's (most_recent_year_S3 points to 'Scope 3 Tool'!$D$28, a
      # distinct cell). Falls back to by3 if not provided (no MRY data).
      mry_year_s3 <- if (!is.na(most_recent_year_s3)) most_recent_year_s3 else by3
      a3 <- sbti_aca_engine(by3, base_year_s3_e, mry_year_s3, mry_s3_e, ty3,
                              nz_ambition = s3_params$nz_ambition, nz_year = net_zero_year,
                              min_larr = s3_params$min_larr)
      scope3_path <- data.frame(year = as.numeric(names(a3$trajectory)),
                                  scope3_emissions = as.numeric(a3$trajectory))
      scope3_note <- paste0("Cross-sector ACA (", s3_ambition, "): ",
                             s3_params$nz_ambition * 100, "% net-zero ambition by ", net_zero_year,
                             ", ", s3_params$min_larr * 100, "% minimum annual reduction floor.")

    } else if (s3_method %in% c("Economic intensity", "Physical intensity")) {
      if (is.na(s3_base_year_output) || s3_base_year_output <= 0) {
        stop("Economic/Physical intensity method requires a positive base-year output value ",
             "(revenue/value-added, or physical output units).")
      }
      annual_rate <- 0.07  # Calculations!D247 / D252 -- fixed constant in the Excel, not derived
      base_intensity <- base_year_s3_e / s3_base_year_output
      # Replicates Calculations!D248/D259's asymmetric exponent exactly:
      # base_year<=2020 uses (target_year - base_year); base_year>2020
      # uses (target_year - 2020) instead -- not (target_year - base_year)
      # -- an odd-looking but deliberate quirk of the source formula.
      exponent <- if (by3 <= 2020) (ty3 - by3) else (ty3 - 2020)
      total_reduction <- 1 - (1 - annual_rate) ^ exponent
      target_intensity <- base_intensity * (1 - total_reduction)
      years <- seq.int(by3, ty3)
      # Straight-line intensity path from base to target year, since the
      # Excel only ever states the two endpoints (base-year and
      # target-year intensity), not an intermediate trajectory.
      intensity_path <- seq(base_intensity, target_intensity, length.out = length(years))
      scope3_emissions_path <- intensity_path * s3_base_year_output
      scope3_path <- data.frame(year = years, scope3_emissions = scope3_emissions_path)
      scope3_note <- paste0(
        s3_method, ": ", scales::percent(total_reduction, accuracy = 0.1), " total intensity reduction, ",
        "base year intensity ", signif(base_intensity, 3), " tCO2e/unit -> target ",
        signif(target_intensity, 3), " tCO2e/unit. Straight-line path between endpoints ",
        "(the Excel only states the two endpoints, not an interim trajectory)."
      )
    }
  }

  # ---- Combine ----
  if (!is.null(scope2_path)) {
    combined <- merge(scope1_path, scope2_path, by = "year")
    combined$total_emissions <- combined$scope1_emissions + combined$scope2_emissions
  } else {
    combined <- scope1_path
    combined$total_emissions <- combined$scope1_emissions
  }
  if (!is.null(scope3_path)) {
    # Scope 3 can now have a genuinely different base/target year than
    # Scope 1&2 -- if the two ranges don't overlap at all, scope3_emissions
    # would be NA in every row of the merged table and silently vanish
    # from the plot with no explanation. Flag that explicitly instead.
    if (!any(scope3_path$year %in% combined$year)) {
      scope3_note <- paste0(
        scope3_note, " NOTE: Scope 3's year range (", min(scope3_path$year), "-", max(scope3_path$year),
        ") doesn't overlap AT ALL with Scope 1/2's (", min(combined$year), "-", max(combined$year),
        ") -- it won't appear on the combined chart/table below at all."
      )
    }
    combined <- merge(combined, scope3_path, by = "year", all.x = TRUE)
    combined$total_emissions <- combined$total_emissions + ifelse(is.na(combined$scope3_emissions), 0, combined$scope3_emissions)
  }

  # Explicit column order -- merge() appends scope3_emissions AFTER
  # total_emissions (since total_emissions was already created by the
  # scope1/2 merge above), which would silently mislabel the plot/table
  # if left as-is. Reorder here once, rather than relying on merge()'s
  # incidental column placement.
  col_order <- c("year", "scope1_emissions",
                 if ("scope2_emissions" %in% names(combined)) "scope2_emissions",
                 if ("scope3_emissions" %in% names(combined)) "scope3_emissions",
                 "total_emissions")
  combined <- combined[, col_order]

  list(
    company_name = company_name,
    method = target_setting_method,
    sector = if (target_setting_method == "Sectoral Decarbonization Approach") sda_sector else "other",
    scope3_note = scope3_note,
    path = combined
  )
}

# ======================================================
# PART 0.6 -- METHODOLOGY REGISTRY (real 58, per Rajat's list) +
# DEFAULT PROJECT-TYPE CATALOG derived from it
#
# methodology_registry.csv is the real registry: 58 rows (CDM ACM/AM/AMS/
# AR/TOOL, Verra VM/VMD, Puro.earth, Isometric, +1 unresolved), each
# pre-classified into mechanism / action / bucket_key by the team, with a
# confidence flag and notes. This replaces the earlier 11-row illustrative
# placeholder list entirely.
#
# bucket_key is generic (nat_avoid / nat_removal / tech_avoid /
# tech_removal / comm_avoid) -- exactly the 5 Portfolio Mix Engine slider
# keys -- so the SAME derive_bucket_key() mapping used for these 58 also
# applies to row #59, #100, #200, etc: any future methodology just needs a
# mechanism + action (or an explicit bucket_key override), and it slots
# into the right bucket without touching any downstream code.
# ======================================================

# Generic mechanism+action -> bucket_key mapping. Kept as a function (not
# inlined) so it can be reapplied to new methodologies added later, and so
# an explicit bucket_key already supplied in the registry (e.g. from a
# human classification, as in the CSV) always takes precedence over this
# derivation rather than being silently overwritten by it.
derive_bucket_key <- function(mechanism, action) {
  ifelse(mechanism == "Nature-based" & action == "Avoidance", "nat_avoid",
  ifelse(mechanism == "Nature-based" & action == "Removal", "nat_removal",
  ifelse(mechanism == "Technology-based" & action == "Avoidance", "tech_avoid",
  ifelse(mechanism == "Technology-based" & action == "Removal", "tech_removal",
  ifelse(mechanism == "Community-based", "comm_avoid", NA_character_))))
  )
}

methodology_registry_path <- "methodology_registry.csv"
if (!file.exists(methodology_registry_path)) {
  stop(
    "Missing '", methodology_registry_path, "' -- this holds the real 58-",
    "methodology registry (mechanism/action/bucket mapping) and must sit ",
    "next to this script."
  )
}

methodology_registry <- read.csv(methodology_registry_path, stringsAsFactors = FALSE)

# Trust the registry's own bucket_key where given; only derive it where
# missing (keeps future rows working even if whoever adds them forgets to
# fill in bucket_key themselves).
methodology_registry$bucket_key <- ifelse(
  !is.na(methodology_registry$bucket_key) & nzchar(methodology_registry$bucket_key),
  methodology_registry$bucket_key,
  derive_bucket_key(methodology_registry$mechanism, methodology_registry$action)
)

# Two categories of row are excluded from the CREDITABLE catalog (the one
# that feeds pricing/supply/portfolio allocation) but are NOT hidden from
# view -- see methodology_registry_excluded below, surfaced in the PCE tab:
#   - is_tool == TRUE: CDM/AR "TOOL" methodologies are supporting
#     calculation tools (baseline, additionality, leakage, etc.), not
#     standalone credit-generating methodologies. They have no price or
#     supply of their own and can't be allocated a portfolio share.
#   - bucket_key == "other": flagged in the registry's own notes as not a
#     confirmed methodology name (per Rajat) -- needs definition before it
#     can be classified into any of the 5 buckets.
methodology_registry$excluded_reason <- ifelse(
  methodology_registry$is_tool, "Supporting tool, not a standalone credit methodology",
  ifelse(methodology_registry$bucket_key == "other",
         "Needs definition before it can be bucketed (see notes)", NA_character_)
)

methodology_registry_excluded <- methodology_registry %>% filter(!is.na(excluded_reason))

creditable_methodologies <- methodology_registry %>%
  filter(is.na(excluded_reason), bucket_key %in% c("nat_avoid", "nat_removal", "tech_avoid", "tech_removal", "comm_avoid"))

# ---- REAL price extraction, where available ----
# Path is CONFIGURABLE (SATYA_PRICE_FILE env var), not hardcoded -- the
# real production file will have the identical structure, only the path
# changes. simplifyVector = FALSE is required: this file has many NULL
# prices (methodology codes not yet priced), and jsonlite's default
# simplification silently drops NULLs rather than preserving them as
# "no real price for this code yet" -- which is exactly the signal we
# need to fall back to the synthetic placeholder correctly.
price_file <- Sys.getenv("SATYA_PRICE_FILE", "methodology_prices_sample.json")

# ---- Internal-use access code (per request: gate Backtesting, Coverage
# Audit, Internal Demand Trends, and Pricing behind a single shared code
# rather than leaving them open to every visitor). Configurable via the
# SATYA_INTERNAL_CODE environment variable so the real code doesn't need
# to live in the source itself; falls back to a placeholder default for
# local testing if that variable isn't set. Client-side rendering means
# this is real friction, not real security -- a sufficiently determined
# person could still find hidden output IDs -- but it matches what was
# actually asked for: keep casual/external visitors out, not withstand
# a determined attacker.
internal_access_code <- Sys.getenv("SATYA_INTERNAL_CODE", "internal2026")

real_prices <- NULL
if (file.exists(price_file)) {
  real_prices <- tryCatch(
    jsonlite::fromJSON(price_file, simplifyVector = FALSE),
    error = function(e) {
      warning("Could not parse '", price_file, "': ", conditionMessage(e),
              " -- falling back to synthetic prices for every methodology.")
      NULL
    }
  )
  cat("Price file loaded from:", price_file, "\n")
} else {
  warning("Price file not found at '", price_file, "' (set the SATYA_PRICE_FILE ",
          "environment variable to point elsewhere) -- using synthetic prices ",
          "for every methodology.")
}

# Looks up ONE methodology's real price entry, if the file covers it AND
# it's non-NULL. Returns NULL otherwise -- the caller falls back to the
# synthetic bucket-based draw in that case, never a fabricated number
# dressed up as real. Field names below (price/supply_tons/dev_cost) are
# a reasonable guess pending confirmation of the actual file's schema --
# only $price is assumed required; the other two are optional per-entry.
get_real_price <- function(code) {
  if (is.null(real_prices) || is.null(real_prices$prices)) return(NULL)
  row <- real_prices$prices[[code]]
  if (is.null(row) || is.null(row$price)) return(NULL)
  row
}

# ---- Illustrative price / supply / geography / developer per methodology ----
# PLACEHOLDER DATA for any methodology code the real price file above does
# NOT cover -- the registry gives us real codes and bucket classification,
# but not real procurement prices/supply/geography/counterparties for
# everything (most of Rajat's list has no real price yet). Generated
# per-bucket so the LP/concentration caps have realistic-shaped data to
# bind on for uncovered codes; every cell remains editable in-app.
bucket_price_lo <- c(nat_avoid = 6,  nat_removal = 12, tech_avoid = 8,  tech_removal = 90,  comm_avoid = 6)
bucket_price_hi <- c(nat_avoid = 16, nat_removal = 28, tech_avoid = 15, tech_removal = 400, comm_avoid = 12)

# ---- GHG Protocol Scope 3 categories (per the Jul 31 sync) ----
# The 15 standard categories, per the GHG Protocol Corporate Value Chain
# (Scope 3) Standard. Every methodology in the catalog above already
# applies across all scopes -- what actually distinguishes Scope 3 work
# is WHICH of these 15 categories the emissions fall under, not a
# separate methodology set.
scope3_categories <- tibble::tribble(
  ~cat_id, ~cat_name,
  1,  "Purchased goods and services",
  2,  "Capital goods",
  3,  "Fuel- and energy-related activities",
  4,  "Upstream transportation and distribution",
  5,  "Waste generated in operations",
  6,  "Business travel",
  7,  "Employee commuting",
  8,  "Upstream leased assets",
  9,  "Downstream transportation and distribution",
  10, "Processing of sold products",
  11, "Use of sold products",
  12, "End-of-life treatment of sold products",
  13, "Downstream leased assets",
  14, "Franchises",
  15, "Investments"
)

# ---- Sector-specific Scope 3 category splits ----
# REPLACES the earlier version, which combined one real cross-industry
# figure (C1+C11 = 84%) with 13 invented category weights. Per direct
# request: no more speculative numbers for the 15 categories. This
# version uses ACTUAL published data where it exists, and is explicit
# -- not silent -- about where it doesn't.
#
# Source for Chemicals (the only sector with real numbers below): CDP
# Technical Note "Relevance of Scope 3 Categories by Sector" v3.0 (2024),
# section 2.4, analyzing 146 Chemicals companies' 2021 CDP climate
# change questionnaire responses (https://cdn.cdp.net/cdp-production/
# cms/guidance_docs/pdfs/000/003/504/original/CDP-technical-note-scope-
# 3-relevance-by-sector.pdf):
#   - Category 1 "Purchased goods and services" = 58% of Scope 3 (REAL,
#     Chemicals-specific, stated explicitly in the source)
#   - Category 11 "Use of sold products" = 19% of Scope 3 (REAL,
#     Chemicals-specific, stated explicitly)
#   - Category 6 "Business travel" = 0.10%, Category 7 "Employee
#     commuting" = 0.20% -- REAL, but a CROSS-SECTOR average stated in
#     the source's intro, not Chemicals-specific
#   - Categories 2, 3, 4, 9 are named as relevant "medium" size for
#     Chemicals (WBCSD, 2013) but the source gives NO exact percentage
#     for them -- the remaining share is split EVENLY across these 4,
#     labeled as such, rather than inventing a weighting the source
#     doesn't support
#   - Categories 5, 8, 10, 12, 13, 14, 15 are not identified as relevant
#     to Chemicals anywhere in the source -- given a small, evenly-split
#     residual rather than zero (a specific company could still have
#     some emissions here), clearly labeled as unresearched
#
# EXTENDED to every other GHGRP sector in this app that has a defensible
# CDP high-impact-sector match -- same source document, same "named
# relevant but unquantified -> split evenly" discipline as Chemicals.
# Two of the four match TYPES matter here:
#   - EXACT: this app's sector name IS one of CDP's high-impact sectors
#     (Chemicals only)
#   - APPROXIMATE: a genuinely related but not identical CDP sector is
#     used as the closest available stand-in (e.g. this app's 4
#     petroleum/gas sectors all map to CDP's single "Oil & Gas" sector;
#     "Minerals" maps to CDP's "Cement" sector, the closest mineral-
#     products category CDP covers) -- labeled as approximate everywhere
#     it's shown, never presented as if it were an exact match
#   - NONE: no defensible CDP sector exists at all (Waste, Industrial
#     Gas Suppliers, Suppliers/Injection of CO2) -- equal 1/15 split
scope3_sector_category_shares <- list(
  "Chemicals" = list(
    match_type = "exact",
    source = "CDP Technical Note (2024), Chemicals section, 146 companies' 2021 CDP data",
    shares = c(
      `1` = 0.58, `11` = 0.19, `6` = 0.0010, `7` = 0.0020,
      `2` = 0.0568, `3` = 0.0568, `4` = 0.0568, `9` = 0.0568,
      `5` = 0.00386, `8` = 0.00386, `10` = 0.00386, `12` = 0.00386,
      `13` = 0.00386, `14` = 0.00386, `15` = 0.00386
    )
  ),
  "Metals" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Metals & Mining section (\"Processing Metals\" segment) -- only Category 1 has a published figure",
    shares = local({
      rest <- (1 - 0.35) / 14
      c(`1` = 0.35, setNames(rep(rest, 14), as.character(setdiff(1:15, 1))))
    })
  ),
  "Pulp and Paper" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Paper & Forestry section, sector-wide figures",
    shares = local({
      named <- c(`1` = 0.35, `10` = 0.15, `12` = 0.19, `4` = 0.08)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Power Plants" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Electric Utilities section",
    shares = local({
      named <- c(`11` = 0.41, `3` = 0.39, `15` = 0.09)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Petroleum and Natural Gas Systems" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Oil & Gas section",
    shares = local({
      named <- c(`11` = 0.91, `1` = 0.04)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Refineries" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Oil & Gas section",
    shares = local({
      named <- c(`11` = 0.91, `1` = 0.04)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Petroleum Product Suppliers" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Oil & Gas section",
    shares = local({
      named <- c(`11` = 0.91, `1` = 0.04)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Natural Gas and Natural Gas Liquids Suppliers" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Oil & Gas section",
    shares = local({
      named <- c(`11` = 0.91, `1` = 0.04)
      rest <- (1 - sum(named)) / (15 - length(named))
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Coal-based Liquid Fuel Supply" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Coal section",
    shares = local({
      named <- c(`11` = 0.98)
      rest <- (1 - sum(named)) / 14
      full <- setNames(rep(rest, 15), as.character(1:15))
      full[names(named)] <- named
      full
    })
  ),
  "Minerals" = list(
    match_type = "approximate",
    source = "CDP Technical Note (2024), Cement section -- closest mineral-products sector CDP covers, only Category 1 quantified",
    shares = local({
      rest <- (1 - 0.39) / 14
      c(`1` = 0.39, setNames(rep(rest, 14), as.character(setdiff(1:15, 1))))
    })
  )
)

# Returns list(shares = named vector cat_id -> share summing to 1,
# sourced = TRUE if ANY real data anchors this sector (exact OR
# approximate match), match_type = "exact"/"approximate"/"none", source
# = citation string for display. Always call this instead of reading a
# static illustrative_share column -- the correct split depends on which
# sector is selected, and callers should show match_type/source so an
# approximate mapping is never presented as if it were exact.
get_scope3_category_shares <- function(sector) {
  if (!is.null(sector) && sector %in% names(scope3_sector_category_shares)) {
    entry <- scope3_sector_category_shares[[sector]]
    shares <- entry$shares
    full <- setNames(rep(0, 15), as.character(1:15))
    full[names(shares)] <- shares
    list(shares = full / sum(full), sourced = TRUE, match_type = entry$match_type, source = entry$source)
  } else {
    list(
      shares = setNames(rep(1 / 15, 15), as.character(1:15)), sourced = FALSE,
      match_type = "none", source = "No CDP high-impact-sector equivalent found"
    )
  }
}

set.seed(42)  # reproducible illustrative placeholders, not real market data
dev_choices <- paste0("DevCo ", LETTERS[1:10])

# All projects kept to the USA -- per request, dropped the international
# geography spread so every project genuinely has a real state (needed
# for both proximity matching AND the facility/project map below; a
# coarse continent label like "Southeast Asia" can't be plotted as a
# point on a US map or matched to a specific facility state).
us_state_choices <- c(
  "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Florida", "Georgia",
  "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Michigan",
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Mexico",
  "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

# Census region per state -- replaces the old continent-level "geography"
# field's role in the 40% concentration cap (now "no single US region
# over 40%" instead of "no single continent") -- a more meaningful grain
# now that everything's US-based anyway.
us_region_lookup <- c(
  "Connecticut" = "Northeast", "Maine" = "Northeast", "Massachusetts" = "Northeast",
  "New Hampshire" = "Northeast", "New Jersey" = "Northeast", "New York" = "Northeast",
  "Pennsylvania" = "Northeast", "Rhode Island" = "Northeast", "Vermont" = "Northeast",
  "Illinois" = "Midwest", "Indiana" = "Midwest", "Iowa" = "Midwest", "Kansas" = "Midwest",
  "Michigan" = "Midwest", "Minnesota" = "Midwest", "Missouri" = "Midwest",
  "Nebraska" = "Midwest", "North Dakota" = "Midwest", "Ohio" = "Midwest",
  "South Dakota" = "Midwest", "Wisconsin" = "Midwest",
  "Alabama" = "South", "Arkansas" = "South", "Florida" = "South", "Georgia" = "South",
  "Kentucky" = "South", "Louisiana" = "South", "Mississippi" = "South",
  "North Carolina" = "South", "Oklahoma" = "South", "South Carolina" = "South",
  "Tennessee" = "South", "Texas" = "South", "Virginia" = "South", "West Virginia" = "South",
  "Arizona" = "West", "California" = "West", "Colorado" = "West", "Idaho" = "West",
  "Montana" = "West", "Nevada" = "West", "New Mexico" = "West", "Oregon" = "West",
  "Utah" = "West", "Washington" = "West", "Wyoming" = "West"
)

# Approximate state centroids (lat/lon) for the facility/project map --
# standard reference points, not precise geographic centroids, but close
# enough for a state-level dot map.
us_state_centroids <- tribble(
  ~state, ~lat, ~lon,
  "Alabama", 32.8, -86.8, "Arizona", 34.2, -111.9, "Arkansas", 34.9, -92.4,
  "California", 37.2, -119.7, "Colorado", 39.0, -105.5, "Florida", 28.6, -82.4,
  "Georgia", 32.9, -83.4, "Idaho", 44.4, -114.6, "Illinois", 40.0, -89.2,
  "Indiana", 39.9, -86.3, "Iowa", 42.0, -93.5, "Kansas", 38.5, -98.4,
  "Kentucky", 37.5, -85.3, "Louisiana", 31.0, -92.0, "Michigan", 44.3, -85.4,
  "Minnesota", 46.3, -94.3, "Mississippi", 32.7, -89.7, "Missouri", 38.5, -92.5,
  "Montana", 47.0, -109.6, "Nebraska", 41.5, -99.8, "Nevada", 39.3, -116.6,
  "New Mexico", 34.4, -106.1, "New York", 42.9, -75.5, "North Carolina", 35.5, -79.1,
  "North Dakota", 47.5, -100.5, "Ohio", 40.3, -82.7, "Oklahoma", 35.5, -97.5,
  "Oregon", 44.0, -120.5, "Pennsylvania", 40.9, -77.8, "South Carolina", 33.9, -80.9,
  "South Dakota", 44.4, -100.2, "Tennessee", 35.9, -86.4, "Texas", 31.5, -99.3,
  "Utah", 39.3, -111.7, "Virginia", 37.5, -78.9, "Washington", 47.4, -120.5,
  "West Virginia", 38.6, -80.6, "Wisconsin", 44.6, -89.9, "Wyoming", 43.0, -107.5
)

# ======================================================
# GLOBAL GEOGRAPHY (Phase 1 -- country-level precision) ----
# Extends the US-only geography above to a genuine global framework,
# per the mandate to support international buyers/projects. Country is
# the finest precision available outside the US (no free global source
# matches US county-level granularity -- Phase 2, if pursued, would add
# state/province precision for specific countries via rnaturalearth's
# ne_states(), which needs the GitHub-only rnaturalearthhires package;
# deliberately deferred to keep this phase CRAN-only and deploy-clean).
# The US keeps its EXISTING state/county structures above, completely
# unchanged -- this section is purely additive.
# ======================================================

# One representative set of countries per the requested regions. Europe
# and "the Middle East (Gulf countries)" aren't themselves countries, so
# each is represented by its major real economies rather than invented
# as a single unit -- same principle as the US being 50 real states, not
# one "North America" blob.
global_country_list <- c(
  "United States",
  "Germany", "France", "Netherlands", "Spain", "Italy", "Poland",   # Europe
  "United Kingdom",
  "India", "China", "Japan", "Singapore", "Australia",
  "Brazil", "Chile", "Argentina", "Uruguay",
  "Saudi Arabia", "United Arab Emirates", "Qatar",                  # Gulf
  "South Africa", "Morocco"                                         # Africa
)

# Country -> macro-region, plays the SAME role internationally that
# us_region_lookup plays domestically: the "same region" proximity tier
# and an input to the concentration-cap logic.
country_region_lookup <- c(
  "United States" = "North America",
  "Germany" = "Europe", "France" = "Europe", "Netherlands" = "Europe",
  "Spain" = "Europe", "Italy" = "Europe", "Poland" = "Europe", "United Kingdom" = "Europe",
  "India" = "Asia", "China" = "Asia", "Japan" = "Asia", "Singapore" = "Asia", "Australia" = "Australia",
  "Brazil" = "Latin America", "Chile" = "Latin America", "Argentina" = "Latin America", "Uruguay" = "Latin America",
  "Saudi Arabia" = "Middle East", "United Arab Emirates" = "Middle East", "Qatar" = "Middle East",
  "South Africa" = "Africa", "Morocco" = "Africa"
)

# Approximate country centroids (lat/lon) -- same role as
# us_state_centroids, one level up: standard reference points for a
# country-level dot on the world map, not precise geographic centroids.
country_centroids <- tribble(
  ~country, ~lat, ~lon,
  "United States", 39.8, -98.6,
  "Germany", 51.2, 10.4, "France", 46.6, 2.2, "Netherlands", 52.1, 5.3,
  "Spain", 40.0, -3.7, "Italy", 42.8, 12.6, "Poland", 51.9, 19.1,
  "United Kingdom", 54.0, -2.5,
  "India", 22.0, 79.0, "China", 35.0, 103.0, "Japan", 36.5, 138.2,
  "Singapore", 1.35, 103.8, "Australia", -25.0, 134.0,
  "Brazil", -10.8, -52.9, "Chile", -35.7, -71.5, "Argentina", -35.4, -65.2, "Uruguay", -32.8, -56.0,
  "Saudi Arabia", 24.0, 45.0, "United Arab Emirates", 23.8, 54.3, "Qatar", 25.3, 51.2,
  "South Africa", -30.0, 25.0, "Morocco", 32.0, -5.0
)

# Returns TRUE for the one country that still has the detailed US
# state/county structures available; every other country only has
# country-level precision in this phase. Centralized so the rest of the
# code checks this once, consistently, rather than string-comparing
# "United States" in a dozen different places.
has_subnational_data <- function(country) identical(country, "United States")

# ======================================================
# SYNTHETIC COUNTRY DATA -- Chile, Brazil, Australia, India, Japan,
# South Africa, Morocco, Singapore
# ======================================================
# No real facility-level source exists yet for these 8 countries
# (unlike US GHGRP and EU EPRTR, both real regulatory data). Per
# explicit request, generated as SYNTHETIC country-level annual
# emissions -- illustrative placeholders for the regional Portfolio
# Mix tabs, NEVER presented as real reported figures. Every output
# built on this is labeled "Synthetic" so it's never confused with
# the real US/EU data sitting alongside it.
#
# anchor_mt values are rough real-world-SCALE anchors (order of
# magnitude only, not sourced figures) purely so the synthetic walk
# doesn't produce an absurd number -- Chile's ~95Mt vs India's ~2800Mt
# reflects the real difference in economy size, even though neither
# specific number is itself real or sourced. South Africa's coal-heavy
# grid puts it well above Morocco's much smaller economy; Singapore's
# petrochemical/refining sector puts it above what its small size
# alone would suggest.
synthetic_row_countries <- tribble(
  ~country,        ~anchor_mt, ~seed,
  "Chile",         95,         501,
  "Brazil",        550,        502,
  "Australia",     420,        503,
  "India",         2800,       504,
  "Japan",         1050,       505,
  "South Africa",  440,        506,
  "Morocco",       85,         507,
  "Singapore",     52,         508
)

synthetic_sector_list <- c("Energy", "Industry", "Manufacturing", "Mining", "Other")

# Base R + dplyr only (lapply + bind_rows) -- this app loads each
# package it needs individually rather than the whole tidyverse
# bundle, and purrr was never one of them.
synthetic_country_year <- bind_rows(lapply(seq_len(nrow(synthetic_row_countries)), function(i) {
  row <- synthetic_row_countries[i, ]
  set.seed(row$seed)
  yrs <- 2015:2024
  walk <- cumprod(1 + rnorm(length(yrs), mean = -0.01, sd = 0.025))  # mild decline + real noise, reproducible
  tibble(country = row$country, year = yrs, emissions_mt = round(row$anchor_mt * walk, 1))
}))

synthetic_country_sector <- bind_rows(lapply(seq_len(nrow(synthetic_row_countries)), function(i) {
  row <- synthetic_row_countries[i, ]
  set.seed(row$seed + 1000)
  s <- runif(length(synthetic_sector_list))
  tibble(country = row$country, sector = synthetic_sector_list, share = s / sum(s))
})) %>%
  left_join(synthetic_country_year, by = "country") %>%
  mutate(emissions_mt = emissions_mt * share) %>%
  select(country, year, sector, emissions_mt)

# ---- Unified global country-level table -- combines whatever real
# data exists (US GHGRP, EU EPRTR) with the synthetic countries above,
# every row explicitly tagged with its actual data_source. Nothing
# downstream should EVER drop this tag -- any table or chart built on
# global_country_emissions must keep showing which rows are real and
# which are illustrative.
global_country_emissions <- bind_rows(
  ghgp_panel_filtered %>%
    group_by(year) %>%
    summarise(emissions_mt = sum(emissions, na.rm = TRUE) / 1e6, .groups = "drop") %>%
    mutate(country = "United States", data_source = "Real (US GHGRP)"),
  if (has_eu_data) {
    hist_by_country_eu %>% transmute(country, year, emissions_mt = emissions / 1e6, data_source = "Real (EU EPRTR, approx. Scope 1)")
  } else {
    tibble(country = character(0), year = integer(0), emissions_mt = numeric(0), data_source = character(0))
  },
  synthetic_country_year %>% transmute(country, year, emissions_mt, data_source = "Synthetic (illustrative)")
)

global_country_list_available <- sort(unique(global_country_emissions$country))

# ---- County-level location (per request: state alone is too coarse for
# real proximity decisions) ----
# County polygon data, computed ONCE here (not inside the map-render
# function, which would recompute this ~3,000-polygon dataset on every
# single render) -- reused both for the centroid lookup below AND as the
# actual county BOUNDARY layer on the facility/project maps.
us_county_map_data <- ggplot2::map_data("county")

# REAL county names AND centroids, both derived from the SAME single
# source (ggplot2::map_data("county")) -- NOT cross-referenced against a
# second source (maps::county.fips) for names. That two-source version
# had a real bug: county.fips's polyname strings and map_data("county")'s
# own subregion strings aren't guaranteed to match exactly for multi-word
# county names (e.g. "King and Queen"), so an inner_join between them
# could silently drop counties that then failed to color on the map --
# exactly what happened. Using ONE source for both the lookup table and
# the map's own fill-join guarantees the strings can never mismatch.
us_county_lookup <- us_county_map_data %>%
  group_by(region, subregion) %>%
  summarise(lat = mean(range(lat)), lon = mean(range(long)), .groups = "drop") %>%
  transmute(
    state  = tools::toTitleCase(region),
    county = tools::toTitleCase(subregion),
    lat, lon
  ) %>%
  filter(state %in% us_state_choices) %>%
  distinct(state, county, .keep_all = TRUE)

methodology_base <- creditable_methodologies %>%
  transmute(
    project_type     = title_short,
    mechanism        = mechanism,
    action           = action,
    methodology_code = methodology_code,
    country          = sample(global_country_list, n(), replace = TRUE),
    developer        = sample(dev_choices, n(), replace = TRUE),
    buyer_price      = round(runif(n(), bucket_price_lo[bucket_key], bucket_price_hi[bucket_key])),
    dev_cost         = round(buyer_price * runif(n(), 0.45, 0.65)),
    # DAC/OAE are deliberately scarce and expensive, reflecting real-world
    # technology readiness today -- overridden after the generic draw above
    supply_tons      = round(runif(n(), 1e5, 3e6)),
    key              = bucket_key,
    confidence       = confidence,
    notes            = notes,
    price_source     = "Synthetic (illustrative)"
  ) %>%
  mutate(
    # State only populated for US-located methodologies -- every other
    # country has country-level precision only in this phase (Phase 1),
    # per has_subnational_data()'s own definition of what "detailed"
    # means right now.
    state     = ifelse(country == "United States", sample(us_state_choices, n(), replace = TRUE), NA_character_),
    geography = country_region_lookup[country]
  )

# Overlay REAL prices wherever price_file actually covers a code -- done
# row by row, not a bulk replace, so partial coverage (most codes NULL, a
# handful real) works correctly without disturbing the synthetic rows.
# Applied at the METHODOLOGY level (before exploding into project
# instances below) -- a real price anchors the methodology's baseline;
# individual project instances then vary around it.
n_real_priced <- 0
for (i in seq_len(nrow(methodology_base))) {
  code <- methodology_base$methodology_code[i]
  real <- get_real_price(code)
  if (!is.null(real)) {
    methodology_base$buyer_price[i]  <- real$price
    if (!is.null(real$supply_tons)) methodology_base$supply_tons[i] <- real$supply_tons
    if (!is.null(real$dev_cost))    methodology_base$dev_cost[i]    <- real$dev_cost
    methodology_base$price_source[i] <- "Real (price file)"
    n_real_priced <- n_real_priced + 1
  }
}
cat("Methodologies with a real price from", price_file, ":", n_real_priced,
    "of", nrow(methodology_base), "\n")

scarce_override <- c("Isometric-DAC" = 550, "Isometric-OAE" = 220)
for (mc in names(scarce_override)) {
  # Only applies to rows STILL on the synthetic placeholder -- a real
  # price from the file for these same codes takes precedence and is not
  # overwritten by this older scarcity assumption.
  hit <- methodology_base$methodology_code == mc &
    methodology_base$price_source == "Synthetic (illustrative)"
  methodology_base$buyer_price[hit] <- scarce_override[mc]
  methodology_base$dev_cost[hit]    <- round(scarce_override[mc] * 0.6)
  methodology_base$supply_tons[hit] <- 60000
}

# ---- Explode each METHODOLOGY into PROJECT instances ----
# Per Avishkar's framing (Jul 31 sync): a methodology (e.g. "Improved
# Forest Management") is the same everywhere; the PROJECTS that actually
# deliver it are location-specific, each with its own price/supply. The
# optimizer needs to be able to choose between a Texas instance and an
# Oregon instance of the SAME methodology -- which a 1-methodology-1-row
# catalog can't represent at all. This is now genuinely project-level:
# methodology_code stays shared across instances (for the methodology-
# level rollup and the 30% methodology concentration cap), while
# project_id and project_type become instance-specific. Kept US-only (no
# international projects) so every instance has a real state -- needed
# for both proximity matching and the facility/project map.
N_PROJECT_INSTANCES <- 2
methodology_base$methodology_name <- methodology_base$project_type

# Illustrative project-name generator -- real carbon projects have their
# own brand names (e.g. "Big Bend Forest Trust"), not just "<Methodology>
# (<State>)". These are SYNTHETIC combinations (place-style word + a
# suffix matching the project's own bucket), clearly not real registered
# project names, but structured the way real ones actually read --
# methodology_name is kept as its own separate field either way, so the
# methodology-level rollup is never confused by the display name.
project_name_prefixes <- c(
  "Cascade", "Meadowbrook", "Blue Ridge", "Prairie Wind", "Sunrise", "Evergreen",
  "Copper Creek", "Silver Lake", "Golden Valley", "Timberline", "Red Rock",
  "Cedar Hollow", "Willow Bend", "Stone Ridge", "Deep River", "High Plains",
  "Coastal Bend", "Pine Bluff", "Blackwater", "Green Mountain", "Sweetgrass",
  "Bear Creek", "Sandhill", "Ironwood", "Clearwater", "Falcon Ridge",
  "Whitetail", "Amber Fields", "Slate Canyon", "Juniper Basin"
)
project_name_suffix <- c(
  nat_avoid = "Conservation Initiative", nat_removal = "Restoration Project",
  tech_avoid = "Clean Energy Program", tech_removal = "Carbon Capture Facility",
  comm_avoid = "Community Energy Cooperative"
)

project_catalog_default <- methodology_base %>%
  slice(rep(seq_len(n()), each = N_PROJECT_INSTANCES)) %>%
  group_by(methodology_code) %>%
  mutate(
    instance_num = row_number(),
    # Each instance gets its OWN country, distinct from its siblings --
    # same principle the original US-only version used for state (e.g.
    # one "Improved Forest Management" instance in Brazil, another in
    # Chile, rather than every instance of a methodology stuck in one
    # place).
    country = sample(global_country_list, n()),
    state = ifelse(country == "United States", sample(us_state_choices, n()), NA_character_),
    geography = country_region_lookup[country],
    # Price varies +/-15% per instance -- real regional cost differences
    # (labor, land, logistics, local regulatory overhead) for the SAME
    # methodology delivered in different places, anchored to the
    # methodology's own real-or-synthetic base price either way, not a
    # fabricated number.
    buyer_price = round(buyer_price * runif(n(), 0.85, 1.15)),
    dev_cost    = round(buyer_price * runif(n(), 0.45, 0.65)),
    # Total methodology supply split across its instances via randomized
    # shares (not an even divide) so instances aren't artificially
    # identical in size.
    supply_tons = {
      shares <- runif(n())
      shares <- shares / sum(shares)
      round(first(supply_tons) * shares)
    },
    project_name = paste0(sample(project_name_prefixes, n()), " ", project_name_suffix[key])
  ) %>%
  ungroup() %>%
  # County assignment happens OUTSIDE the grouped mutate above (needs a
  # per-row lookup against us_county_lookup by each row's own state,
  # which dplyr's rowwise-free vectorized sample() can't do cleanly
  # inside a group_by(methodology_code) block where "state" varies per
  # row within the group). US-only -- has_subnational_data() defines
  # exactly which country still gets this level of precision in Phase 1.
  rowwise() %>%
  mutate(
    county = if (has_subnational_data(country)) {
      opts <- us_county_lookup$county[us_county_lookup$state == state]
      if (length(opts) == 0) NA_character_ else sample(opts, 1)
    } else {
      NA_character_
    }
  ) %>%
  ungroup() %>%
  mutate(
    project_id   = paste0(methodology_code, "-", instance_num),
    project_type = paste0(
      project_name, " (",
      case_when(
        !is.na(county) ~ paste0(county, " County, ", state),
        !is.na(state)  ~ state,
        TRUE           ~ country
      ),
      ")"
    )
  ) %>%
  select(-instance_num, -project_name) %>%
  as.data.frame()

# ---- Scope-3-category-RESTRICTED interventions (per follow-up request) ----
# Every row above is scope-AGNOSTIC -- a market carbon credit offsets a
# ton regardless of which scope generated it, which is how real carbon
# markets actually work. These rows are different in kind, not degree:
# operational programs tied to ONE specific part of the value chain (a
# business-travel reduction program can only ever reduce Category 6
# emissions, by definition -- it isn't a market credit and can't stand
# in for a Scope 1 reduction). Tagged with applicable_scope so the LP
# can enforce that restriction below; every row above gets "any".
project_catalog_default$applicable_scope <- "any"

scope3_interventions <- tribble(
  ~project_type,                               ~cat_id, ~buyer_price, ~supply_tons, ~state,       ~county,
  "Supplier Engagement & Efficiency Program",   1,       9,            40000,        "Illinois",   "Cook",
  "Upstream Transportation Route Optimization", 4,       6,            25000,        "Texas",      "Harris",
  "Business Travel Reduction Program",          6,       4,            15000,        "New York",   "New York",
  "Employee Commuting Incentive Program",       7,       3,            12000,        "California", "Los Angeles",
  "Product Use-Phase Efficiency Redesign",      11,      11,           60000,        "Michigan",   "Wayne"
) %>%
  left_join(scope3_categories %>% select(cat_id, cat_name), by = "cat_id") %>%
  transmute(
    project_type      = project_type,
    mechanism         = "Technology-based",
    action            = "Avoidance",
    methodology_code  = paste0("INTERVENTION-S3C", cat_id),
    methodology_name  = project_type,
    geography         = us_region_lookup[state],
    developer         = "Internal Program",
    buyer_price       = buyer_price,
    dev_cost          = round(buyer_price * 0.5),
    supply_tons       = supply_tons,
    key               = "tech_avoid",
    confidence        = "User-added",
    notes             = paste0(
      "Scope 3 Category ", cat_id, " (", cat_name, ") ONLY -- an internal value-chain ",
      "program, not a market credit. Cannot be counted toward any other scope or category."
    ),
    state             = state,
    county            = county,
    country           = "United States",
    project_id        = paste0("INTERVENTION-S3C", cat_id, "-1"),
    applicable_scope  = paste0("scope3_cat", cat_id)
  )

project_catalog_default <- bind_rows(project_catalog_default, scope3_interventions)

# ---- SYNTHETIC price history, per methodology ----
# PLACEHOLDER DATA -- a random walk over the last 24 months, for each
# methodology, CONSTRUCTED TO END EXACTLY AT that methodology's current
# buyer_price above. This is deliberate: the "latest" point in this
# history and the price actually used by the Portfolio Curation Engine
# must be the same number by construction, not two independently-drawn
# values that could silently disagree. When a real price time series
# exists, replace this whole block -- the shape (methodology_code, date,
# price) is what the Pricing tab and the catalog both expect.
set.seed(99)
price_history_months <- 24
price_history_dates  <- rev(seq(Sys.Date(), by = "-1 month", length.out = price_history_months))

methodology_price_history <- bind_rows(lapply(seq_len(nrow(project_catalog_default)), function(i) {
  end_price <- project_catalog_default$buyer_price[i]
  # Random walk backward from the current price -- monthly % steps, then
  # reversed so the series reads left-to-right ending at "today".
  steps     <- rnorm(price_history_months - 1, mean = 0, sd = 0.035)
  path_back <- end_price / cumprod(1 + steps)
  prices    <- rev(c(end_price, path_back))

  data.frame(
    project_id        = project_catalog_default$project_id[i],
    methodology_code = project_catalog_default$methodology_code[i],
    project_type      = project_catalog_default$project_type[i],
    key               = project_catalog_default$key[i],
    date              = price_history_dates,
    # floor at 50% of current, avoiding a stray near-zero random walk value
    price             = round(pmax(prices, 0.5 * end_price), 2),
    stringsAsFactors  = FALSE
  )
}))

cat("Synthetic price history built:", n_distinct(methodology_price_history$methodology_code),
    "methodologies x", price_history_months, "months\n\n")

# ======================================================
# PART 0.7 -- SM2 LINEAR-PROGRAM ALLOCATOR (lpSolve)
# Real constrained optimization. Three binding constraints per the
# transcript: budget, emissions gap, and per-methodology available supply
# -- the LP will never recommend more of any one methodology than its
# supply ceiling, regardless of how attractive its price/weight is.
# Concentration caps follow PCE v3.1 Sec.5 / Build-Out Plan Sec.6.3 exactly:
# Project 20% / Methodology 30% / Geography 40% / Developer 25% of total
# portfolio tons.
#
# IMPORTANT: the objective below is "category-weighted tons covered", NOT
# the spec's "risk-adjusted tonnage" (RAT). RAT requires SM3 survival/
# reversal discounting, which this app does not implement. Do not present
# this optimizer's output as risk-adjusted.
# ======================================================

# One row per group in `groups`: sum(x_i in group) - cap * sum(all x_j) <= 0
build_share_cap_constraints <- function(groups, cap, n) {
  ug <- unique(groups)
  mat <- matrix(0, nrow = length(ug), ncol = n)
  for (gi in seq_along(ug)) {
    in_group <- groups == ug[gi]
    mat[gi, in_group] <- mat[gi, in_group] + 1
    mat[gi, ] <- mat[gi, ] - cap
  }
  mat
}

# Solves: maximize sum(obj_weights_i * x_i)
#   s.t. sum(x_i) <= gap_tons
#        sum(x_i) >= tier_min_frac * gap_tons          (if tier_min_frac > 0)
#        sum(price_i * x_i) <= budget                   (if budget is not NULL)
#        x_i <= supply_tons_i                            (per-methodology supply ceiling)
#        project / methodology / geography / developer concentration caps
#        sum(x_i in bucket) <= bucket_cap * sum(all x_j)  (if bucket_cap is not NULL --
#          diversification across the 5 Nature/Technology/Community buckets, e.g. no
#          single bucket allowed to dominate the recommended portfolio. This REPLACES
#          manually-set bucket percentage sliders: the optimizer picks whatever mix of
#          the 58 methodologies actually minimizes the gap within budget, this cap just
#          keeps it from concentrating entirely in one bucket)
#        sum(x_i where applicable_scope == cat) <= category_caps[cat]  (per category_caps --
#          a Scope-3-category-RESTRICTED intervention (applicable_scope != "any") can
#          never be credited beyond that category's OWN gap ceiling, since buying more
#          of it wouldn't count toward anything else. Scope-agnostic rows (applicable_scope
#          == "any") are never capped by this -- they can always flexibly cover whatever's
#          left of the total gap_tons.)
#        x_i >= 0
# Returns list(status, tons). status == 0 means solved; anything else means
# infeasible (most commonly: the tier floor can't be met within budget,
# supply, the concentration caps, and the bucket diversification cap).
solve_portfolio_lp <- function(catalog, gap_tons, obj_weights, budget = NULL, tier_min_frac = 0, bucket_cap = NULL, category_caps = NULL) {
  n <- nrow(catalog)
  if (n == 0 || gap_tons <= 0) {
    return(list(status = 0, tons = rep(0, n)))
  }

  mat_list <- list()
  dir_list <- character(0)
  rhs_list <- numeric(0)

  # total tons can't exceed the gap
  mat_list[[length(mat_list) + 1]] <- matrix(1, nrow = 1, ncol = n)
  dir_list <- c(dir_list, "<=")
  rhs_list <- c(rhs_list, gap_tons)

  # claim-tier minimum coverage
  if (tier_min_frac > 0) {
    mat_list[[length(mat_list) + 1]] <- matrix(1, nrow = 1, ncol = n)
    dir_list <- c(dir_list, ">=")
    rhs_list <- c(rhs_list, tier_min_frac * gap_tons)
  }

  # budget (hard constraint per spec)
  if (!is.null(budget)) {
    mat_list[[length(mat_list) + 1]] <- matrix(catalog$buyer_price, nrow = 1, ncol = n)
    dir_list <- c(dir_list, "<=")
    rhs_list <- c(rhs_list, budget)
  }

  # per-project supply ceiling -- 1e12 sentinel for any row missing a
  # real supply figure (treated as effectively unconstrained, not zero)
  supply_vec <- ifelse(is.na(catalog$supply_tons), 1e12, catalog$supply_tons)
  supply_mat <- diag(n)

  # project_type is now a SPECIFIC LOCATED INSTANCE (e.g. "Improved Forest
  # Management (Texas)"), not the methodology itself -- methodology_code
  # is what's shared across a methodology's multiple project instances.
  # So these two caps now do genuinely different jobs: proj_mat (20%)
  # limits reliance on any ONE specific project; meth_mat (30%) limits
  # reliance on any ONE methodology's combined total across ALL its
  # project instances together.
  proj_mat <- build_share_cap_constraints(catalog$project_type, 0.20, n)
  meth_mat <- build_share_cap_constraints(catalog$methodology_code, 0.30, n)
  geo_mat  <- build_share_cap_constraints(catalog$geography, 0.40, n)
  dev_mat  <- build_share_cap_constraints(catalog$developer, 0.25, n)

  extra_mats <- list(supply_mat, proj_mat, meth_mat, geo_mat, dev_mat)
  extra_dirs <- c(rep("<=", n), rep("<=", nrow(proj_mat)), rep("<=", nrow(meth_mat)),
                   rep("<=", nrow(geo_mat)), rep("<=", nrow(dev_mat)))
  extra_rhs  <- c(supply_vec, rep(0, nrow(proj_mat)), rep(0, nrow(meth_mat)),
                   rep(0, nrow(geo_mat)), rep(0, nrow(dev_mat)))

  if (!is.null(bucket_cap)) {
    bucket_mat <- build_share_cap_constraints(catalog$key, bucket_cap, n)
    extra_mats <- c(extra_mats, list(bucket_mat))
    extra_dirs <- c(extra_dirs, rep("<=", nrow(bucket_mat)))
    extra_rhs  <- c(extra_rhs, rep(0, nrow(bucket_mat)))
  }

  # Category-restricted rows (applicable_scope != "any") can never be
  # credited beyond their own category's gap ceiling -- one row of
  # "sum(x_i in that category) <= cap" per category actually present in
  # category_caps. Categories with no restricted rows in the catalog
  # (or a cap of 0/NA) are simply skipped, not zeroed out incorrectly.
  if (!is.null(category_caps) && length(category_caps) > 0) {
    cat_rows <- lapply(names(category_caps), function(lbl) {
      row <- matrix(0, nrow = 1, ncol = n)
      in_cat <- catalog$applicable_scope == lbl
      if (any(in_cat)) row[1, in_cat] <- 1
      row
    })
    valid <- sapply(seq_along(cat_rows), function(i) any(cat_rows[[i]] > 0))
    if (any(valid)) {
      cat_mat <- do.call(rbind, cat_rows[valid])
      extra_mats <- c(extra_mats, list(cat_mat))
      extra_dirs <- c(extra_dirs, rep("<=", nrow(cat_mat)))
      extra_rhs  <- c(extra_rhs, unname(category_caps[valid]))
    }
  }

  const_mat <- do.call(rbind, c(mat_list, extra_mats))
  const_dir <- c(dir_list, extra_dirs)
  const_rhs <- c(rhs_list, extra_rhs)

  sol <- lpSolve::lp("max", obj_weights, const_mat, const_dir, const_rhs)

  list(status = sol$status, tons = if (sol$status == 0) sol$solution else rep(0, n))
}

# ======================================================
# PART 0.8 -- OVERVIEW SECTION-BOX HELPER
# Wraps a chunk of Overview content in a colored card so the three
# Sub-Model 0 sections (Objectives / Forecasting / Gap Estimation) are
# visually distinct at a glance, not just separated by headings.
# ======================================================

section_box <- function(title, bg, border, ...) {
  tags$div(
    style = paste0(
      "background:", bg, "; border-left: 6px solid ", border, "; ",
      "border-radius: 8px; padding: 1.25rem 1.75rem; margin-bottom: 1.75rem;"
    ),
    h3(title, style = paste0("color:", border, "; margin-top: 0;")),
    ...
  )
}

# "All funded projects (global)" panel -- now the rotating globe
# everywhere it appears, replacing the static world map. A uiOutput
# wrapper (not a direct plotlyOutput) since plotly may not be
# installed -- the wrapper renders the real interactive globe when it
# is, or a plain message when it isn't, per wire_globe_output() below.
globe_panel_ui <- function(wrapper_id) {
  column(width = 6, tags$b("All funded projects (global)"), uiOutput(wrapper_id))
}

# ---- Internal-use gate ----
# Wraps a block of tab content so it only renders once the shared
# internal_unlocked flag is set (see the server-side observer that
# checks the access code). Everything is gated the same way, one shared
# unlock for all four restricted areas (Backtesting, Coverage Audit,
# Internal Demand Trends, Pricing) rather than a separate code per tab --
# enter it once, it applies everywhere.
internal_gate <- function(...) {
  tagList(
    conditionalPanel(
      condition = "input.internal_unlocked == false",
      tags$div(
        style = "background:#FDEDEC; border:2px solid #E74C3C; border-radius:8px; padding:1.5rem; margin:1rem 0; text-align:center;",
        tags$h4(style = "color:#C0392B; margin-top:0;", "\U0001F512 INTERNAL USE ONLY"),
        tags$p("This section is restricted. Enter the access code to view it."),
        tags$div(
          style = "max-width:320px; margin:0 auto;",
          passwordInput("internal_code_entry", NULL, placeholder = "Access code"),
          actionButton("internal_unlock_btn", "Unlock", class = "btn-danger"),
          uiOutput("internal_unlock_error")
        )
      )
    ),
    conditionalPanel(
      condition = "input.internal_unlocked == true",
      ...
    )
  )
}

# ======================================================
# PART 1 -- UI
# ======================================================

ui <- navbarPage(
  title = "",
  theme = NULL,
  header = tagList(
    tags$head(tags$title("Satya Carbon")),
    tags$div(
      style = "display:none;",
      checkboxInput("internal_unlocked", NULL, value = FALSE)
    )
  ),

  # ---- TAB 1: COVER ----
  # Landing page -- company name and a short statement of what the tool
  # actually does, before anyone touches real data. Deliberately brief:
  # this is a cover, not documentation (the earlier, more detailed
  # methodology writeup lives outside the app now, not duplicated here).
  tabPanel(
    "Home",
    fluidPage(
      tags$div(
        style = "max-width:780px; margin:8vh auto 0; text-align:center; padding:0 1.5rem;",
        tags$img(
          src = "https://assets.zyrosite.com/cdn-cgi/image/format=auto,w=600,fit=crop/s7PCQuvqRD79j1IE/1gmxdt-logomakr-300dpi-1-4I4wBKmUEdDGTcmV.png",
          style = "max-width:260px; margin:0 auto 1.8rem; display:block;",
          alt = "Satya Carbon logo"
        ),
        tags$p(style = "color:#7F8C8D; font-size:17px; margin-bottom:2.2rem; letter-spacing:0.3px;", "Emissions Intelligence & Carbon Credit Portfolio Platform"),
        tags$div(style = "width:220px; height:2px; background:linear-gradient(90deg, transparent, #27AE60, transparent); margin:0 auto 4rem;"),
        tags$p(
          style = "color:#AEB6BF; font-size:11.5px; letter-spacing:0.2px; line-height:1.6; margin-bottom:1rem;",
          "\u00A9 2026 Satya Carbon. All rights reserved.", tags$br(),
          "This platform and its contents are the proprietary and confidential intellectual property of Satya Carbon."
        )
      )
    )
  ),

  # ---- TOP-LEVEL: SECTOR VIEW (merged United States + Europe) ----
  # Per direction: Sector View is no longer duplicated as a sub-tab
  # inside both the United States and Europe dropdowns -- one main
  # tab, with United States and Europe as internal sub-tabs instead.
  # Identical content and output IDs to before (sector_plot,
  # eu_sector_plot, eu_sector_bucket_plot, etc.) -- only the
  # navigation changed, not any underlying logic.
  tabPanel(
    "Global Sector View",
    tabsetPanel(
      tabPanel(
        "United States",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "sector_select", "Sector",
          choices  = sector_list,
          selected = sector_list[1]
        ),
        checkboxInput("show_target_sector", "Show target pathway", value = TRUE),
        checkboxInput("show_forecast_sector", "Show model forecast", value = TRUE),
        hr(),
        helpText(
          "Solid line: observed GHGRP emissions (2011-", last_hist_year, ").",
          "Dashed line: model forecast (", last_hist_year + 1, "-", last_fore_year, ").",
          "Dotted line: implied sector decarbonization target pathway, ",
          "anchored at each facility's ", last_hist_year, " actual emissions."
        ),
        hr(),
        uiOutput("target_meta_sector")
      ),
      mainPanel(
        width = 9,
        uiOutput("sector_stat_cards"),
        plotOutput("sector_plot", height = "460px"),
        plotOutput("sector_gap_plot", height = "180px"),
        br(),
        DTOutput("sector_table")
      )
    )
      ),
      tabPanel(
        "Europe",
    fluidPage(
      uiOutput("eu_data_status"),
      conditionalPanel(
        condition = "output.eu_data_available",
        tabsetPanel(
          tabPanel(
            "Official EEA Sectors (historical)",
            br(),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput("eu_sector_select", "Sector (EEA official category)", choices = NULL),
                helpText(em(
                  "Read directly from the Air_Releases_Sector sheet -- the EEA's own ",
                  "official rollup, not derived from facility-level activity codes."
                )),
                hr(),
                uiOutput("eu_sector_meta")
              ),
              mainPanel(
                width = 9,
                uiOutput("eu_scope_note"),
                plotOutput("eu_sector_plot", height = "440px"),
                br(),
                DTOutput("eu_sector_table")
              )
            )
          ),
          tabPanel(
            "Model Forecast by Sector",
            br(),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput("eu_sector_bucket_select", "Sector (model's own grouping)", choices = NULL),
                helpText(em(
                  "The panel model's own sector grouping -- EPRTR Annex I activity codes, ",
                  "bucketed to the ones with enough facilities for a real trend estimate. ",
                  "NOT the same categories as the official EEA sectors on the other sub-tab."
                )),
                hr(),
                uiOutput("eu_sector_bucket_meta")
              ),
              mainPanel(
                width = 9,
                uiOutput("eu_forecast_note"),
                plotOutput("eu_sector_bucket_plot", height = "440px"),
                br(),
                DTOutput("eu_sector_bucket_table")
              )
            )
          )
        )
      )
    )
      )
    )
  ),
  # ---- USA: grouped under one navbar dropdown ----
  # Sector View, Company Profile, and Portfolio Mix were three
  # separate top-level tabs, all US-specific, sitting apart in the
  # navbar from each other with unrelated tabs (EU, LATAM, etc.) in
  # between. navbarMenu() groups them under one "USA" dropdown --
  # same three tabs, same content, unchanged, just organized so the
  # navbar itself communicates "these three belong together."
  navbarMenu(
    "United States",


  # ---- TAB 6: COMPANY PROFILE ----
  # Consolidates what used to be three separate tabs (Facility & Company
  # View, SBTi Calculator, New Company Intake) into one workflow. The
  # actual problem being fixed: SBTi Calculator and New Company Intake
  # used to ask for the same company name, historical emissions, and
  # target year TWICE, in two disconnected forms that could silently
  # drift out of sync (the purple SBTi line using stale default numbers
  # while every other line used real data was a direct symptom of this).
  # Now there is exactly ONE place to enter a new company's data --
  # SBTi-specific fields (method, SDA sector, net zero year, Scope 3
  # ambition) are still asked for, since NCI has no equivalent, but
  # company name / historical emissions / target year are entered once
  # and both the trend charts and the SBTi engine read the same values.
  # A second mode lets you browse an EXISTING real GHGRP facility/company
  # instead of entering a new one -- a genuinely different use case
  # (exploring real reported data vs. submitting a new company's own),
  # kept as a toggle in the same tab rather than two more separate ones.
  tabPanel(
    "Company Profile",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(
          "intake_company_mode", "Company",
          choices = c(
            "Select an existing company" = "existing",
            "Add a new company" = "new"
          ),
          selected = "new"
        ),
        conditionalPanel(
          condition = "input.intake_company_mode == 'existing'",
          selectizeInput(
            "intake_company_existing_picker", NULL,
            choices = NULL, selected = NULL,
            options = list(placeholder = "Select a company...")
          )
        ),
        conditionalPanel(
          condition = "input.intake_company_mode == 'new'",
          textInput("intake_company_new_name", NULL, placeholder = "Company / Facility Name")
        ),
        # Hidden, kept in sync with whichever of the two controls above is
        # currently active (by the observer below) -- every downstream
        # reactive (matching, auto-populate, SBTi sync, etc.) reads this
        # one value, exactly as before, regardless of which path was used.
        tags$div(style = "display:none;", textInput("intake_company_name", NULL, value = "")),
        selectInput(
          "intake_facility_country", "Company location (country, optional)",
          choices = c("Not specified" = "", global_country_list),
          selected = ""
        ),
        conditionalPanel(
          condition = "input.intake_facility_country == 'United States'",
          selectInput(
            "intake_facility_state", "State (optional)",
            choices = c("Not specified" = "", us_state_choices),
            selected = ""
          ),
          selectInput(
            "intake_facility_county", "County (optional)",
            choices = c("Select a state first" = ""),
            selected = ""
          ),
          helpText(em("County-level location gives the most precise proximity matching on the Portfolio Mix tab; state alone still works if you don't know the county."))
        ),
        conditionalPanel(
          condition = "input.intake_facility_country != 'United States' && input.intake_facility_country != ''",
          helpText(em("Country-level proximity matching is used for this location -- state/county precision is currently only available for the United States."))
        ),
        selectInput(
          "intake_sector", "Closest matching sector",
          choices = sector_list, selected = sector_list[1]
          ),
          uiOutput("intake_median_stat"),
          checkboxInput(
            "intake_show_forecast",
            "Show model forecast (sector trend)",
            value = TRUE
          ),
          hr(),
          conditionalPanel(
            condition = "input.intake_company_mode == 'new'",
            # Per explicit request: companies are now assumed to always
            # have Scope 1/2/3 data available, so the "do you have
            # data?" questions are gone -- these hidden inputs stay
            # fixed at "yes" so every downstream reactive that checks
            # input$intake_has_data / input$intake_has_data_s23 (data
            # parsing, target pathway, SBTi sync, the gap chart) keeps
            # working completely unchanged. The "no data" interim-
            # benchmark branch that used to live here is gone entirely,
            # not just hidden -- it's genuinely unreachable now.
            tags$div(
              style = "display:none;",
              radioButtons("intake_has_data", NULL, choices = c("yes" = "yes"), selected = "yes"),
              radioButtons("intake_has_data_s23", NULL, choices = c("yes" = "yes"), selected = "yes")
            ),
            tags$div(
              style = "background:#F4F6F7; border-radius:6px; padding:0.7rem 1rem; margin-bottom:12px;",
              tags$b("Upload from Excel"), tags$br(),
              downloadLink("intake_template_download", "Download the blank template", style = "font-size:12.5px;"),
              tags$br(),
              fileInput("intake_template_upload", NULL, accept = c(".xlsx"), buttonLabel = "Browse...", placeholder = ""),
              uiOutput("intake_template_upload_status")
            ),
            # Manual paste fields -- kept functional (the upload above
            # populates these exact inputs via updateTextAreaInput(), so
            # intake_user_data() and friends still read from them either
            # way) but no longer shown, since Excel upload is now the
            # primary path.
            tags$div(
              style = "display:none;",
              textAreaInput(
                "intake_emissions_csv", "Historical emissions (tCO2e) -- Scope 1",
                rows = 6, placeholder = "2021,125000\n2022,118000\n2023,110500"
              ),
              textAreaInput(
                "intake_emissions_csv_s2", "Historical emissions (tCO2e) -- Scope 2",
                rows = 4, placeholder = "2021,18000\n2022,17200\n2023,16100"
              ),
              textAreaInput(
                "intake_emissions_csv_s3", "Historical emissions (tCO2e) -- Scope 3",
                rows = 4, placeholder = "2021,135000\n2022,128000\n2023,119000"
              )
            )
          ),
          # Target-year settings -- shown once real data exists, whether
          # that's because a new company pasted it above, or because an
          # existing company was matched and its real data was auto-
          # populated (the auto-populate observer sets intake_has_data to
          # "yes" on match). Same shared inputs either way -- the CSV
          # paste box above is the only thing that's genuinely redundant
          # for a matched company; the target year is not.
          conditionalPanel(
            condition = "input.intake_has_data == 'yes'",
            checkboxInput(
              "intake_use_sector_target",
              "Use this sector's published target instead of setting my own",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.intake_use_sector_target == false",
              sliderInput("intake_target_year", "Target year", value = 2030, min = 2026, max = 2050, step = 1, sep = ""),
              sliderInput("intake_target_reduction", "Target reduction from baseline (%)",
                          value = 30, min = 1, max = 100, step = 1, post = "%")
            )
          ),
          hr(),
          uiOutput("intake_meta"),
          hr(),

          # ---- SBTi-specific controls -- everything else SBTi needs
          # (company name, historical emissions, target year) is already
          # entered above; this is only what has no NCI equivalent. ----
          tags$b("SBTi target-setting method"),
          tags$div(
            style = "display:none;",
            textInput("sbti_calc_company", NULL, value = ""),
            numericInput("sbti_calc_base_year", NULL, value = 2015, min = 1990, max = 2040),
            numericInput("sbti_calc_s1", NULL, value = 100000, min = 0),
            numericInput("sbti_calc_s2", NULL, value = 150000, min = 0),
            numericInput("sbti_calc_target_year", NULL, value = 2030, min = 2025, max = 2050),
            numericInput("sbti_calc_mry_year", NULL, value = 2015, min = 1990, max = 2040),
            numericInput("sbti_calc_mry_s1", NULL, value = 100000, min = 0),
            numericInput("sbti_calc_mry_s2", NULL, value = 150000, min = 0),
            numericInput("sbti_calc_base_year_s3", NULL, value = 2015, min = 1990, max = 2040),
            numericInput("sbti_calc_target_year_s3", NULL, value = 2030, min = 2025, max = 2050),
            numericInput("sbti_calc_mry_year_s3", NULL, value = 2015, min = 1990, max = 2040),
            numericInput("sbti_calc_s3", NULL, value = 0, min = 0),
            numericInput("sbti_calc_mry_s3", NULL, value = 0, min = 0)
          ),
          radioButtons(
            "sbti_calc_method", "Scope 1 & 2 Method",
            choices = c(
              "Sectoral Decarbonization Approach (SDA)" = "Sectoral Decarbonization Approach",
              "Absolute Contraction Approach (ACA)"      = "Absolute Contraction Approach"
            ),
            selected = "Absolute Contraction Approach"
          ),
          helpText(em("Auto-routed from the sector above -- override if needed.")),
          conditionalPanel(
            condition = "input.sbti_calc_method == 'Sectoral Decarbonization Approach'",
            selectInput("sbti_calc_sda_sector", "SDA Sector", choices = c("Power", "Cement")),
            numericInput("sbti_calc_activity", "Base Year Activity Output", value = 5000000, min = 0),
            helpText("MWh generated (Power) or tonnes of cement produced (Cement).")
          ),
          numericInput("sbti_calc_net_zero_year", "Net Zero Year", value = 2050, min = 2030, max = 2060),
          selectInput(
            "sbti_calc_s3_method", "Scope 3 Method",
            choices = c("Cross-sector ACA" = "Cross-sector ACA",
                        "Economic intensity" = "Economic intensity",
                        "Physical intensity" = "Physical intensity")
          ),
          conditionalPanel(
            condition = "input.sbti_calc_s3_method == 'Cross-sector ACA'",
            radioButtons(
              "sbti_calc_s3_ambition", "Ambition Level",
              choices = c("1.5C (90% net-zero ambition, 4.2% floor)" = "1.5C",
                          "Well-Below 2C (75% net-zero ambition, 2.5% floor)" = "WB2C"),
              selected = "1.5C"
            )
          ),
          conditionalPanel(
            condition = "input.sbti_calc_s3_method != 'Cross-sector ACA'",
            numericInput("sbti_calc_s3_output", "Base Year Output (revenue/value-added or physical units)", value = 1000000, min = 0),
            helpText("Scope 3 intensity = Base Year Scope 3 Emissions / this value.")
          )
      ),
      mainPanel(
        width = 8,
          tabsetPanel(
            tabPanel(
              "Scope 1",
              br(),
              plotOutput("intake_plot", height = "460px"),
              plotOutput("intake_level_bar_s1_plot", height = "340px"),
              plotOutput("intake_gap_s1_plot", height = "220px")
            ),
            tabPanel(
              "Scope 2",
              br(),
              uiOutput("intake_scope2_header"),
              plotOutput("intake_plot_s2", height = "400px"),
              plotOutput("intake_level_bar_s2_plot", height = "340px"),
              plotOutput("intake_gap_s2_plot", height = "220px")
            ),
            tabPanel(
              "Scope 3",
              br(),
              uiOutput("intake_scope3_header"),
              tabsetPanel(
                tabPanel(
                  "Trend",
                  br(),
                  plotOutput("intake_plot_s3", height = "400px"),
                  plotOutput("intake_level_bar_s3_plot", height = "340px"),
                  plotOutput("intake_gap_s3_plot", height = "220px")
                ),
                tabPanel(
                  "Category Breakdown",
                  br(),
                  h5("Scope 3 breakdown by GHG Protocol category"),
                  h6("Forecasted, all years"),
                  plotOutput("intake_s3_category_trend_plot", height = "380px"),
                  DTOutput("intake_s3_category_trend_table"),
                  hr(),
                  h6("Ratios currently in use"),
                  DTOutput("intake_s3_ratio_table"),
                  plotOutput("intake_s3_category_plot", height = "420px"),
                  DTOutput("intake_s3_category_table")
                )
              )
            ),
            tabPanel(
              "SBTi Detail",
              br(),
              uiOutput("sbti_derived_readout"),
              br(),
              uiOutput("sbti_calc_error"),
              plotOutput("sbti_calc_plot", height = "440px"),
              br(),
              uiOutput("sbti_calc_scope3_note"),
              DTOutput("sbti_calc_table")
            ),
            tabPanel(
              "Facilities",
              br(),
              uiOutput("intake_facilities_content")
            )
          ),
          br(),
          DTOutput("intake_table")
      )
    )
  ),

  # ---- TAB 8: PORTFOLIO MIX ----
  # Combines what were two separate tabs (Portfolio Mix Engine +
  # Portfolio Curation Engine) into one -- per the Jul 31 sync, the
  # near-identical names were confusing on their own, split across two
  # tabs. Bucket-level and methodology-level views are now subheadings
  # (subtabs) within this one tab instead of two separate top-level
  # navbar entries. Sidebar controls from both are merged into one
  # panel; explanatory helpText trimmed down significantly per Rajat/
  # Avishkar's note that it was only there for presentation, not because
  # the interface needed it to be usable.
  tabPanel(
    "Portfolio Mix",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        uiOutput("pme_context"),
        hr(),
        selectInput("pme_year", "Year to size portfolio for", choices = NULL),
        selectInput(
          "pme_gap_source", "Size portfolio against",
          choices = c("Your Stated Goal" = "own", "Industry Target" = "industry", "SBTi-Recommended" = "sbti"),
          selected = "own"
        ),
        numericInput("pme_budget", "Annual credit budget ($)", value = 50000, min = 0, step = 1000),
        tags$b("Relative preference weight"),
        numericInput("pme_wt_nat_avoid", "Nature-based avoidance", value = 25, min = 0, max = 100, step = 5),
        numericInput("pme_wt_nat_removal", "Nature-based removal", value = 20, min = 0, max = 100, step = 5),
        numericInput("pme_wt_tech_avoid", "Technology-based avoidance", value = 15, min = 0, max = 100, step = 5),
        numericInput("pme_wt_tech_removal", "Technology-based removal", value = 20, min = 0, max = 100, step = 5),
        numericInput("pme_wt_comm_avoid", "Community-based avoidance", value = 20, min = 0, max = 100, step = 5),
        hr(),
        tags$b("Methodology-level detail (below)"),
        sliderInput(
          "pce_preference_strength", "Respect stated mix vs. minimize gap",
          min = 0, max = 100, value = 30, step = 5, post = "%"
        ),
        sliderInput(
          "pce_bucket_cap", "Max share from any one bucket",
          min = 20, max = 100, value = 45, step = 5, post = "%"
        ),
        selectInput(
          "pce_claim_tier", "Claim tier (minimum coverage floor)",
          choices = c("None" = "none", "Silver (10%)" = "silver", "Gold (50%)" = "gold", "Platinum (100%)" = "platinum"),
          selected = "none"
        ),
        hr(),
        uiOutput("pme_facility_readout"),
        sliderInput(
          "pme_proximity_weight", "Prioritize projects near the facility",
          min = 0, max = 100, value = 0, step = 5, post = "%"
        ),
        hr(),
        sliderInput(
          "pme_forward_discount", "Forward pricing discount (5-Year tab only)",
          min = 0, max = 60, value = 30, step = 5, post = "%"
        )
      ),
      mainPanel(
        width = 10,
        tabsetPanel(
          tabPanel(
            "Short Term: 1-Year Optimal Portfolio",
            tags$div(style = "height:10px;"),
            uiOutput("pme_summary"),
            fluidRow(
              column(width = 6, plotOutput("pme_tons_plot", height = "270px")),
              column(width = 6, plotOutput("pme_spend_plot", height = "270px"))
            ),
            hr(),
            h5("Where your facility is, and where these projects are"),
            fluidRow(
              column(
                width = 6,
                tags$b("Your facility (US state/county precision)"),
                plotOutput("pme_map_us", height = "420px", click = "pme_map_click")
              ),
              globe_panel_ui("pme_map_world_wrapper")
            ),
            uiOutput("pme_map_detail"),
            hr(),
            h5("Full breakdown, by category and project"),
            DTOutput("pme_table")
          ),
          tabPanel(
            "Long Term: 5-Year Optimal Portfolio",
            tags$div(style = "height:10px;"),
            uiOutput("pme_forward_discount_readout"),
            uiOutput("pme_5yr_full_summary"),
            fluidRow(
              column(width = 6, plotOutput("pme_5yr_tons_plot", height = "270px")),
              column(width = 6, plotOutput("pme_5yr_spend_plot", height = "270px"))
            ),
            hr(),
            h5("Where your facility is, and where these projects are"),
            fluidRow(
              column(
                width = 6,
                tags$b("Your facility (US state/county precision)"),
                plotOutput("pme_5yr_map_us", height = "420px", click = "pme_5yr_map_click")
              ),
              globe_panel_ui("pme_5yr_map_world_wrapper")
            ),
            uiOutput("pme_5yr_map_detail"),
            hr(),
            h5("Full breakdown, by category and project"),
            DTOutput("pme_5yr_category_table")
          ),
          tabPanel(
            "Methodologies",
            tags$div(style = "height:10px;"),
            DTOutput("pce_table"),
            uiOutput("pce_adjusted_summary"),
            hr(),
            h6("By methodology (projects summed up to their shared methodology)"),
            DTOutput("pce_methodology_table"),
            hr(),
            h5("Add a new methodology"),
            fluidRow(
              column(width = 4, textInput("pce_new_project_type", "Project Type", placeholder = "e.g. Mangrove Restoration")),
              column(width = 4, textInput("pce_new_methodology_code", "Methodology Code", placeholder = "e.g. VM0033")),
              column(width = 4, selectInput("pce_new_mechanism", "Mechanism", choices = c("Nature-based", "Technology-based", "Community-based")))
            ),
            fluidRow(
              column(width = 4, selectInput("pce_new_action", "Action", choices = c("Avoidance", "Removal"))),
              column(width = 4, numericInput("pce_new_price", "Price ($/t)", value = NA, min = 0)),
              column(width = 4, numericInput("pce_new_dev_cost", "Dev Cost ($/t, optional)", value = NA, min = 0))
            ),
            fluidRow(
              column(width = 4, numericInput("pce_new_supply", "Supply (t)", value = NA, min = 0)),
              column(width = 4, selectInput("pce_new_state", "US State", choices = c("Choose..." = "", us_state_choices))),
              column(width = 4, textInput("pce_new_developer", "Developer (optional)", placeholder = "e.g. DevCo K"))
            ),
            actionButton("pce_add_methodology_btn", "Add methodology", class = "btn-primary"),
            uiOutput("pce_add_methodology_status")
          ),
          tabPanel(
            "Coverage Audit",
            internal_gate(
              tags$div(style = "height:10px;"),
              h5("What the current inventory can and can't cover"),
              uiOutput("pce_coverage_summary"),
              uiOutput("pce_coverage_note"),
              br(),
              h6("How much of each scope's gap the CURRENT recommended portfolio covers"),
              fluidRow(
                column(width = 4, plotOutput("pce_coverage_pie_s1", height = "260px")),
                column(width = 4, plotOutput("pce_coverage_pie_s2", height = "260px")),
                column(width = 4, plotOutput("pce_coverage_pie_s3", height = "260px"))
              ),
              br(),
              h6("% covered, by Scope 3 category"),
              plotOutput("pce_coverage_category_plot", height = "560px"),
              br(),
              h6("By Scope 3 category"),
              DTOutput("pce_coverage_table")
            )
          ),
          tabPanel(
            "Internal: Demand Trends",
            internal_gate(
              tags$div(style = "height:10px;"),
              plotOutput("pme_demand_plot", height = "320px"),
              br(),
              DTOutput("pme_demand_table")
            )
          )
        )
      )
    )
  )
  ),


  # ---- EUROPE: grouped under one navbar dropdown ----
  # Same reasoning and mechanism as the USA dropdown -- these four
  # tabs were already adjacent in the file; renamed here to drop the
  # redundant "EU " prefix now that the dropdown label itself says
  # "Europe". Internal output IDs (eu_sector_plot, eu_cp_*, etc.)
  # are untouched -- only the user-facing tab labels changed.
  navbarMenu(
    "Europe",

  # ---- TAB: EU FACILITY VIEW ----
  # Same real-data-only honesty as EU Sector View, at facility level --
  # plus the real panel-model forecast line, same intuition as the US
  # Facility View's solid-observed / dashed-forecast convention.
  tabPanel(
    "Facility View",
    fluidPage(
      uiOutput("eu_data_status_fac"),
      conditionalPanel(
        condition = "output.eu_data_available",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectizeInput(
              "eu_facility_select", "Facility",
              choices = NULL, selected = NULL,
              options = list(placeholder = "Start typing a facility name...")
            ),
            uiOutput("eu_facility_meta")
          ),
          mainPanel(
            width = 9,
            uiOutput("eu_scope_note_fac"),
            plotOutput("eu_facility_plot", height = "460px"),
            br(),
            h4("Highest emitters by country"),
            plotOutput("eu_country_plot", height = "400px")
          )
        )
      )
    )
  ),

  # ---- TAB: EU COMPANY PROFILE ----
  # Same real-company-matching pattern as the US Company Profile:
  # typing an existing company auto-populates its real EU data instead
  # of requiring manual re-entry, one shared field for existing-or-new,
  # facilities nested beneath their parent company. Deliberately
  # narrower than the US version in exactly the places EU data doesn't
  # support: Scope 1 only (no Hertwich & Wood-style Scope 2/3 ratio
  # system built for EU sectors), and no SBTi/sector-target calculator
  # (no real EU sector-targets workbook exists -- see the pipeline
  # script's own header). A company CAN still set its own aspirational
  # target here -- that's the company's own choice, not a sourced
  # regulatory number, so it's not the same kind of fabrication.
  tabPanel(
    "Company Profile",
    fluidPage(
      uiOutput("eu_cp_data_status"),
      conditionalPanel(
        condition = "output.eu_data_available",
        sidebarLayout(
          sidebarPanel(
            width = 4,
            radioButtons(
              "eu_cp_mode", "Company",
              choices = c("Select an existing company" = "existing", "Add a new company" = "new"),
              selected = "new"
            ),
            conditionalPanel(
              condition = "input.eu_cp_mode == 'existing'",
              selectizeInput(
                "eu_cp_existing_picker", NULL,
                choices = NULL, selected = NULL,
                options = list(placeholder = "Select a company...")
              )
            ),
            conditionalPanel(
              condition = "input.eu_cp_mode == 'new'",
              textInput("eu_cp_new_name", NULL, placeholder = "Company / Facility Name")
            ),
            tags$div(style = "display:none;", textInput("eu_cp_company_name", NULL, value = "")),
            uiOutput("eu_cp_match_status"),
            conditionalPanel(
              condition = "input.eu_cp_mode == 'new'",
              selectInput("eu_cp_country", "Company location (country)",
                          choices = c("Not specified" = "", country_list_eu), selected = ""),
              selectInput("eu_cp_sector", "Closest matching sector (model grouping)",
                          choices = sector_bucket_list_eu, selected = sector_bucket_list_eu[1]),
              hr(),
              radioButtons(
                "eu_cp_has_data", "Do you have historical emissions data?",
                choices = c("Yes, I have emissions data" = "yes", "No, not yet" = "no"),
                selected = "no"
              ),
              conditionalPanel(
                condition = "input.eu_cp_has_data == 'yes'",
                helpText(
                  "Paste one 'year,emissions_tCO2e' pair per line, e.g.:", tags$br(),
                  "2021,125000", tags$br(), "2022,118000", tags$br(), "2023,110500"
                ),
                textAreaInput(
                  "eu_cp_emissions_csv", "Historical emissions (tCO2e) -- Scope 1 (approx.)",
                  rows = 6, placeholder = "2021,125000\n2022,118000\n2023,110500"
                )
              )
            ),
            hr(),
            checkboxInput("eu_cp_set_target", "Set my own target (optional)", value = FALSE),
            conditionalPanel(
              condition = "input.eu_cp_set_target == true",
              sliderInput("eu_cp_target_year", "Target year", value = 2030, min = 2026, max = 2050, step = 1, sep = ""),
              sliderInput("eu_cp_target_reduction", "Target reduction from baseline (%)",
                          value = 30, min = 1, max = 100, step = 1, post = "%"),
              helpText(em("This is the company's own aspirational target -- not a sourced sector or regulatory number."))
            )
          ),
          mainPanel(
            width = 8,
            tabsetPanel(
              tabPanel(
                "Scope 1",
                br(),
                uiOutput("eu_cp_scope_note"),
                plotOutput("eu_cp_trend_plot", height = "460px"),
                br(),
                DTOutput("eu_cp_table")
              ),
              tabPanel(
                "Facilities",
                br(),
                uiOutput("eu_cp_facilities_content")
              )
            )
          )
        )
      )
    )
  ),

  # ---- TAB: EU PORTFOLIO MIX ----
  # Reuses the EXACT same LP solver (solve_portfolio_lp) and the SAME
  # global project catalog (catalog_rv() -- already covers all 19
  # supported countries, European ones included) as the US Portfolio
  # Mix -- neither of those was ever US-specific. What's genuinely new
  # here: gap sizing comes from the active EU Company Profile company
  # instead of the US one, and per explicit request, the map pairing
  # is EU country + world (not US state/county + world) -- both maps
  # shown together, same pattern as the US tab.
  # Deliberately simpler than the US version: one tab, not five --
  # bucket-preference weighting is uniform (no per-bucket slider panel
  # duplicated here) and there's no Scope 3 category-capping, since
  # neither concept exists for EU data yet. Real LP optimization, real
  # proximity scoring, real maps -- just a narrower sidebar.
  tabPanel(
    "Portfolio Mix",
    fluidPage(
      uiOutput("eu_pm_data_status"),
      conditionalPanel(
        condition = "output.eu_data_available",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput("eu_pm_company_readout"),
            hr(),
            numericInput("eu_pm_gap_tons", "Emissions gap to cover (tCO2e)", value = 100000, min = 0),
            helpText(em("Defaults to the active EU company's forecast for the selected year, if one is set on EU Company Profile -- override freely.")),
            selectInput("eu_pm_gap_year", "Gap year", choices = 2025:2029, selected = 2025),
            numericInput("eu_pm_budget", "Annual budget ($)", value = 500000, min = 0),
            hr(),
            sliderInput("eu_pm_proximity_weight", "Proximity weight (%)", value = 20, min = 0, max = 60, step = 5),
            sliderInput("eu_pm_bucket_cap", "Max share per methodology bucket (%)", value = 30, min = 10, max = 100, step = 5),
            selectInput("eu_pm_claim_tier", "Minimum coverage floor",
                        choices = c("None" = "none", "Silver (10%)" = "silver", "Gold (50%)" = "gold", "Platinum (100%)" = "platinum"),
                        selected = "none")
          ),
          mainPanel(
            width = 9,
            uiOutput("eu_pm_context"),
            fluidRow(
              column(width = 6, plotOutput("eu_pm_tons_plot", height = "270px")),
              column(width = 6, plotOutput("eu_pm_spend_plot", height = "270px"))
            ),
            hr(),
            h5("Where the active company is, and where these projects are"),
            fluidRow(
              column(
                width = 6,
                tags$b("Europe (zoomed)"),
                plotOutput("eu_pm_map_eu", height = "420px")
              ),
              globe_panel_ui("eu_pm_map_world_wrapper")
            ),
            hr(),
            h5("Full breakdown, by project"),
            DTOutput("eu_pm_table")
          )
        )
      )
    )
  )
  ),


  # ---- TAB: PORTFOLIO MIX LATAM ----
  # Same pattern as EU Portfolio Mix: reuses solve_portfolio_lp(),
  # compute_proximity_score(), catalog_rv(), and the region-zoom map --
  # none of those are region-specific. Gap sizing comes from Brazil or
  # Chile's SYNTHETIC emissions trend (no real facility-level source
  # exists for either yet -- see synthetic_row_countries' own comment).
  # The data-source badge stays visible throughout so this is never
  # confused with the real US/EU data sitting in the adjacent tabs.
  tabPanel(
    "Latin America",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("latam_pm_country", "Country", choices = c("Brazil", "Chile"), selected = "Brazil"),
          uiOutput("latam_pm_data_badge"),
          hr(),
          numericInput("latam_pm_gap_mt", "Emissions gap to cover (Mt CO2e)", value = 10, min = 0),
          helpText(em("Auto-suggested as 10% of the selected country's latest-year synthetic total -- override freely.")),
          numericInput("latam_pm_budget", "Annual budget ($)", value = 1000000, min = 0),
          hr(),
          sliderInput("latam_pm_proximity_weight", "Proximity weight (%)", value = 20, min = 0, max = 60, step = 5),
          sliderInput("latam_pm_bucket_cap", "Max share per methodology bucket (%)", value = 30, min = 10, max = 100, step = 5)
        ),
        mainPanel(
          width = 9,
          uiOutput("latam_pm_context"),
          fluidRow(
            column(width = 6, plotOutput("latam_pm_tons_plot", height = "270px")),
            column(width = 6, plotOutput("latam_pm_spend_plot", height = "270px"))
          ),
          hr(),
          plotOutput("latam_pm_trend_plot", height = "260px"),
          hr(),
          h5("Where the selected country is, and where these projects are"),
          fluidRow(
            column(width = 6, tags$b("Latin America (zoomed)"), plotOutput("latam_pm_map_zoom", height = "420px")),
            globe_panel_ui("latam_pm_map_world_wrapper")
          ),
          hr(),
          h5("Full breakdown, by project"),
          DTOutput("latam_pm_table")
        )
      )
    )
  ),

  # ---- TAB: PORTFOLIO MIX ASIA ----
  # Same pattern -- India or Japan, both synthetic.
  tabPanel(
    "Asia",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("asia_pm_country", "Country", choices = c("India", "Japan", "Singapore"), selected = "India"),
          uiOutput("asia_pm_data_badge"),
          hr(),
          numericInput("asia_pm_gap_mt", "Emissions gap to cover (Mt CO2e)", value = 10, min = 0),
          helpText(em("Auto-suggested as 10% of the selected country's latest-year synthetic total -- override freely.")),
          numericInput("asia_pm_budget", "Annual budget ($)", value = 1000000, min = 0),
          hr(),
          sliderInput("asia_pm_proximity_weight", "Proximity weight (%)", value = 20, min = 0, max = 60, step = 5),
          sliderInput("asia_pm_bucket_cap", "Max share per methodology bucket (%)", value = 30, min = 10, max = 100, step = 5)
        ),
        mainPanel(
          width = 9,
          uiOutput("asia_pm_context"),
          fluidRow(
            column(width = 6, plotOutput("asia_pm_tons_plot", height = "270px")),
            column(width = 6, plotOutput("asia_pm_spend_plot", height = "270px"))
          ),
          hr(),
          plotOutput("asia_pm_trend_plot", height = "260px"),
          hr(),
          h5("Where the selected country is, and where these projects are"),
          fluidRow(
            column(width = 6, tags$b("Asia (zoomed)"), plotOutput("asia_pm_map_zoom", height = "420px")),
            globe_panel_ui("asia_pm_map_world_wrapper")
          ),
          hr(),
          h5("Full breakdown, by project"),
          DTOutput("asia_pm_table")
        )
      )
    )
  ),

  # ---- TAB: PORTFOLIO MIX AUSTRALIA ----
  # Same pattern -- one country, no selector needed, synthetic data.
  tabPanel(
    "Australia",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("au_pm_data_badge"),
          numericInput("au_pm_gap_mt", "Emissions gap to cover (Mt CO2e)", value = 10, min = 0),
          helpText(em("Auto-suggested as 10% of Australia's latest-year synthetic total -- override freely.")),
          numericInput("au_pm_budget", "Annual budget ($)", value = 1000000, min = 0),
          hr(),
          sliderInput("au_pm_proximity_weight", "Proximity weight (%)", value = 20, min = 0, max = 60, step = 5),
          sliderInput("au_pm_bucket_cap", "Max share per methodology bucket (%)", value = 30, min = 10, max = 100, step = 5)
        ),
        mainPanel(
          width = 9,
          uiOutput("au_pm_context"),
          fluidRow(
            column(width = 6, plotOutput("au_pm_tons_plot", height = "270px")),
            column(width = 6, plotOutput("au_pm_spend_plot", height = "270px"))
          ),
          hr(),
          plotOutput("au_pm_trend_plot", height = "260px"),
          hr(),
          h5("Where Australia is, and where these projects are"),
          fluidRow(
            column(width = 6, tags$b("Australia (zoomed)"), plotOutput("au_pm_map_zoom", height = "420px")),
            globe_panel_ui("au_pm_map_world_wrapper")
          ),
          hr(),
          h5("Full breakdown, by project"),
          DTOutput("au_pm_table")
        )
      )
    )
  ),

  # ---- TAB: PORTFOLIO MIX AFRICA ----
  # Same pattern -- South Africa or Morocco, both synthetic.
  tabPanel(
    "Africa",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("africa_pm_country", "Country", choices = c("South Africa", "Morocco"), selected = "South Africa"),
          uiOutput("africa_pm_data_badge"),
          hr(),
          numericInput("africa_pm_gap_mt", "Emissions gap to cover (Mt CO2e)", value = 10, min = 0),
          helpText(em("Auto-suggested as 10% of the selected country's latest-year synthetic total -- override freely.")),
          numericInput("africa_pm_budget", "Annual budget ($)", value = 1000000, min = 0),
          hr(),
          sliderInput("africa_pm_proximity_weight", "Proximity weight (%)", value = 20, min = 0, max = 60, step = 5),
          sliderInput("africa_pm_bucket_cap", "Max share per methodology bucket (%)", value = 30, min = 10, max = 100, step = 5)
        ),
        mainPanel(
          width = 9,
          uiOutput("africa_pm_context"),
          fluidRow(
            column(width = 6, plotOutput("africa_pm_tons_plot", height = "270px")),
            column(width = 6, plotOutput("africa_pm_spend_plot", height = "270px"))
          ),
          hr(),
          plotOutput("africa_pm_trend_plot", height = "260px"),
          hr(),
          h5("Where the selected country is, and where these projects are"),
          fluidRow(
            column(width = 6, tags$b("Africa (zoomed)"), plotOutput("africa_pm_map_zoom", height = "420px")),
            globe_panel_ui("africa_pm_map_world_wrapper")
          ),
          hr(),
          h5("Full breakdown, by project"),
          DTOutput("africa_pm_table")
        )
      )
    )
  ),
  # ---- INTERNAL USE: grouped under one navbar dropdown ----
  # Backtesting and Pricing are both already gated behind the
  # internal-access code (internal_gate()) -- grouping them under one
  # "Internal Use" dropdown makes that restriction visible in the
  # navigation itself, not just inside each tab. Same content, same
  # access code, same everything -- only the navbar organization
  # changed.
  navbarMenu(
    "Internal Use",
  # ---- TAB 2: BACKTESTING (the gate -- everything else only shows facilities that pass this) ----
  # Validates the MODELING APPROACH, not the live forecast directly: a
  # separate model is fit on 2011-2020 only, then tested against 2021-2023
  # -- years it never saw. Every other tab in this app (Sector View,
  # Company Profile, everything) only ever shows facilities that passed
  # this quality gate -- the filtering happens once, in the data
  # pipeline, and cascades to everything built from ghgp_panel_filtered /
  # future_pred downstream.
  # The controls in this tab's sidebar are DIAGNOSTIC ONLY -- they let you
  # explore what a different threshold would look like, but do not change
  # what's actually shown elsewhere in the app. To change the real gate,
  # edit quality_gate_threshold / quality_gate_min_facilities in the
  # pipeline script and rerun it.
  tabPanel(
    "Backtesting",
    fluidPage(
      internal_gate(
        helpText(
          "A separate model is fit on 2011-2020 only, then tested against 2021-2023 -- ",
          "years it never saw. Every other tab in this app ONLY shows facilities that passed ",
          "the quality gate below -- that filtering already happened in the data pipeline, ",
          "before this app even loaded."
        ),
        hr(),
        h4("MdAPE by sector -- gated facilities only, 2023"),
        plotOutput("backtest_lastyear_plot", height = "380px"),
        hr(),
        h4("MdAPE by sector, across the full test window (2021-2023)"),
        plotOutput("backtest_multiyear_plot", height = "420px")
      )
    )
  ),
  # ---- TAB 9: PRICING (methodology price history) ----
  # SYNTHETIC price history for now -- a random walk per methodology,
  # constructed to end exactly at the price currently used in the
  # Portfolio Curation Engine's catalog, so the two are never inconsistent.
  # When a real price time series exists, this whole data source gets
  # replaced; the tab itself doesn't need to change.
  tabPanel(
    "Pricing",
    internal_gate(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          helpText(
            em("SYNTHETIC price history for now -- a random walk per methodology, built to end ",
               "exactly at the price currently used in the Portfolio Curation Engine's catalog. ",
               "The latest point in this history IS that price, not a separately-drawn number.")
          ),
          hr(),
          checkboxGroupInput(
            "pricing_bucket_filter", "Show buckets",
            choices = c(
              "Nature-based avoidance" = "nat_avoid", "Nature-based removal" = "nat_removal",
              "Technology-based avoidance" = "tech_avoid", "Technology-based removal" = "tech_removal",
              "Community-based avoidance" = "comm_avoid"
            ),
            selected = c("nat_avoid", "nat_removal", "tech_avoid", "tech_removal", "comm_avoid")
          ),
          hr(),
          uiOutput("pricing_methodology_selector"),
          helpText("Selecting none shows all methodologies in the checked buckets above.")
        ),
        mainPanel(
          width = 9,
          h4("Price over time, by methodology"),
          plotOutput("pricing_history_plot", height = "460px"),
          br(),
          h4("Current price snapshot"),
          p(em("Latest point in the history above -- this is the same number the Portfolio Curation Engine uses.")),
          DTOutput("pricing_snapshot_table")
        )
      )
    )
  )
  )
)

# ======================================================
# PART 2 -- SERVER
# ======================================================

server <- function(input, output, session) {

  # ---- Internal-use gate: checks the entered code against
  # internal_access_code and flips the shared hidden checkbox that every
  # internal_gate()-wrapped section's conditionalPanel watches. One
  # unlock applies to all four restricted areas at once.
  internal_unlock_msg <- reactiveVal(NULL)

  observeEvent(input$internal_unlock_btn, {
    entered <- if (is.null(input$internal_code_entry)) "" else input$internal_code_entry
    if (identical(entered, internal_access_code)) {
      updateCheckboxInput(session, "internal_unlocked", value = TRUE)
      internal_unlock_msg(NULL)
    } else {
      internal_unlock_msg("Incorrect code.")
    }
  })

  output$internal_unlock_error <- renderUI({
    msg <- internal_unlock_msg()
    req(!is.null(msg))
    tags$p(style = "color:#C0392B; font-size:12.5px; margin-top:6px;", msg)
  })

  # Populate facility selectize lazily (large list) --------------------
  updateSelectizeInput(
    session, "facility_select",
    choices  = facility_choices,
    selected = facility_choices[1],
    server   = TRUE
  )

  # Populate the county dropdown from REAL county names (us_county_lookup,
  # sourced from maps::county.fips) whenever the state changes -- cascading
  # select, since Shiny doesn't support a static two-level dependent
  # dropdown without a server-side update like this.
  observeEvent(input$intake_facility_state, {
    state <- input$intake_facility_state
    if (!nzchar(state)) {
      updateSelectInput(session, "intake_facility_county", choices = c("Select a state first" = ""), selected = "")
      return()
    }
    counties <- sort(us_county_lookup$county[us_county_lookup$state == state])
    choices <- c("Not specified" = "", counties)
    updateSelectInput(session, "intake_facility_county", choices = choices, selected = "")
  }, ignoreInit = TRUE)

  # Populate company selectize lazily (potentially very large list, since
  # the first-word grouping produces many single-facility "companies") ---
  updateSelectizeInput(
    session, "company_select",
    choices  = company_choices,
    selected = company_choices[1],
    server   = TRUE
  )

  # Company Profile's existing-company picker -- alphabetically sorted
  # (per explicit request), real companies only (no create=TRUE here --
  # a genuinely new company uses the separate "Add a new company" path
  # instead, so the two are never ambiguous).
  updateSelectizeInput(
    session, "intake_company_existing_picker",
    choices  = company_choices_alpha,
    selected = character(0),
    server   = TRUE
  )

  # Keeps the shared hidden intake_company_name in sync with whichever
  # of the two visible controls is currently active -- every downstream
  # reactive keeps reading input$intake_company_name exactly as before,
  # regardless of which path (existing vs. new) the value came from.
  observe({
    val <- if (identical(input$intake_company_mode, "existing")) {
      if (is.null(input$intake_company_existing_picker)) "" else input$intake_company_existing_picker
    } else {
      if (is.null(input$intake_company_new_name)) "" else input$intake_company_new_name
    }
    updateTextInput(session, "intake_company_name", value = val)
  })

  # ---------------- OVERVIEW: target-lookup summary table ----------------
  # Direct render of target_lookup, exactly as loaded from
  # shiny_data/target_lookup.rds -- no transformation of the underlying
  # numbers, so this always matches whatever the pipeline script actually
  # produced. source_organization/source_document/official_url are new
  # columns added by the pipeline's citation join (Part 4B) -- if you're
  # running this against an older shiny_data/ export that predates that
  # join, those three columns will be NA and the Source column will just
  # show blank/plain text instead of a link.
  output$target_lookup_table <- renderDT({
    has_citation_cols <- all(c("source_organization", "source_document", "official_url") %in% names(target_lookup))

    df <- target_lookup %>% arrange(primary_sector)

    if (has_citation_cols) {
      df <- df %>%
        mutate(
          source = case_when(
            !is.na(official_url) & nzchar(official_url) ~ paste0(
              '<a href="', official_url, '" target="_blank" rel="noopener noreferrer">',
              ifelse(!is.na(source_organization) & nzchar(source_organization),
                     source_organization, "Source"),
              '</a>'
            ),
            !is.na(source_organization) ~ source_organization,
            TRUE ~ ""
          )
        ) %>%
        select(primary_sector, baseline_year, target_year, reduction_fraction,
               annual_rate, confidence, source, source_document, caveat)

      dt <- datatable(
        df,
        rownames = FALSE,
        escape = -7,   # column 7 = "source" -- the only column allowed to render raw HTML (the <a> link)
        options = list(pageLength = 20, dom = "t", scrollX = TRUE),
        colnames = c("Sector", "Baseline Year", "Target Year", "Reduction Fraction",
                     "Implied Annual Rate", "Confidence", "Source", "Source Document", "Caveat")
      )
    } else {
      dt <- datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 20, dom = "t", scrollX = TRUE),
        colnames = c("Sector", "Baseline Year", "Target Year", "Reduction Fraction",
                     "Implied Annual Rate", "Confidence", "Caveat")
      )
    }

    dt %>% formatPercentage(c("reduction_fraction", "annual_rate"), 1)
  })

  # Compact reference table -- condensed version of the "Sector Targets"
  # detail sheet: sub-sector, source, document, metric, scope, and a link.
  # Falls back to an explanatory placeholder if the pipeline export
  # predates the sub_sector/metric/scope_coverage columns.
  output$target_sources_table <- renderDT({
    has_ref_cols <- all(c("sub_sector", "metric", "scope_coverage", "official_url") %in% names(target_lookup))

    if (!has_ref_cols) {
      return(datatable(
        data.frame(Note = "Rerun the data pipeline to populate source references (older shiny_data/ export)."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }

    df <- target_lookup %>%
      arrange(primary_sector) %>%
      mutate(
        reference = case_when(
          !is.na(official_url) & nzchar(official_url) ~ paste0(
            '<a href="', official_url, '" target="_blank" rel="noopener noreferrer">',
            ifelse(!is.na(source_organization) & nzchar(source_organization),
                   source_organization, "Source"),
            '</a>'
          ),
          !is.na(source_organization) ~ source_organization,
          TRUE ~ ""
        )
      ) %>%
      select(primary_sector, sub_sector, reference, source_document, metric, scope_coverage)

    datatable(
      df,
      rownames = FALSE,
      escape = -3,   # column 3 = "reference" -- the only column allowed to render raw HTML (the <a> link)
      options = list(pageLength = 20, dom = "t", scrollX = TRUE),
      colnames = c("Sector", "Sub-sector / Commodity", "Reference", "Source Document", "Metric", "Scope / Coverage")
    )
  })

  # Live view of the Hertwich & Wood (2018) ratio table -- exactly what
  # get_scope23_ratio() reads to build every v1 Scope 2/3 estimate
  # (forecast, Own Goal target, Industry Target baseline) elsewhere in
  # the app. Shown here so the Overview's Scope 2/3 methodology section
  # has real numbers, not just a description of the approach.
  output$scope23_ratio_table_view <- renderDT({
    df <- scope23_ratio_table %>%
      arrange(primary_sector) %>%
      mutate(
        scope2_pct = paste0(round(scope2_multiplier * 100, 1), "%"),
        scope3_pct = paste0(round(scope3_multiplier * 100, 1), "%")
      ) %>%
      select(primary_sector, ipcc_bucket, scope2_pct, scope3_pct, scope23_confidence)

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 20, dom = "t", scrollX = TRUE),
      colnames = c("Sector", "IPCC Bucket", "Scope 2 (% of Scope 1)", "Scope 3 (% of Scope 1)", "Confidence")
    )
  })

  # Coverage summary across EVERY sector this app supports -- one row
  # each, showing whether its Scope 3 category split is backed by an
  # exact CDP sector match, an approximate one, or no match at all.
  # Built directly from scope3_sector_category_shares/get_scope3_
  # category_shares() -- the same source every live calculation uses.
  output$overview_scope3_sector_coverage_table <- renderDT({
    sectors <- unique(scope23_ratio_table$primary_sector)
    rows <- lapply(sectors, function(s) {
      info <- get_scope3_category_shares(s)
      data.frame(primary_sector = s, match_type = info$match_type, source = info$source, stringsAsFactors = FALSE)
    })
    df <- bind_rows(rows) %>% arrange(match_type)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 20, dom = "t", scrollX = TRUE),
      colnames = c("Sector", "Match Type", "Source / Basis")
    ) %>%
      formatStyle(
        "match_type", target = "row",
        backgroundColor = styleEqual(c("exact", "approximate", "none"), c("#EAFAF1", "#FEF9E7", "#FDEDEC"))
      )
  })

  # Same idea, one level deeper: the Chemicals-specific 15-category
  # split shown in Overview section 3, sourced from get_scope3_category_
  # shares() -- the SAME function every live per-company calculation
  # uses, so this table can never drift from what the app actually does.
  output$overview_scope3_category_table <- renderDT({
    share_info <- get_scope3_category_shares("Chemicals")
    df <- scope3_categories %>%
      mutate(
        share_pct = paste0(round(share_info$shares[as.character(cat_id)] * 100, 1), "%"),
        basis = case_when(
          cat_id %in% c(1, 11)    ~ "Real, Chemicals-specific (CDP 2024)",
          cat_id %in% c(6, 7)     ~ "Real, cross-sector average (CDP 2024)",
          cat_id %in% c(2, 3, 4, 9) ~ "Named relevant, no exact figure -- split evenly",
          TRUE                     ~ "Not identified as relevant -- residual split evenly"
        )
      ) %>%
      select(cat_id, cat_name, share_pct, basis)

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 15, dom = "t"),
      colnames = c("Category #", "GHG Protocol Category", "Share (Chemicals)", "Basis")
    )
  })

  # ---------------- PROJECT CATALOG (editable, feeds Portfolio Curation Engine) ----------------

  catalog_rv <- reactiveVal(project_catalog_default)

  output$project_catalog_table <- renderDT({
    df <- catalog_rv() %>%
      mutate(margin_per_ton = buyer_price - dev_cost) %>%
      select(project_id, project_type, methodology_name, mechanism, action, methodology_code,
             geography, state, developer, buyer_price, dev_cost, supply_tons, margin_per_ton,
             applicable_scope, confidence, notes)

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 12, dom = "t", scrollX = TRUE),
      colnames = c("Project ID", "Project (specific instance)", "Methodology", "Mechanism", "Action",
                   "Methodology Code", "Region", "State", "Developer", "Buyer Price ($/ton)",
                   "Dev Cost ($/ton)", "Available Supply (t)", "Margin ($/ton)",
                   "Applicable Scope", "Bucket Confidence", "Registry Notes"),
      # disabled: project_id(0) -- generated, not hand-edited; project_type(1)/
      # methodology_name(2)/mechanism(3)/action(4)/methodology_code(5) --
      # structural taxonomy; margin_per_ton(12) -- computed; applicable_scope(13)
      # -- structural (which gap it can count against, not a free edit);
      # confidence(14)/notes(15) -- carried over from the registry for review,
      # not meant to be edited here. Everything else (geography, state,
      # developer, buyer_price, dev_cost, supply_tons) is editable.
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5, 12, 13, 14, 15)))
    )
  })

  observeEvent(input$project_catalog_table_cell_edit, {
    info <- input$project_catalog_table_cell_edit
    df <- catalog_rv()
    # BUGFIX (was): mapped displayed column POSITION directly onto the
    # underlying data frame's column position with a hardcoded "+1
    # offset," which only worked because the two column orders happened
    # to match exactly -- any future reordering or added display column
    # (like this one) would have silently written edits into the WRONG
    # field. Now maps by column NAME instead, which stays correct
    # regardless of how the displayed column set changes.
    display_cols <- c("project_id", "project_type", "methodology_name", "mechanism", "action", "methodology_code",
                       "geography", "state", "developer", "buyer_price", "dev_cost", "supply_tons",
                       "margin_per_ton", "confidence", "notes")
    col_name <- display_cols[info$col + 1]
    req(!is.na(col_name), col_name %in% names(df))
    df[info$row, col_name] <- DT::coerceValue(info$value, df[info$row, col_name])
    catalog_rv(df)
  })

  # Adds a brand-new methodology row directly to the live catalog -- for
  # when a real new methodology needs to go in without touching R code or
  # methodology_registry.csv. Builds a row with the EXACT same schema as
  # every existing catalog row (including `key`, computed the same way
  # the original catalog derived it from mechanism + action), so it works
  # identically in the LP, concentration caps, and every display.
  observeEvent(input$pce_add_methodology_btn, {
    project_type <- trimws(input$pce_new_project_type)
    methodology_code <- trimws(input$pce_new_methodology_code)
    state_val <- if (nzchar(input$pce_new_state)) input$pce_new_state else NA_character_

    # BUGFIX (was): blocked adding a methodology_code that already existed
    # ANYWHERE in the catalog. That made sense when 1 row = 1 methodology,
    # but now methodology_code is deliberately shared across multiple
    # PROJECT instances of the same methodology (per the geographic
    # project-level restructuring) -- so a repeated code is now the
    # NORMAL case (adding another project instance under an existing
    # methodology), not an error. What's still checked: the exact same
    # (methodology_code, state) pair isn't already in the catalog, since
    # THAT would be a genuine duplicate project, not a new instance.
    existing <- catalog_rv() %>% filter(methodology_code == !!methodology_code)
    is_new_methodology <- nrow(existing) == 0
    duplicate_instance <- !is_new_methodology && any(
      (is.na(existing$state) & is.na(state_val)) | (!is.na(existing$state) & !is.na(state_val) & existing$state == state_val)
    )

    errors <- c()
    if (!nzchar(project_type)) errors <- c(errors, "Project Type is required.")
    if (!nzchar(methodology_code)) errors <- c(errors, "Methodology Code is required.")
    if (is.na(state_val)) errors <- c(errors, "US State is required -- every project needs a real location for proximity matching and the map.")
    if (duplicate_instance) {
      errors <- c(errors, paste0(
        "A project under methodology \"", methodology_code, "\" already exists in ", state_val,
        " -- give this instance a different State, or edit the existing row instead."
      ))
    }
    if (is.na(input$pce_new_price) || input$pce_new_price <= 0) errors <- c(errors, "Price ($/t) must be a positive number.")
    if (is.na(input$pce_new_supply) || input$pce_new_supply <= 0) errors <- c(errors, "Supply (t) must be a positive number.")

    if (length(errors) > 0) {
      output$pce_add_methodology_status <- renderUI({
        tags$div(
          style = "background:#FDEDEC; border:1px solid #E74C3C; border-radius:6px; padding:0.6rem 0.9rem; margin-top:0.6rem;",
          tags$b("Couldn't add this methodology:"),
          tags$ul(lapply(errors, tags$li))
        )
      })
      return()
    }

    # Same mapping the original catalog used to derive bucket key from
    # mechanism + action -- Community-based has only one bucket (avoidance)
    # in this app today, regardless of which Action is picked for it.
    new_key <- if (input$pce_new_mechanism == "Nature-based" && input$pce_new_action == "Avoidance") {
      "nat_avoid"
    } else if (input$pce_new_mechanism == "Nature-based" && input$pce_new_action == "Removal") {
      "nat_removal"
    } else if (input$pce_new_mechanism == "Technology-based" && input$pce_new_action == "Avoidance") {
      "tech_avoid"
    } else if (input$pce_new_mechanism == "Technology-based" && input$pce_new_action == "Removal") {
      "tech_removal"
    } else {
      "comm_avoid"
    }

    dev_cost_val <- if (!is.na(input$pce_new_dev_cost) && input$pce_new_dev_cost > 0) {
      input$pce_new_dev_cost
    } else {
      round(input$pce_new_price * 0.55)   # same illustrative 45-65% margin assumption used elsewhere, midpoint
    }

    # methodology_name: if this code already exists, reuse ITS existing
    # methodology_name (so the methodology-level rollup stays labeled
    # consistently even if this new instance's own project_type differs
    # slightly in wording); otherwise this new project_type IS the
    # methodology's name, since it's the first instance of it.
    methodology_name_val <- if (is_new_methodology) project_type else existing$methodology_name[1]
    next_instance <- if (is_new_methodology) 1 else max(as.numeric(sub(paste0("^", methodology_code, "-"), "", existing$project_id)), na.rm = TRUE) + 1
    project_id_val <- paste0(methodology_code, "-", next_instance)

    new_row <- data.frame(
      project_type     = paste0(project_type, " (", state_val, ")"),
      mechanism        = input$pce_new_mechanism,
      action           = input$pce_new_action,
      methodology_code = methodology_code,
      methodology_name = methodology_name_val,
      project_id       = project_id_val,
      geography        = us_region_lookup[[state_val]],
      developer        = if (nzchar(trimws(input$pce_new_developer))) trimws(input$pce_new_developer) else "Unspecified",
      buyer_price      = input$pce_new_price,
      dev_cost         = dev_cost_val,
      supply_tons      = input$pce_new_supply,
      key              = new_key,
      confidence       = "User-added",
      notes            = "Added manually via the app -- not from methodology_registry.csv.",
      state            = state_val,
      applicable_scope = "any",
      stringsAsFactors = FALSE
    )

    catalog_rv(bind_rows(catalog_rv(), new_row))

    output$pce_add_methodology_status <- renderUI({
      tags$div(
        style = "background:#EAFAF1; border:1px solid #27AE60; border-radius:6px; padding:0.6rem 0.9rem; margin-top:0.6rem;",
        tags$b("Added: "), new_row$project_type, " (", methodology_code, ") -- now included in the optimization above."
      )
    })

    # Clear the form for the next entry
    updateTextInput(session, "pce_new_project_type", value = "")
    updateTextInput(session, "pce_new_methodology_code", value = "")
    updateNumericInput(session, "pce_new_price", value = NA)
    updateNumericInput(session, "pce_new_dev_cost", value = NA)
    updateNumericInput(session, "pce_new_supply", value = NA)
    updateSelectInput(session, "pce_new_state", selected = "")
    updateTextInput(session, "pce_new_developer", value = "")
  })

  # ---------------- PRICING TAB ----------------

  bucket_display_names <- c(
    nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
    tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
    comm_avoid = "Community-based avoidance"
  )

  pricing_bucket_choices <- reactive({
    req(input$pricing_bucket_filter)
    methodology_price_history %>%
      filter(key %in% input$pricing_bucket_filter) %>%
      distinct(project_id, methodology_code, project_type) %>%
      arrange(project_type)
  })

  output$pricing_methodology_selector <- renderUI({
    choices_df <- pricing_bucket_choices()
    req(nrow(choices_df) > 0)
    # BUGFIX (was): keyed on methodology_code, which is now deliberately
    # SHARED across a methodology's multiple project instances -- picking
    # one instance from this dropdown would silently plot every instance
    # sharing that code, not just the one the label named. project_id is
    # unique per instance, so selection now matches the label exactly.
    choices <- setNames(choices_df$project_id, paste0(choices_df$project_type, " (", choices_df$methodology_code, ")"))
    # Default to the first 8 in the currently-checked buckets, not all of
    # them -- up to 46 lines on one chart by default would be unreadable;
    # the user can add more or fewer via this selector.
    default_selected <- head(choices, 8)
    selectizeInput(
      "pricing_methodology_select", "Methodologies to plot",
      choices = choices, selected = as.character(default_selected), multiple = TRUE
    )
  })

  pricing_filtered_history <- reactive({
    req(input$pricing_bucket_filter)
    df <- methodology_price_history %>% filter(key %in% input$pricing_bucket_filter)
    if (!is.null(input$pricing_methodology_select) && length(input$pricing_methodology_select) > 0) {
      df <- df %>% filter(project_id %in% input$pricing_methodology_select)
    }
    df
  })

  output$pricing_history_plot <- renderPlot({
    df <- pricing_filtered_history()
    req(nrow(df) > 0)
    df <- df %>% mutate(bucket = bucket_display_names[key])

    ggplot(df, aes(x = date, y = price, color = project_type, linetype = bucket)) +
      geom_line(linewidth = 0.9) +
      geom_point(data = df %>% group_by(project_id) %>% filter(date == max(date)) %>% ungroup(),
                 size = 2.2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        subtitle = "Solid point = latest price, currently used in the Portfolio Curation Engine",
        x = NULL, y = "Price ($/tCO2e)", color = "Methodology", linetype = "Bucket"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5), legend.position = "right")
  })

  output$pricing_snapshot_table <- renderDT({
    df <- pricing_filtered_history()
    req(nrow(df) > 0)
    snapshot <- df %>%
      group_by(project_id, methodology_code, project_type, key) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      mutate(bucket = bucket_display_names[key]) %>%
      arrange(desc(price)) %>%
      select(project_type, methodology_code, bucket, price)

    datatable(
      snapshot, rownames = FALSE,
      options = list(pageLength = 15, dom = "tp"),
      colnames = c("Project Type", "Code", "Bucket", "Current Price ($/t)")
    )
  })

  # ---- Excluded-from-portfolio reference table (supporting tools + the
  # unresolved "other" row) -- kept visible so nothing from the registry
  # is silently dropped; each row shows WHY it's excluded from pricing/
  # allocation rather than just disappearing. ----
  output$methodology_excluded_table <- renderDT({
    df <- methodology_registry_excluded %>%
      select(methodology_code, registry_family, title_short, excluded_reason, notes)
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE),
      colnames = c("Methodology Code", "Registry Family", "Title", "Why Excluded", "Notes")
    )
  })

  # ---------------- SECTOR VIEW ----------------

  sector_hist <- reactive({
    hist_by_sector %>% filter(primary_sector == input$sector_select)
  })

  sector_fore <- reactive({
    fore_by_sector %>% filter(primary_sector == input$sector_select)
  })

  sector_targ <- reactive({
    target_by_sector %>% filter(primary_sector == input$sector_select)
  })

  output$target_meta_sector <- renderUI({
    tl <- target_lookup %>% filter(primary_sector == input$sector_select)
    if (nrow(tl) == 0) {
      tags$p(em("No published target pathway for this sector."))
    } else {
      tagList(
        tags$b("Target: "),
        tags$span(
          scales::percent(tl$reduction_fraction[1], accuracy = 0.1),
          " reduction by ", tl$target_year[1],
          " (baseline ", tl$baseline_year[1], ")"
        ),
        tags$br(),
        tags$b("Implied annual rate: "),
        tags$span(scales::percent(tl$annual_rate[1], accuracy = 0.1)),
        if (!is.na(tl$confidence[1])) {
          tagList(tags$br(), tags$b("Confidence: "), tags$span(tl$confidence[1]))
        },
        if (!is.na(tl$caveat[1]) && nzchar(tl$caveat[1])) {
          tagList(tags$br(), tags$em(tl$caveat[1]))
        }
      )
    }
  })

  # Compact stat-card row: quick-glance numbers before the chart.
  # States card degrades gracefully if state data wasn't loaded (older
  # shiny_data export), same pattern used elsewhere in this app.
  output$sector_stat_cards <- renderUI({
    req(input$sector_select)
    sec_fac <- facility_lookup %>% filter(primary_sector == input$sector_select)
    fac_count  <- nrow(sec_fac)
    total_2023 <- sum(sec_fac$emissions_2023, na.rm = TRUE)

    n_states <- if (has_state_data) {
      hist_by_sector_state %>%
        filter(primary_sector == input$sector_select, n_facilities > 0) %>%
        distinct(state) %>% nrow()
    } else {
      NA
    }

    gap_final <- tryCatch({
      df <- sector_gap()
      row <- df %>% filter(year == max(year))
      if (nrow(row) > 0) row$gap_mt[1] else NA
    }, error = function(e) NA)

    stat_card <- function(value, label) {
      tags$div(
        style = "flex:1; background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center;",
        tags$div(style = "font-size:22px; font-weight:700; color:#2C3E50;", value),
        tags$div(style = "font-size:11.5px; color:#7F8C8D;", label)
      )
    }

    tags$div(
      style = "display:flex; gap:12px; margin-bottom:16px;",
      stat_card(comma(fac_count), "Facilities"),
      stat_card(paste0(comma(round(total_2023 / 1e6, 2)), " Mt"), paste0(last_hist_year, " emissions (sum)")),
      stat_card(if (!is.na(n_states)) n_states else "--", if (has_state_data) "States" else "States (rerun pipeline)"),
      stat_card(if (!is.na(gap_final)) paste0(comma(round(gap_final, 2)), " Mt") else "--", paste0(last_fore_year, " gap"))
    )
  })

  output$sector_plot <- renderPlot({
    sec   <- input$sector_select
    col   <- sector_colors[[sec]]
    if (is.null(col) || is.na(col)) col <- "#34495E"

    p <- ggplot() +
      geom_line(
        data = sector_hist(),
        aes(x = year, y = emissions / 1e6),
        color = col, linewidth = 1.1
      ) +
      geom_point(
        data = sector_hist(),
        aes(x = year, y = emissions / 1e6),
        color = col, size = 1.6
      )

    if (isTRUE(input$show_forecast_sector) && nrow(sector_fore()) > 0) {
      bridge_fore <- bind_rows(
        sector_hist() %>% filter(year == last_hist_year) %>%
          transmute(year, p50 = emissions),
        sector_fore()
      )
      p <- p +
        geom_line(
          data = bridge_fore,
          aes(x = year, y = p50 / 1e6),
          color = col, linewidth = 1.1, linetype = "dashed"
        )
    }

    if (isTRUE(input$show_target_sector) && nrow(sector_targ()) > 0) {
      bridge_targ <- bind_rows(
        sector_hist() %>% filter(year == last_hist_year) %>%
          transmute(year, target = emissions),
        sector_targ()
      )
      p <- p +
        geom_line(
          data = bridge_targ,
          aes(x = year, y = target / 1e6),
          color = target_color, linewidth = 1.1, linetype = "dotted"
        )
    }

    p +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = comma) +
      labs(
        title = sec,
        subtitle = "Solid = observed | Dashed = model forecast | Dotted = target pathway",
        x = NULL, y = "Emissions (Mt CO2e)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
  })

  output$sector_table <- renderDT({
    hist_tbl <- sector_hist() %>% transmute(year, value = emissions / 1e6, series = "Observed")
    fore_tbl <- sector_fore() %>% transmute(year, value = p50 / 1e6, series = "Forecast")
    targ_tbl <- sector_targ() %>% transmute(year, value = target / 1e6, series = "Target")

    bind_rows(hist_tbl, fore_tbl, targ_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      mutate(across(where(is.numeric) & !matches("year"), ~round(.x, 3))) %>%
      datatable(
        options = list(pageLength = 15, dom = "tp"),
        rownames = FALSE,
        colnames = c("Year", "Observed (Mt)", "Forecast (Mt)", "Target (Mt)")
      )
  })

  # Gap = forecast minus target, in years where both exist
  sector_gap <- reactive({
    fc <- sector_fore()
    tg <- sector_targ()
    req(nrow(fc) > 0, nrow(tg) > 0)
    inner_join(
      fc %>% select(year, p50),
      tg %>% select(year, target),
      by = "year"
    ) %>%
      mutate(gap_mt = (p50 - target) / 1e6)
  })

  output$sector_gap_plot <- renderPlot({
    df <- sector_gap()
    req(nrow(df) > 0)
    make_credit_bar(df, "gap_mt", "Gap (Mt CO2e)", x_breaks_arg = x_breaks)
  })

  # ---------------- EU SECTOR VIEW / EU FACILITY VIEW ----------------
  # Completely independent of every US reactive above and below --
  # reads only the tables loaded from the separate shiny_data_eu/
  # folder. Nothing here can affect the US tabs' behavior, and nothing
  # in the US tabs affects these.

  output$eu_data_available <- reactive({ has_eu_data })
  outputOptions(output, "eu_data_available", suspendWhenHidden = FALSE)

  eu_status_message <- function() {
    if (has_eu_data) return(NULL)
    tags$div(
      style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:8px; padding:1rem 1.5rem; margin:1rem 0;",
      tags$b("Europe data not available yet."), " Run satya_carbon_v4_01_data_pipeline_europe.R ",
      "to generate the shiny_data_eu/ folder this tab reads from."
    )
  }
  output$eu_data_status     <- renderUI({ eu_status_message() })
  output$eu_data_status_fac <- renderUI({ eu_status_message() })

  eu_scope_note_ui <- function() {
    req(has_eu_data)
    tags$div(
      style = "background:#EBF5FB; border-left:4px solid #2980B9; border-radius:4px; padding:0.6rem 1rem; margin-bottom:12px; font-size:12.5px;",
      tags$b("Approximate Scope 1 from EPRTR air releases "), "(CO2 + CH4 + N2O + SF6, IPCC AR6 GWP-converted). ",
      "HFC/PFC excluded -- mixed-gas categories in the source data, no single defensible GWP. Not an official Scope 1 inventory."
    )
  }
  output$eu_scope_note     <- renderUI({ eu_scope_note_ui() })
  output$eu_scope_note_fac <- renderUI({ eu_scope_note_ui() })

  output$eu_forecast_note <- renderUI({
    req(has_eu_data)
    tags$div(
      style = "background:#EAFAF1; border-left:4px solid #27AE60; border-radius:4px; padding:0.6rem 1rem; margin-bottom:12px; font-size:12.5px;",
      tags$b("Real panel forecast, not a placeholder: "), "a facility-level fixed/random-effects model ",
      "(log emissions ~ year + sector + sector:year + country + facility random intercept/slope), fit on ",
      "this EU data specifically -- same modeling intuition as the US pipeline's model, independently fit, ",
      "not reused. No target line yet -- no equivalent EU sector-targets workbook exists (see the pipeline ",
      "script's own header)."
    )
  })

  observe({
    req(has_eu_data)
    updateSelectInput(session, "eu_sector_select", choices = sector_list_eu_official, selected = sector_list_eu_official[1])
    updateSelectInput(session, "eu_sector_bucket_select", choices = sector_bucket_list_eu, selected = sector_bucket_list_eu[1])
    updateSelectizeInput(session, "eu_facility_select", choices = facility_choices_eu,
                          selected = facility_choices_eu[1], server = TRUE)
  })

  # ---- Official EEA sector sheet (historical only) ----

  eu_sector_hist <- reactive({
    req(has_eu_data, input$eu_sector_select)
    hist_by_sector_eu %>% filter(sector == input$eu_sector_select) %>% arrange(year)
  })

  output$eu_sector_meta <- renderUI({
    df <- eu_sector_hist()
    req(nrow(df) > 0)
    tags$div(
      style = "font-size:12.5px; color:#5D6D7E;",
      tags$b("Source: "), "Air_Releases_Sector sheet (EEA official rollup)", tags$br(),
      tags$b("Years covered: "), min(df$year), "-", max(df$year)
    )
  })

  output$eu_sector_plot <- renderPlot({
    df <- eu_sector_hist()
    req(nrow(df) > 0)
    ggplot(df, aes(x = year, y = emissions / 1e6)) +
      geom_line(color = "#2980B9", linewidth = 1.1) +
      geom_point(color = "#2980B9", size = 1.8) +
      scale_x_continuous(breaks = eu_x_breaks[eu_x_breaks <= 2024]) +
      scale_y_continuous(labels = comma) +
      labs(
        title = input$eu_sector_select,
        subtitle = "Observed Scope 1 (approx.), Mt CO2e -- Europe, official EEA sector (EPRTR)",
        x = NULL, y = "Mt CO2e"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_sector_table <- renderDT({
    df <- eu_sector_hist()
    req(nrow(df) > 0)
    df %>%
      transmute(Year = year, `Emissions (Mt CO2e)` = round(emissions / 1e6, 3)) %>%
      datatable(options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  })

  # ---- Model's own sector grouping (historical + real forecast) ----

  eu_sector_bucket_hist <- reactive({
    req(has_eu_data, input$eu_sector_bucket_select)
    hist_by_sector_bucket_eu %>% filter(sector_bucket == input$eu_sector_bucket_select) %>% arrange(year)
  })

  eu_sector_bucket_fore <- reactive({
    req(has_eu_data, input$eu_sector_bucket_select)
    fore_by_sector_bucket_eu %>% filter(sector_bucket == input$eu_sector_bucket_select) %>% arrange(year)
  })

  output$eu_sector_bucket_meta <- renderUI({
    df <- eu_sector_bucket_hist()
    req(nrow(df) > 0)
    tags$div(
      style = "font-size:12.5px; color:#5D6D7E;",
      tags$b("Facilities in this bucket: "), max(df$n_facilities, na.rm = TRUE), tags$br(),
      tags$b("Years covered: "), min(df$year), "-", max(df$year)
    )
  })

  output$eu_sector_bucket_plot <- renderPlot({
    hist_df <- eu_sector_bucket_hist()
    fore_df <- eu_sector_bucket_fore()
    req(nrow(hist_df) > 0)

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions / 1e6), color = "#27AE60", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions / 1e6), color = "#27AE60", size = 1.8)

    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(
        hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions),
        fore_df %>% select(year, p50)
      )
      p <- p + geom_line(data = bridge, aes(x = year, y = p50 / 1e6), color = "#27AE60", linewidth = 1.1, linetype = "dashed")
    }

    p + scale_x_continuous(breaks = eu_x_breaks) + scale_y_continuous(labels = comma) +
      labs(
        title = input$eu_sector_bucket_select,
        subtitle = "Solid = observed | Dashed = model forecast -- Mt CO2e, Europe (EPRTR, model's sector grouping)",
        x = NULL, y = "Mt CO2e"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_sector_bucket_table <- renderDT({
    hist_tbl <- eu_sector_bucket_hist() %>% transmute(year, value = round(emissions / 1e6, 3), series = "Observed")
    fore_tbl <- eu_sector_bucket_fore() %>% transmute(year, value = round(p50 / 1e6, 3), series = "Forecast")
    req(nrow(hist_tbl) > 0)
    bind_rows(hist_tbl, fore_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      datatable(options = list(pageLength = 15, dom = "tp"), rownames = FALSE,
                colnames = c("Year", "Observed (Mt)", "Forecast (Mt)"))
  })

  # ---- Facility view (historical + real forecast) ----

  eu_fac_id <- reactive({
    req(has_eu_data, input$eu_facility_select)
    input$eu_facility_select
  })

  eu_fac_hist <- reactive({
    req(has_eu_data)
    eu_panel_filtered %>% filter(facility_id == eu_fac_id()) %>% arrange(year)
  })

  eu_fac_fore <- reactive({
    req(has_eu_data)
    future_pred_eu %>% filter(facility_id == eu_fac_id()) %>% arrange(year)
  })

  eu_fac_meta_row <- reactive({
    req(has_eu_data)
    facility_lookup_eu %>% filter(facility_id == eu_fac_id()) %>% slice(1)
  })

  output$eu_facility_meta <- renderUI({
    m <- eu_fac_meta_row()
    req(nrow(m) > 0)
    tags$div(
      style = "font-size:12.5px; color:#5D6D7E;",
      tags$b("Country: "), m$country[1], tags$br(),
      tags$b("Activity code: "), m$activity_code[1], tags$br(),
      tags$b("Model sector bucket: "), m$sector_bucket[1], tags$br(),
      tags$b("City: "), if (!is.na(m$city[1])) m$city[1] else "not reported"
    )
  })

  output$eu_facility_plot <- renderPlot({
    hist_df <- eu_fac_hist()
    fore_df <- eu_fac_fore()
    m <- eu_fac_meta_row()
    req(nrow(hist_df) > 0)

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = "#8E44AD", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = "#8E44AD", size = 1.8)

    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(
        hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions),
        fore_df %>% select(year, p50)
      )
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = "#8E44AD", linewidth = 1.1, linetype = "dashed")
    }

    p + scale_x_continuous(breaks = eu_x_breaks) + scale_y_continuous(labels = comma) +
      labs(
        title = if (nrow(m) > 0) m$facility_name[1] else "",
        subtitle = "Solid = observed | Dashed = model forecast -- tCO2e (approx. Scope 1), Europe (EPRTR)",
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_country_plot <- renderPlot({
    req(has_eu_data)
    top_countries <- hist_by_country_eu %>%
      filter(year == max(year)) %>%
      arrange(desc(emissions)) %>%
      head(15)
    ggplot(top_countries, aes(x = reorder(country, emissions), y = emissions / 1e6)) +
      geom_col(fill = "#2980B9") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        subtitle = paste0("Top 15 countries, ", max(hist_by_country_eu$year), " (Mt CO2e, approx. Scope 1)"),
        x = NULL, y = "Mt CO2e"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  # ---------------- EU COMPANY PROFILE ----------------
  # Mirrors the US Company Profile's real-company-matching pattern
  # exactly (existing-vs-new radio, hidden synced name field, two-tier
  # match: exact first-word key then substring fallback), reading only
  # EU tables. Narrower than the US version in exactly the places EU
  # data doesn't support (no Scope 2/3, no sourced target) -- see the
  # tab's own UI comment for why.

  output$eu_cp_data_status <- renderUI({
    if (has_eu_data) return(NULL)
    tags$div(
      style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:8px; padding:1rem 1.5rem; margin:1rem 0;",
      tags$b("Europe data not available yet."), " Run satya_carbon_v4_01_data_pipeline_europe.R first."
    )
  })

  updateSelectizeInput(session, "eu_cp_existing_picker", choices = eu_company_choices_alpha,
                        selected = character(0), server = TRUE)

  observe({
    val <- if (identical(input$eu_cp_mode, "existing")) {
      if (is.null(input$eu_cp_existing_picker)) "" else input$eu_cp_existing_picker
    } else {
      if (is.null(input$eu_cp_new_name)) "" else input$eu_cp_new_name
    }
    updateTextInput(session, "eu_cp_company_name", value = val)
  })

  eu_cp_matched_facilities <- reactive({
    req(has_eu_data)
    nm <- trimws(input$eu_cp_company_name)
    req(nzchar(nm))
    key <- extract_company(nm)
    fac <- facility_lookup_eu %>% filter(eu_company == key)
    if (nrow(fac) == 0) {
      nm_upper <- toupper(nm)
      fac <- facility_lookup_eu %>% filter(grepl(nm_upper, toupper(facility_name), fixed = TRUE))
    }
    if (nrow(fac) == 0) return(NULL)
    fac %>% arrange(desc(emissions_latest))
  })

  eu_cp_match <- reactive({
    fac <- eu_cp_matched_facilities()
    if (is.null(fac)) return(NULL)
    data.frame(company = extract_company(fac$facility_name[1]), n_facilities = nrow(fac), stringsAsFactors = FALSE)
  })

  observeEvent(input$eu_cp_company_name, {
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    if (!is.null(m)) {
      updateRadioButtons(session, "eu_cp_has_data", selected = "yes")
      fac <- eu_cp_matched_facilities()
      if (nrow(fac) > 0) {
        dom_sector <- fac %>% count(sector_bucket, wt = emissions_latest, sort = TRUE) %>% slice(1) %>% pull(sector_bucket)
        if (length(dom_sector) > 0) updateSelectInput(session, "eu_cp_sector", selected = as.character(dom_sector))
        dom_country <- fac$country[1]
        if (!is.null(dom_country) && !is.na(dom_country)) updateSelectInput(session, "eu_cp_country", selected = dom_country)
      }
    }
  }, ignoreInit = TRUE)

  output$eu_cp_match_status <- renderUI({
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    company_typed <- if (is.null(input$eu_cp_company_name)) "" else trimws(input$eu_cp_company_name)
    if (is.null(m)) {
      if (nzchar(company_typed)) {
        tags$div(
          style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:6px; padding:0.5rem 0.8rem; margin-bottom:10px; font-size:12px;",
          em("No match in the EU EPRTR database -- treated as a new company. Enter data manually below.")
        )
      } else NULL
    } else {
      fac <- eu_cp_matched_facilities()
      tags$div(
        style = "background:#EAFAF1; border:1px solid #A9DFBF; border-radius:6px; padding:0.5rem 0.8rem; margin-bottom:10px; font-size:12px;",
        tags$b("Matched: "), m$company[1], " -- real EPRTR data found for ", nrow(fac), " facilit",
        if (nrow(fac) == 1) "y" else "ies", ". Historical Scope 1 (approx.) auto-populated below."
      )
    }
  })

  eu_cp_user_data <- reactive({
    req(has_eu_data)
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    if (!is.null(m)) {
      fac <- eu_cp_matched_facilities()
      return(
        eu_panel_filtered %>% filter(facility_id %in% fac$facility_id) %>%
          group_by(year) %>% summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>% arrange(year)
      )
    }
    req(input$eu_cp_has_data == "yes", input$eu_cp_emissions_csv)
    parse_scope_csv(input$eu_cp_emissions_csv)
  })

  eu_cp_forecast <- reactive({
    req(has_eu_data)
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    req(!is.null(m))
    fac <- eu_cp_matched_facilities()
    future_pred_eu %>% filter(facility_id %in% fac$facility_id) %>%
      group_by(year) %>% summarise(p50 = sum(p50, na.rm = TRUE), .groups = "drop") %>% arrange(year)
  })

  output$eu_cp_scope_note <- renderUI({
    req(has_eu_data)
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    tags$div(
      style = "background:#EBF5FB; border-left:4px solid #2980B9; border-radius:4px; padding:0.6rem 1rem; margin-bottom:12px; font-size:12.5px;",
      tags$b("Approximate Scope 1 from EPRTR "), "(CO2 + CH4 + N2O + SF6, IPCC AR6 GWP-converted). Not an official Scope 1 inventory.",
      if (is.null(m)) tags$span(" No forecast for a new, unmatched company -- the panel model only covers matched real EPRTR facilities.")
    )
  })

  output$eu_cp_trend_plot <- renderPlot({
    hist_df <- eu_cp_user_data()
    req(nrow(hist_df) > 0)
    fore_df <- tryCatch(eu_cp_forecast(), error = function(e) tibble())

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = "#34495E", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = "#34495E", size = 1.8)

    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions), fore_df)
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = "#34495E", linewidth = 1.1, linetype = "dashed")
    }

    if (isTRUE(input$eu_cp_set_target)) {
      base_year <- max(hist_df$year); base_val <- hist_df$emissions[hist_df$year == base_year][1]
      target_years <- base_year:input$eu_cp_target_year
      target_vals <- base_val * (1 - input$eu_cp_target_reduction / 100) ^ ((target_years - base_year) / (input$eu_cp_target_year - base_year))
      targ_df <- tibble(year = target_years, target = target_vals)
      p <- p + geom_line(data = targ_df, aes(x = year, y = target), color = target_color, linewidth = 1.1, linetype = "dotted")
    }

    p + scale_x_continuous(breaks = eu_x_breaks) + scale_y_continuous(labels = comma) +
      labs(subtitle = "Solid = observed | Dashed = model forecast | Dotted = your own target (if set)",
           x = NULL, y = "Emissions (tCO2e, approx. Scope 1)") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_cp_table <- renderDT({
    hist_tbl <- eu_cp_user_data() %>% transmute(year, value = round(emissions), series = "Observed")
    req(nrow(hist_tbl) > 0)
    fore_df <- tryCatch(eu_cp_forecast(), error = function(e) tibble())
    fore_tbl <- if (nrow(fore_df) > 0) fore_df %>% transmute(year, value = round(p50), series = "Forecast") else tibble()
    bind_rows(hist_tbl, fore_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      datatable(options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  })

  output$eu_cp_facilities_content <- renderUI({
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    if (is.null(m)) {
      return(tags$p(em("Facility-level detail is only available for companies matched to real EPRTR facilities.")))
    }
    fac <- eu_cp_matched_facilities()
    tagList(
      h5(paste0(m$company[1], " -- company rollup (", nrow(fac), if (nrow(fac) == 1) " facility)" else " facilities)")),
      plotOutput("eu_cp_rollup_plot", height = "360px"),
      hr(),
      h5("Individual facility detail"),
      selectInput("eu_cp_facility_drill", NULL, choices = setNames(fac$facility_id, fac$facility_name)),
      plotOutput("eu_cp_drill_plot", height = "360px")
    )
  })

  output$eu_cp_rollup_plot <- renderPlot({
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    req(!is.null(m))
    hist_df <- eu_cp_user_data()
    fore_df <- tryCatch(eu_cp_forecast(), error = function(e) tibble())
    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = "#34495E", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = "#34495E", size = 1.6)
    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions), fore_df)
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = "#34495E", linewidth = 1.1, linetype = "dashed")
    }
    p + scale_x_continuous(breaks = eu_x_breaks) + scale_y_continuous(labels = comma) +
      labs(subtitle = "Company rollup | Solid = observed | Dashed = forecast", x = NULL, y = "Emissions (tCO2e)") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_cp_drill_plot <- renderPlot({
    req(input$eu_cp_facility_drill)
    fid <- input$eu_cp_facility_drill
    fname <- facility_lookup_eu$facility_name[facility_lookup_eu$facility_id == fid][1]
    hist_df <- eu_panel_filtered %>% filter(facility_id == fid) %>% arrange(year)
    fore_df <- future_pred_eu %>% filter(facility_id == fid) %>% select(year, p50)
    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = "#8E44AD", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = "#8E44AD", size = 1.6)
    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions), fore_df)
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = "#8E44AD", linewidth = 1.1, linetype = "dashed")
    }
    p + scale_x_continuous(breaks = eu_x_breaks) + scale_y_continuous(labels = comma) +
      labs(title = fname, subtitle = "Solid = observed | Dashed = forecast", x = NULL, y = "Emissions (tCO2e)") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  # ---------------- EU PORTFOLIO MIX ----------------
  # Reuses solve_portfolio_lp(), compute_proximity_score(), catalog_rv(),
  # and build_facility_map_world() directly -- none of those were ever
  # US-specific. Simplified relative to the US version: uniform bucket
  # preference (no per-bucket slider panel), proximity + cost-
  # effectiveness only. Gap sizing ties to whichever company is active
  # on EU Company Profile.

  output$eu_pm_data_status <- renderUI({
    if (has_eu_data) return(NULL)
    tags$div(
      style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:8px; padding:1rem 1.5rem; margin:1rem 0;",
      tags$b("Europe data not available yet."), " Run satya_carbon_v4_01_data_pipeline_europe.R first."
    )
  })

  output$eu_pm_company_readout <- renderUI({
    req(has_eu_data)
    m <- tryCatch(eu_cp_match(), error = function(e) NULL)
    company_name <- if (is.null(input$eu_cp_company_name)) "" else input$eu_cp_company_name
    tags$div(
      style = "font-size:12.5px; color:#7F8C8D; margin-bottom:6px;",
      tags$b("Active company: "),
      tags$span(style = "color:#2C3E50; font-weight:600;",
                if (nzchar(company_name)) company_name else "not set"),
      tags$br(),
      tags$em("Set on the \"EU Company Profile\" tab.")
    )
  })

  # Auto-populate the gap from the active company's forecast, same
  # spirit as the US tab's own gap auto-sizing -- override freely.
  observe({
    req(has_eu_data, input$eu_pm_gap_year)
    fore_df <- tryCatch(eu_cp_forecast(), error = function(e) tibble())
    if (nrow(fore_df) > 0) {
      match_row <- fore_df %>% filter(year == as.integer(input$eu_pm_gap_year))
      if (nrow(match_row) > 0) {
        updateNumericInput(session, "eu_pm_gap_tons", value = round(match_row$p50[1]))
      }
    }
  })

  eu_run_lp_alloc <- function(gap_tons, budget) {
    req(!is.na(budget), budget >= 0, !is.na(gap_tons), gap_tons >= 0)
    catalog <- catalog_rv() %>% mutate(margin_per_ton = buyer_price - dev_cost)

    facility_country <- if (is.null(input$eu_cp_country)) "" else input$eu_cp_country
    has_facility <- nzchar(facility_country)
    beta <- if (has_facility) input$eu_pm_proximity_weight / 100 else 0
    uniform_weight <- 1 - beta   # no bucket-preference dimension for EU -- see block comment above

    proximity_score <- compute_proximity_score(catalog, facility_country, NA_character_, NA_character_)
    catalog$proximity_score <- proximity_score
    catalog$proximity_tier <- if (!has_facility) {
      "Not evaluated (no company set)"
    } else {
      case_when(
        proximity_score == 0.6  ~ "Same country",
        proximity_score == 0.35 ~ "Same region",
        TRUE                    ~ "Elsewhere globally"
      )
    }

    obj_weights <- uniform_weight * rep(1, nrow(catalog)) + beta * proximity_score

    bucket_cap <- input$eu_pm_bucket_cap / 100
    tier_min_frac <- switch(input$eu_pm_claim_tier, "silver" = 0.10, "gold" = 0.50, "platinum" = 1.00, 0)

    ideal_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = NULL, tier_min_frac = 0, bucket_cap = bucket_cap)
    catalog$ideal_tons <- floor(ideal_sol$tons)

    funded_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = budget, tier_min_frac = tier_min_frac, bucket_cap = bucket_cap)
    tier_shortfall <- tier_min_frac > 0 && funded_sol$status != 0
    if (tier_shortfall) {
      funded_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = budget, tier_min_frac = 0, bucket_cap = bucket_cap)
    }
    catalog$funded_tons <- floor(funded_sol$tons)
    catalog$funded_cost <- round(catalog$funded_tons * catalog$buyer_price)
    catalog$ideal_cost  <- round(catalog$ideal_tons * catalog$buyer_price)

    attr(catalog, "tier_shortfall")   <- tier_shortfall
    attr(catalog, "gap_tons")         <- gap_tons
    attr(catalog, "budget")           <- budget
    attr(catalog, "facility_country") <- facility_country
    catalog
  }

  eu_pm_alloc <- reactive({
    eu_run_lp_alloc(input$eu_pm_gap_tons, input$eu_pm_budget)
  })

  output$eu_pm_context <- renderUI({
    alloc <- eu_pm_alloc()
    gap_tons <- attr(alloc, "gap_tons")
    budget <- attr(alloc, "budget")
    facility_country <- attr(alloc, "facility_country")
    has_facility <- nzchar(facility_country)
    total_funded_tons <- sum(alloc$funded_tons)
    total_funded_cost <- sum(alloc$funded_cost)
    coverage_pct <- if (gap_tons > 0) round(total_funded_tons / gap_tons * 100, 1) else 0

    tagList(
      tags$b("Gap: "), tags$span(comma(round(gap_tons)), " tCO2e"),
      tags$span(" | ", tags$b("Budget: "), "$", comma(budget)), tags$br(),
      tags$b("Recommended: "), tags$span(comma(total_funded_tons), " tCO2e (", coverage_pct, "% of gap), $", comma(total_funded_cost)),
      tags$br(),
      tags$b("Company country: "),
      tags$span(if (has_facility) facility_country else "not specified -- proximity ignored"),
      if (isTRUE(attr(alloc, "tier_shortfall"))) {
        tags$div(
          style = "background:#FDEDEC; border:1px solid #E74C3C; border-radius:4px; padding:0.3rem 0.6rem; margin-top:0.3rem;",
          tags$b("Claim tier not met: "), "the coverage floor can't be reached within budget/supply/concentration caps."
        )
      }
    )
  })

  output$eu_pm_tons_plot <- renderPlot({
    alloc <- eu_pm_alloc() %>% filter(funded_tons > 0)
    req(nrow(alloc) > 0)
    ggplot(alloc, aes(x = reorder(project_type, funded_tons), y = funded_tons, fill = key)) +
      geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
      scale_fill_manual(values = pme_colors, guide = "none") +
      labs(subtitle = "Tons recommended, by project", x = NULL, y = "tCO2e") +
      theme_minimal(base_size = 11) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_pm_spend_plot <- renderPlot({
    alloc <- eu_pm_alloc() %>% filter(funded_cost > 0)
    req(nrow(alloc) > 0)
    ggplot(alloc, aes(x = reorder(project_type, funded_cost), y = funded_cost, fill = key)) +
      geom_col() + coord_flip() + scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = pme_colors, guide = "none") +
      labs(subtitle = "Spend recommended, by project", x = NULL, y = "$") +
      theme_minimal(base_size = 11) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$eu_pm_map_eu <- renderPlot({
    alloc <- eu_pm_alloc()
    facility_country <- attr(alloc, "facility_country")
    m <- build_facility_map_eu_zoom(alloc, facility_country)
    if (is.null(m)) {
      ggplot() + annotate("text", x = 0, y = 0, label = "Map unavailable (rnaturalearth/sf not installed).", size = 4) + theme_void()
    } else m
  })

  output$eu_pm_map_world <- renderPlot({
    alloc <- eu_pm_alloc()
    facility_country <- attr(alloc, "facility_country")
    m <- build_facility_map_world(alloc, facility_country, NA_character_)
    if (is.null(m)) {
      ggplot() + annotate("text", x = 0, y = 0, label = "Map unavailable (rnaturalearth/sf not installed).", size = 4) + theme_void()
    } else m
  })

  output$eu_pm_table <- renderDT({
    alloc <- eu_pm_alloc() %>% filter(funded_tons > 0) %>%
      transmute(
        Project = project_type, Methodology = methodology_name, Country = country,
        `Proximity` = proximity_tier, `Tons Funded` = comma(funded_tons),
        `Cost` = paste0("$", comma(funded_cost))
      )
    req(nrow(alloc) > 0)
    datatable(alloc, options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  })

  # ---------------- PORTFOLIO MIX LATAM / ASIA / AUSTRALIA ----------------
  # One shared LP-allocation helper, reused by all three tabs below --
  # rather than tripling the same logic three times (the way it's
  # written once here, a bug fixed here is fixed for all three; three
  # separate near-identical copies would have meant three chances to
  # diverge). Reuses solve_portfolio_lp(), compute_proximity_score(),
  # catalog_rv(), and build_facility_map_region_zoom() -- none of
  # those were ever region-specific. Every number driving these three
  # tabs' gap sizing comes from synthetic_country_year -- clearly
  # labeled as such in every context (data badge, chart subtitle).

  region_run_lp_alloc <- function(gap_tons, budget, facility_country, proximity_weight_pct, bucket_cap_pct) {
    req(!is.na(budget), budget >= 0, !is.na(gap_tons), gap_tons >= 0)
    catalog <- catalog_rv() %>% mutate(margin_per_ton = buyer_price - dev_cost)

    has_facility <- nzchar(facility_country)
    beta <- if (has_facility) proximity_weight_pct / 100 else 0
    uniform_weight <- 1 - beta

    proximity_score <- compute_proximity_score(catalog, facility_country, NA_character_, NA_character_)
    catalog$proximity_score <- proximity_score
    catalog$proximity_tier <- if (!has_facility) {
      "Not evaluated"
    } else {
      case_when(
        proximity_score == 0.6  ~ "Same country",
        proximity_score == 0.35 ~ "Same region",
        TRUE                    ~ "Elsewhere globally"
      )
    }

    obj_weights <- uniform_weight * rep(1, nrow(catalog)) + beta * proximity_score
    bucket_cap <- bucket_cap_pct / 100

    ideal_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = NULL, tier_min_frac = 0, bucket_cap = bucket_cap)
    catalog$ideal_tons <- floor(ideal_sol$tons)

    funded_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = budget, tier_min_frac = 0, bucket_cap = bucket_cap)
    catalog$funded_tons <- floor(funded_sol$tons)
    catalog$funded_cost <- round(catalog$funded_tons * catalog$buyer_price)
    catalog$ideal_cost  <- round(catalog$ideal_tons * catalog$buyer_price)

    attr(catalog, "gap_tons") <- gap_tons
    attr(catalog, "budget") <- budget
    attr(catalog, "facility_country") <- facility_country
    catalog
  }

  # Shared renderers, parameterized by country/region/input-prefix, so
  # the three tabs' server wiring below is a short, uniform call each
  # rather than five near-identical render blocks apiece.
  region_pm_data_badge <- function(country) {
    tags$div(
      style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:6px; padding:0.5rem 0.8rem; margin-bottom:10px; font-size:12px;",
      tags$b("Synthetic data: "), country, "'s emissions are illustrative (no real facility-level source available yet), scaled to a realistic order of magnitude -- not sourced or reported figures."
    )
  }

  region_pm_context_ui <- function(alloc) {
    gap_tons <- attr(alloc, "gap_tons"); budget <- attr(alloc, "budget"); fc <- attr(alloc, "facility_country")
    total_funded_tons <- sum(alloc$funded_tons); total_funded_cost <- sum(alloc$funded_cost)
    coverage_pct <- if (gap_tons > 0) round(total_funded_tons / gap_tons * 100, 1) else 0
    tagList(
      tags$b("Country: "), tags$span(fc), tags$br(),
      tags$b("Gap: "), tags$span(comma(round(gap_tons)), " tCO2e"),
      tags$span(" | ", tags$b("Budget: "), "$", comma(budget)), tags$br(),
      tags$b("Recommended: "), tags$span(comma(total_funded_tons), " tCO2e (", coverage_pct, "% of gap), $", comma(total_funded_cost))
    )
  }

  region_pm_tons_plot <- function(alloc) {
    df <- alloc %>% filter(funded_tons > 0)
    req(nrow(df) > 0)
    ggplot(df, aes(x = reorder(project_type, funded_tons), y = funded_tons, fill = key)) +
      geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
      scale_fill_manual(values = pme_colors, guide = "none") +
      labs(subtitle = "Tons recommended, by project", x = NULL, y = "tCO2e") +
      theme_minimal(base_size = 11) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  }

  region_pm_spend_plot <- function(alloc) {
    df <- alloc %>% filter(funded_cost > 0)
    req(nrow(df) > 0)
    ggplot(df, aes(x = reorder(project_type, funded_cost), y = funded_cost, fill = key)) +
      geom_col() + coord_flip() + scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = pme_colors, guide = "none") +
      labs(subtitle = "Spend recommended, by project", x = NULL, y = "$") +
      theme_minimal(base_size = 11) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  }

  region_pm_trend_plot <- function(country_name) {
    df <- synthetic_country_year %>% filter(country == country_name) %>% arrange(year)
    req(nrow(df) > 0)
    ggplot(df, aes(x = year, y = emissions_mt)) +
      geom_line(color = "#D68910", linewidth = 1.1) + geom_point(color = "#D68910", size = 1.8) +
      scale_x_continuous(breaks = scales::pretty_breaks()) + scale_y_continuous(labels = comma) +
      labs(subtitle = paste0(country_name, " -- synthetic (illustrative) emissions, Mt CO2e per year"), x = NULL, y = "Mt CO2e") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  }

  region_pm_map <- function(alloc, region, zoomed) {
    fc <- attr(alloc, "facility_country")
    m <- if (zoomed) build_facility_map_region_zoom(alloc, fc, region) else build_facility_map_world(alloc, fc, NA_character_)
    if (is.null(m)) {
      ggplot() + annotate("text", x = 0, y = 0, label = "Map unavailable (rnaturalearth/sf not installed).", size = 4) + theme_void()
    } else m
  }

  region_pm_table <- function(alloc) {
    df <- alloc %>% filter(funded_tons > 0) %>%
      transmute(Project = project_type, Methodology = methodology_name, Country = country,
                Proximity = proximity_tier, `Tons Funded` = comma(funded_tons), Cost = paste0("$", comma(funded_cost)))
    req(nrow(df) > 0)
    datatable(df, options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  }

  # Auto-suggest gap as 10% of the country's latest synthetic year --
  # a starting point to override, never a sourced target.
  region_pm_auto_gap <- function(country_name, session, input_id) {
    df <- synthetic_country_year %>% filter(country == country_name)
    if (nrow(df) == 0) return(invisible(NULL))
    latest <- df %>% filter(year == max(year)) %>% pull(emissions_mt)
    updateNumericInput(session, input_id, value = round(latest * 0.10, 2))
  }

  # ---- LATAM ----
  observeEvent(input$latam_pm_country, { region_pm_auto_gap(input$latam_pm_country, session, "latam_pm_gap_mt") }, ignoreNULL = TRUE)
  output$latam_pm_data_badge <- renderUI({ region_pm_data_badge(input$latam_pm_country) })
  latam_pm_alloc <- reactive({
    region_run_lp_alloc(input$latam_pm_gap_mt * 1e6, input$latam_pm_budget, input$latam_pm_country,
                         input$latam_pm_proximity_weight, input$latam_pm_bucket_cap)
  })
  output$latam_pm_context    <- renderUI({ region_pm_context_ui(latam_pm_alloc()) })
  output$latam_pm_tons_plot  <- renderPlot({ region_pm_tons_plot(latam_pm_alloc()) })
  output$latam_pm_spend_plot <- renderPlot({ region_pm_spend_plot(latam_pm_alloc()) })
  output$latam_pm_trend_plot <- renderPlot({ region_pm_trend_plot(input$latam_pm_country) })
  output$latam_pm_map_zoom   <- renderPlot({ region_pm_map(latam_pm_alloc(), "Latin America", zoomed = TRUE) })
  output$latam_pm_map_world  <- renderPlot({ region_pm_map(latam_pm_alloc(), "Latin America", zoomed = FALSE) })
  output$latam_pm_table      <- renderDT({ region_pm_table(latam_pm_alloc()) })

  # ---- ASIA ----
  observeEvent(input$asia_pm_country, { region_pm_auto_gap(input$asia_pm_country, session, "asia_pm_gap_mt") }, ignoreNULL = TRUE)
  output$asia_pm_data_badge <- renderUI({ region_pm_data_badge(input$asia_pm_country) })
  asia_pm_alloc <- reactive({
    region_run_lp_alloc(input$asia_pm_gap_mt * 1e6, input$asia_pm_budget, input$asia_pm_country,
                         input$asia_pm_proximity_weight, input$asia_pm_bucket_cap)
  })
  output$asia_pm_context    <- renderUI({ region_pm_context_ui(asia_pm_alloc()) })
  output$asia_pm_tons_plot  <- renderPlot({ region_pm_tons_plot(asia_pm_alloc()) })
  output$asia_pm_spend_plot <- renderPlot({ region_pm_spend_plot(asia_pm_alloc()) })
  output$asia_pm_trend_plot <- renderPlot({ region_pm_trend_plot(input$asia_pm_country) })
  output$asia_pm_map_zoom   <- renderPlot({ region_pm_map(asia_pm_alloc(), "Asia", zoomed = TRUE) })
  output$asia_pm_map_world  <- renderPlot({ region_pm_map(asia_pm_alloc(), "Asia", zoomed = FALSE) })
  output$asia_pm_table      <- renderDT({ region_pm_table(asia_pm_alloc()) })

  # ---- AUSTRALIA ---- (single country, no selector -- constant "Australia")
  observe({ region_pm_auto_gap("Australia", session, "au_pm_gap_mt") })
  output$au_pm_data_badge <- renderUI({ region_pm_data_badge("Australia") })
  au_pm_alloc <- reactive({
    region_run_lp_alloc(input$au_pm_gap_mt * 1e6, input$au_pm_budget, "Australia",
                         input$au_pm_proximity_weight, input$au_pm_bucket_cap)
  })
  output$au_pm_context    <- renderUI({ region_pm_context_ui(au_pm_alloc()) })
  output$au_pm_tons_plot  <- renderPlot({ region_pm_tons_plot(au_pm_alloc()) })
  output$au_pm_spend_plot <- renderPlot({ region_pm_spend_plot(au_pm_alloc()) })
  output$au_pm_trend_plot <- renderPlot({ region_pm_trend_plot("Australia") })
  output$au_pm_map_zoom   <- renderPlot({ region_pm_map(au_pm_alloc(), "Australia", zoomed = TRUE) })
  output$au_pm_map_world  <- renderPlot({ region_pm_map(au_pm_alloc(), "Australia", zoomed = FALSE) })
  output$au_pm_table      <- renderDT({ region_pm_table(au_pm_alloc()) })

  # ---- AFRICA ----
  observeEvent(input$africa_pm_country, { region_pm_auto_gap(input$africa_pm_country, session, "africa_pm_gap_mt") }, ignoreNULL = TRUE)
  output$africa_pm_data_badge <- renderUI({ region_pm_data_badge(input$africa_pm_country) })
  africa_pm_alloc <- reactive({
    region_run_lp_alloc(input$africa_pm_gap_mt * 1e6, input$africa_pm_budget, input$africa_pm_country,
                         input$africa_pm_proximity_weight, input$africa_pm_bucket_cap)
  })
  output$africa_pm_context    <- renderUI({ region_pm_context_ui(africa_pm_alloc()) })
  output$africa_pm_tons_plot  <- renderPlot({ region_pm_tons_plot(africa_pm_alloc()) })
  output$africa_pm_spend_plot <- renderPlot({ region_pm_spend_plot(africa_pm_alloc()) })
  output$africa_pm_trend_plot <- renderPlot({ region_pm_trend_plot(input$africa_pm_country) })
  output$africa_pm_map_zoom   <- renderPlot({ region_pm_map(africa_pm_alloc(), "Africa", zoomed = TRUE) })
  output$africa_pm_map_world  <- renderPlot({ region_pm_map(africa_pm_alloc(), "Africa", zoomed = FALSE) })
  output$africa_pm_table      <- renderDT({ region_pm_table(africa_pm_alloc()) })

  # Shared wiring for every "All funded projects (global)" globe panel
  # -- one function instead of seven near-identical blocks, same
  # reasoning as region_run_lp_alloc() earlier: a fix here fixes all
  # seven at once, and there's only one place a bug could hide instead
  # of seven chances to diverge. Entire plotly:: usage guarded behind
  # has_plotly -- even DEFINING output[[id]] <- plotly::renderPlotly(...)
  # requires plotly:: to resolve at the point this line runs, so the
  # whole assignment sits inside the if() as a runtime code path, not
  # just an empty-result guard inside the render block. alloc_fn and
  # facility_country_fn are passed as functions (not values) so
  # they're evaluated lazily, at render time, inside the reactive.
  wire_globe_output <- function(wrapper_id, globe_id, alloc_fn, facility_country_fn) {
    output[[wrapper_id]] <- renderUI({
      if (has_plotly) {
        plotly::plotlyOutput(globe_id, height = "420px")
      } else {
        tags$p(em("Interactive globe unavailable (plotly not installed). Install with: install.packages('plotly')"))
      }
    })
    if (has_plotly) {
      output[[globe_id]] <- plotly::renderPlotly({
        g <- build_globe_preview(alloc_fn(), facility_country_fn())
        req(!is.null(g))
        g
      })
    }
  }

  wire_globe_output("eu_pm_map_world_wrapper", "eu_pm_globe", eu_pm_alloc, function() input$eu_cp_country)
  wire_globe_output("latam_pm_map_world_wrapper", "latam_pm_globe", latam_pm_alloc, function() input$latam_pm_country)
  wire_globe_output("asia_pm_map_world_wrapper", "asia_pm_globe", asia_pm_alloc, function() input$asia_pm_country)
  wire_globe_output("au_pm_map_world_wrapper", "au_pm_globe", au_pm_alloc, function() "Australia")
  wire_globe_output("africa_pm_map_world_wrapper", "africa_pm_globe", africa_pm_alloc, function() input$africa_pm_country)
  wire_globe_output("pme_map_world_wrapper", "pme_globe", pce_alloc, function() input$intake_facility_country)
  wire_globe_output("pme_5yr_map_world_wrapper", "pme_5yr_globe", pme_5yr_alloc, function() input$intake_facility_country)

  # ---------------- FACILITY VIEW ----------------

  fac_id <- reactive({
    req(input$facility_select)
    input$facility_select
  })

  fac_hist <- reactive({
    ghgp_panel_filtered %>%
      filter(facility_id == fac_id()) %>%
      group_by(year) %>%
      summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
  })

  fac_fore <- reactive({
    future_pred %>% filter(facility_id == fac_id())
  })

  fac_targ <- reactive({
    target_pred %>% filter(facility_id == fac_id())
  })

  fac_sector <- reactive({
    facility_lookup %>% filter(facility_id == fac_id()) %>% pull(primary_sector) %>% first()
  })

  fac_name <- reactive({
    facility_lookup %>% filter(facility_id == fac_id()) %>% pull(facility_name) %>% first()
  })

  output$target_meta_facility <- renderUI({
    req(fac_id())
    tl <- target_lookup %>% filter(primary_sector == fac_sector())
    if (nrow(tl) == 0) {
      tags$p(em("No published target pathway for this facility's sector."))
    } else {
      tagList(
        tags$b("Sector: "), tags$span(fac_sector()), tags$br(),
        tags$b("Target: "),
        tags$span(
          scales::percent(tl$reduction_fraction[1], accuracy = 0.1),
          " reduction by ", tl$target_year[1],
          " (baseline ", tl$baseline_year[1], ")"
        ),
        tags$br(),
        tags$b("Anchored at: "),
        tags$span(last_hist_year, " actual facility emissions"),
        if (!is.na(tl$caveat[1]) && nzchar(tl$caveat[1])) {
          tagList(tags$br(), tags$em(tl$caveat[1]))
        }
      )
    }
  })

  # Sanity-check counter: confirms the quality gate actually cascaded into
  # what this dropdown shows. Cross-reference against the pipeline's own
  # reported numbers (settings$quality_gate_n_kept / _n_tested) -- these
  # two should always match exactly, since facility_choices IS built from
  # the same already-gated facility_lookup.
  output$facility_view_count <- renderUI({
    n_available <- length(facility_choices)

    tagList(
      tags$div(
        style = "font-size:12.5px; color:#5D6D7E; margin: 4px 0 8px;",
        tags$b(comma(n_available)), " facilities available",
        if (!is.null(qgate_n_tested)) {
          tagList(
            " (of ", comma(qgate_n_tested), " tested -- ", qgate_threshold, "% error threshold)"
          )
        }
      )
    )
  })

  # Compact stat-card row for a single facility: latest emissions, trend
  # since its earliest observed year, sector, and the final-year gap.
  output$facility_stat_cards <- renderUI({
    req(fac_id())
    hist_df <- fac_hist()
    req(nrow(hist_df) > 0)

    first_year <- min(hist_df$year, na.rm = TRUE)
    e_latest <- hist_df %>% filter(year == last_hist_year) %>% pull(emissions)
    e_latest <- if (length(e_latest) > 0) e_latest[1] else NA
    e_first  <- hist_df %>% filter(year == first_year) %>% pull(emissions)
    e_first  <- if (length(e_first) > 0) e_first[1] else NA

    pct_change <- if (!is.na(e_latest) && !is.na(e_first) && e_first != 0) {
      round((e_latest - e_first) / e_first * 100, 1)
    } else {
      NA
    }

    gap_final <- tryCatch({
      df <- fac_gap()
      row <- df %>% filter(year == max(year))
      if (nrow(row) > 0) row$gap_kt[1] else NA
    }, error = function(e) NA)

    stat_card <- function(value, label) {
      tags$div(
        style = "flex:1; background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center;",
        tags$div(style = "font-size:22px; font-weight:700; color:#2C3E50;", value),
        tags$div(style = "font-size:11.5px; color:#7F8C8D;", label)
      )
    }

    tags$div(
      style = "display:flex; gap:12px; margin-bottom:16px;",
      stat_card(if (!is.na(e_latest)) paste0(comma(round(e_latest)), " t") else "--", paste0(last_hist_year, " emissions")),
      stat_card(
        if (!is.na(pct_change)) paste0(ifelse(pct_change > 0, "+", ""), pct_change, "%") else "--",
        paste0("Change since ", first_year)
      ),
      stat_card(fac_sector(), "Sector"),
      stat_card(if (!is.na(gap_final)) paste0(comma(round(gap_final)), " kt") else "--", paste0(last_fore_year, " gap"))
    )
  })

  output$facility_plot <- renderPlot({
    req(fac_id())
    sec <- fac_sector()
    col <- sector_colors[[sec]]
    if (is.null(col) || is.na(col)) col <- "#34495E"

    p <- ggplot() +
      geom_line(
        data = fac_hist(),
        aes(x = year, y = emissions / 1e3),
        color = col, linewidth = 1.1
      ) +
      geom_point(
        data = fac_hist(),
        aes(x = year, y = emissions / 1e3),
        color = col, size = 1.6
      )

    if (isTRUE(input$show_forecast_fac) && nrow(fac_fore()) > 0) {
      bridge_fore <- bind_rows(
        fac_hist() %>% filter(year == last_hist_year) %>%
          transmute(year, p50 = emissions),
        fac_fore() %>% select(year, p50)
      )
      p <- p +
        geom_line(
          data = bridge_fore,
          aes(x = year, y = p50 / 1e3),
          color = col, linewidth = 1.1, linetype = "dashed"
        )
    }

    if (isTRUE(input$show_target_fac) && nrow(fac_targ()) > 0) {
      bridge_targ <- bind_rows(
        fac_hist() %>% filter(year == last_hist_year) %>%
          transmute(year, target = emissions),
        fac_targ() %>% select(year, target)
      )
      p <- p +
        geom_line(
          data = bridge_targ,
          aes(x = year, y = target / 1e3),
          color = target_color, linewidth = 1.1, linetype = "dotted"
        )
    }

    p +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = comma) +
      labs(
        title = fac_name(),
        subtitle = paste0(
          sec, " | Solid = observed | Dashed = model forecast | Dotted = target pathway"
        ),
        x = NULL, y = "Emissions (kt CO2e)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
  })

  output$facility_table <- renderDT({
    req(fac_id())
    hist_tbl <- fac_hist() %>% transmute(year, value = emissions / 1e3, series = "Observed")
    fore_tbl <- fac_fore() %>% transmute(year, value = p50 / 1e3, series = "Forecast")
    targ_tbl <- fac_targ() %>% transmute(year, value = target / 1e3, series = "Target")

    bind_rows(hist_tbl, fore_tbl, targ_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      mutate(across(where(is.numeric) & !matches("year"), ~round(.x, 2))) %>%
      datatable(
        options = list(pageLength = 15, dom = "tp"),
        rownames = FALSE,
        colnames = c("Year", "Observed (kt)", "Forecast (kt)", "Target (kt)")
      )
  })

  # Gap = forecast minus target, in years where both exist
  fac_gap <- reactive({
    req(fac_id())
    fc <- fac_fore()
    tg <- fac_targ()
    req(nrow(fc) > 0, nrow(tg) > 0)
    inner_join(
      fc %>% select(year, p50),
      tg %>% select(year, target),
      by = "year"
    ) %>%
      mutate(gap_kt = (p50 - target) / 1e3)
  })

  output$facility_gap_plot <- renderPlot({
    df <- fac_gap()
    req(nrow(df) > 0)
    make_credit_bar(df, "gap_kt", "Gap (kt CO2e)", x_breaks_arg = x_breaks)
  })

  # ---------------- COMPANY VIEW ----------------

  company_id <- reactive({
    req(input$company_select)
    input$company_select
  })

  company_facilities <- reactive({
    req(company_id())
    facility_lookup %>% filter(company == company_id()) %>% arrange(desc(emissions_2023))
  })

  company_hist <- reactive({
    cf <- company_facilities()
    req(nrow(cf) > 0)
    ghgp_panel_filtered %>%
      filter(facility_id %in% cf$facility_id) %>%
      group_by(year) %>%
      summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
  })

  company_fore <- reactive({
    cf <- company_facilities()
    req(nrow(cf) > 0)
    future_pred %>%
      filter(facility_id %in% cf$facility_id) %>%
      group_by(year) %>%
      summarise(p50 = sum(p50, na.rm = TRUE), .groups = "drop")
  })

  # Sum of each facility's OWN sector-appropriate target -- a multi-sector
  # company's target line correctly reflects each facility's real sector,
  # not just the dominant one (that's only used for chart color/labeling).
  company_targ <- reactive({
    cf <- company_facilities()
    req(nrow(cf) > 0)
    target_pred %>%
      filter(facility_id %in% cf$facility_id) %>%
      group_by(year) %>%
      summarise(target = sum(target, na.rm = TRUE), .groups = "drop")
  })

  company_gap <- reactive({
    fc <- company_fore()
    tg <- company_targ()
    req(nrow(fc) > 0, nrow(tg) > 0)
    inner_join(fc %>% select(year, p50), tg %>% select(year, target), by = "year") %>%
      mutate(gap = p50 - target)
  })

  company_dominant_sector <- reactive({
    cf <- company_facilities()
    req(nrow(cf) > 0)
    cf %>% count(primary_sector, wt = emissions_2023, sort = TRUE) %>% slice(1) %>% pull(primary_sector)
  })

  output$company_meta <- renderUI({
    req(company_id())
    cf <- company_facilities()
    req(nrow(cf) > 0)

    sectors    <- cf %>% distinct(primary_sector) %>% pull(primary_sector)
    dom_sector <- company_dominant_sector()
    tl         <- target_lookup %>% filter(primary_sector == dom_sector)

    tagList(
      tags$b("Company: "), tags$span(company_id()), tags$br(),
      tags$b("Facilities: "), tags$span(nrow(cf)), tags$br(),
      tags$b("Sectors: "), tags$span(paste(sectors, collapse = ", ")),
      if (length(sectors) > 1) {
        tagList(
          tags$br(), tags$em(
            "Multi-sector rollup -- each facility's gap uses its own sector's target; ",
            "the summary below (", dom_sector, ") is just the largest one by emissions."
          )
        )
      },
      tags$br(),
      if (nrow(tl) > 0) {
        tagList(
          tags$b("Target (", dom_sector, "): "),
          tags$span(
            scales::percent(tl$reduction_fraction[1], accuracy = 0.1),
            " reduction by ", tl$target_year[1], " (baseline ", tl$baseline_year[1], ")"
          ),
          tags$br(),
          tags$b("Confidence: "), tags$span(tl$confidence[1])
        )
      } else {
        tags$p(em("No published target pathway for the dominant sector."))
      }
    )
  })

  output$company_plot <- renderPlot({
    req(company_id())
    cf <- company_facilities()
    req(nrow(cf) > 0)

    dom_sector <- company_dominant_sector()
    col <- sector_colors[[dom_sector]]
    if (is.null(col) || is.na(col)) col <- "#34495E"

    hist_df <- company_hist()
    fore_df <- company_fore()
    targ_df <- company_targ()

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = col, linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = col, size = 1.6)

    if (isTRUE(input$show_forecast_company) && nrow(fore_df) > 0) {
      bridge_fore <- bind_rows(
        hist_df %>% filter(year == last_hist_year) %>% transmute(year, p50 = emissions),
        fore_df
      )
      p <- p +
        geom_line(data = bridge_fore, aes(x = year, y = p50), color = col, linewidth = 1.1, linetype = "dashed")
    }

    if (isTRUE(input$show_target_company) && nrow(targ_df) > 0) {
      bridge_targ <- bind_rows(
        hist_df %>% filter(year == last_hist_year) %>% transmute(year, target = emissions),
        targ_df
      )
      p <- p +
        geom_line(data = bridge_targ, aes(x = year, y = target), color = target_color, linewidth = 1.1, linetype = "dotted")
    }

    p +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(labels = comma) +
      labs(
        title = company_id(),
        subtitle = paste0(nrow(cf), " facilities | Solid = observed | Dashed = model forecast | Dotted = target pathway"),
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
  })

  output$company_gap_plot <- renderPlot({
    df <- company_gap()
    req(nrow(df) > 0)
    make_credit_bar(df, "gap", "Gap (tCO2e)", x_breaks_arg = x_breaks)
  })

  output$company_table <- renderDT({
    hist_tbl <- company_hist() %>% transmute(year, value = emissions, series = "Observed")
    fore_tbl <- company_fore() %>% transmute(year, value = p50, series = "Forecast")
    targ_tbl <- company_targ() %>% transmute(year, value = target, series = "Target")

    bind_rows(hist_tbl, fore_tbl, targ_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      mutate(across(where(is.numeric) & !matches("year"), ~round(.x))) %>%
      datatable(
        options = list(pageLength = 15, dom = "tp"),
        rownames = FALSE,
        colnames = c("Year", "Observed (t)", "Forecast (t)", "Target (t)")
      )
  })

  # The facility roster: stat cards + a sorted, sector-colored leaderboard
  # with proportional bars, showing at a glance which facilities dominate
  # this company's footprint.
  output$company_facility_box <- renderUI({
    req(company_id())
    cf <- company_facilities()
    req(nrow(cf) > 0)

    max_e     <- max(cf$emissions_2023, na.rm = TRUE)
    total_e   <- sum(cf$emissions_2023, na.rm = TRUE)
    n_sectors <- n_distinct(cf$primary_sector)

    rows <- lapply(seq_len(nrow(cf)), function(i) {
      row <- cf[i, ]
      pct_of_max   <- if (max_e > 0) row$emissions_2023 / max_e * 100 else 0
      pct_of_total <- if (total_e > 0) round(row$emissions_2023 / total_e * 100, 1) else 0
      col <- sector_colors[[row$primary_sector]]
      if (is.null(col) || is.na(col)) col <- "#7F8C8D"

      tags$div(
        style = "margin-bottom: 14px;",
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:baseline; margin-bottom:4px; gap:8px;",
          tags$div(
            tags$span(style = "font-weight:600; font-size:13.5px;", row$facility_name),
            tags$span(
              style = paste0(
                "background:", col, "22; color:", col, "; padding:2px 9px; border-radius:10px; ",
                "font-size:10.5px; margin-left:8px; font-weight:600;"
              ),
              row$primary_sector
            )
          ),
          tags$span(
            style = "font-size:12.5px; color:#5D6D7E; white-space:nowrap;",
            comma(round(row$emissions_2023)), " t (", pct_of_total, "%)"
          )
        ),
        tags$div(
          style = "background:#ECF0F1; border-radius:5px; height:9px; overflow:hidden;",
          tags$div(style = paste0(
            "background:", col, "; width:", round(pct_of_max, 1), "%; height:100%; border-radius:5px;"
          ))
        )
      )
    })

    tagList(
      tags$div(
        style = "display:flex; gap:12px; margin-bottom:16px;",
        tags$div(
          style = "flex:1; background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center;",
          tags$div(style = "font-size:22px; font-weight:700; color:#2C3E50;", nrow(cf)),
          tags$div(style = "font-size:11.5px; color:#7F8C8D;", "Facilities")
        ),
        tags$div(
          style = "flex:1; background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center;",
          tags$div(style = "font-size:22px; font-weight:700; color:#2C3E50;", comma(round(total_e / 1e3)), " kt"),
          tags$div(style = "font-size:11.5px; color:#7F8C8D;", "2023 emissions (sum)")
        ),
        tags$div(
          style = "flex:1; background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center;",
          tags$div(style = "font-size:22px; font-weight:700; color:#2C3E50;", n_sectors),
          tags$div(style = "font-size:11.5px; color:#7F8C8D;", if (n_sectors == 1) "Sector" else "Sectors")
        )
      ),
      tags$div(
        style = "background:#FFFFFF; border:1px solid #E5E7E9; border-radius:10px; padding:1.25rem 1.5rem; max-height:420px; overflow-y:auto;",
        rows
      )
    )
  })

  # ---------------- BACKTESTING ----------------

  output$backtest_overview <- renderUI({
    if (!has_backtest_data) {
      return(tags$p(em(
        "Rerun the data pipeline to populate backtest results (older shiny_data/ export)."
      )))
    }

    # MEDIAN, not mean: a handful of facilities with near-zero emissions in
    # a given holdout year can produce percentage errors in the thousands
    # for that single row, and a mean lets those outliers dominate the
    # whole metric. Median absolute percentage error (MdAPE) is the
    # standard, robust alternative for exactly this failure mode.
    overall_mdape <- median(backtest_by_facility$error_pct, na.rm = TRUE)
    overall_mean_for_ref <- mean(backtest_by_facility$error_pct, na.rm = TRUE)
    n_fac        <- n_distinct(backtest_by_facility$facility_id)
    n_sectors    <- n_distinct(backtest_by_facility$primary_sector)

    tagList(
      tags$div(
        style = "background:#F4F6F7; border-radius:8px; padding:0.75rem 1rem; text-align:center; margin-bottom:0.75rem;",
        tags$div(style = "font-size:26px; font-weight:700; color:#2C3E50;", paste0(round(overall_mdape, 1), "%")),
        tags$div(style = "font-size:11.5px; color:#7F8C8D;", "Overall MdAPE (2021-2023 holdout)")
      ),
      tags$b("Facilities tested: "), tags$span(comma(n_fac)), tags$br(),
      tags$b("Sectors covered: "), tags$span(n_sectors), tags$br(),
      tags$em(
        "MdAPE = median absolute percentage error. Lower is better -- e.g. 8% means half of the ",
        "backtest model's predictions were off by 8% or less, on years it never saw. Median is used ",
        "instead of the more common mean because a handful of facilities with near-zero emissions in ",
        "a given year can otherwise produce enormous outlier errors that distort the whole metric ",
        "(the plain mean here would show ", round(overall_mean_for_ref, 1), "%, dominated by exactly that)."
      )
    )
  })

  # The ACTUAL gate applied globally by the pipeline -- distinct from the
  # diagnostic sliders below, which only preview alternative thresholds on
  # this tab's own chart/table without changing what the rest of the app shows.
  output$backtest_pipeline_gate_status <- renderUI({
    if (is.null(qgate_threshold)) {
      return(tags$p(em(
        "Rerun the data pipeline to populate quality-gate metadata (older shiny_data/ export -- ",
        "this app is currently showing ALL facilities, ungated)."
      )))
    }

    tagList(
      tags$div(
        style = "background:#FEF9E7; border:1px solid #F1C40F; border-radius:6px; padding:0.6rem 0.9rem;",
        tags$b("Actual gate applied everywhere else in this app:"), tags$br(),
        tags$span("Threshold: error <= ", qgate_threshold, "%"), tags$br(),
        tags$span("Facilities kept: ", comma(qgate_n_kept), " of ", comma(qgate_n_tested), " tested"),
        if (isTRUE(qgate_floor_forced)) {
          tagList(tags$br(), tags$strong(style = "color:#C0392B;",
            "Floor was applied -- fewer facilities passed the threshold than the minimum (",
            comma(qgate_min_facilities), "); the best-performing were kept regardless."
          ))
        }
      )
    )
  })

  # Frontier plot: every 2023 facility's SIGNED error (not absolute --
  # positive = over-predicted, negative = under-predicted), with the
  # +/-20% band drawn explicitly. Uses the fixed pipeline threshold
  # (qgate_threshold) for the pass/fail coloring, since that's the rule
  # that actually determines what's shown elsewhere in the app -- not
  # the diagnostic slider below, which is a separate what-if exploration.
  output$backtest_frontier_plot <- renderPlot({
    req(has_backtest_data)
    last_test_year <- max(backtest_by_facility$year)
    threshold <- if (!is.null(qgate_threshold)) qgate_threshold else 20

    df <- backtest_by_facility %>%
      filter(year == last_test_year, emissions > 0) %>%
      mutate(
        signed_error_pct = (predicted - emissions) / emissions * 100,
        status = ifelse(abs(signed_error_pct) <= threshold, "Pass", "Fail")
      )

    # View clip, NOT a data filter: a handful of facilities with near-zero
    # 2023 emissions can produce signed errors in the millions of percent
    # (same denominator-blowup issue as the earlier MAPE fix), which would
    # crush the entire +/-20% frontier into an invisible line at the
    # bottom of the chart. coord_cartesian() zooms the visible window only
    # -- pass/fail counts above and all point positions are still computed
    # on the FULL, unclipped data; extreme points just render at the edge
    # of the view instead of off-screen.
    y_clip <- 150
    n_offscreen <- sum(abs(df$signed_error_pct) > y_clip, na.rm = TRUE)

    ggplot(df, aes(x = emissions, y = signed_error_pct, color = status)) +
      geom_point(alpha = 0.35, size = 1.1) +
      geom_hline(yintercept = c(-threshold, threshold), linetype = "dashed", color = "#2C3E50", linewidth = 0.7) +
      geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
      scale_color_manual(values = c("Pass" = "#27AE60", "Fail" = "#C0392B"), name = NULL) +
      scale_x_log10(labels = comma) +
      coord_cartesian(ylim = c(-y_clip, y_clip)) +
      labs(
        subtitle = paste0(
          "Each point = one facility, 2023 | Dashed lines = +/-", threshold, "% frontier | ",
          comma(sum(df$status == "Pass")), " pass, ", comma(sum(df$status == "Fail")), " fail",
          if (n_offscreen > 0) paste0(" | ", comma(n_offscreen), " extreme points off-chart (view clipped to +/-", y_clip, "%, all still counted above)") else ""
        ),
        x = "Actual emissions (tCO2e, log scale)", y = "Signed error (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
  })

  # Same MdAPE-by-sector metric as before, but restricted to ONLY the last
  # holdout year (2023) instead of pooling all 3 -- a tighter, more
  # directly-relevant test since 2023 is the year immediately preceding
  # the actual forecast horizon (2024+).
  # MdAPE by sector, computed ONLY on the facilities that passed the
  # quality gate (backtest_gate_data(), defined below -- same threshold +
  # floor logic, so this stays consistent with the pass/fail section and
  # with what the pipeline actually kept everywhere else in the app).
  # This will read lower than an unrestricted population almost by
  # construction (that's the whole point of the gate) -- it answers "how
  # accurate is the retained cohort", not "how accurate is everyone".
  backtest_lastyear_data <- reactive({
    req(has_backtest_data)
    kept <- backtest_gate_data()

    kept %>%
      group_by(primary_sector) %>%
      summarise(
        mdape        = round(median(error_pct, na.rm = TRUE), 1),
        n_facilities = n_distinct(facility_id),
        .groups      = "drop"
      ) %>%
      mutate(
        confidence = case_when(
          mdape < 10 ~ "High",
          mdape < 20 ~ "Medium",
          TRUE       ~ "Low"
        )
      ) %>%
      arrange(mdape)
  })

  output$backtest_lastyear_plot <- renderPlot({
    df <- backtest_lastyear_data()
    req(nrow(df) > 0)
    df <- df %>% mutate(primary_sector = factor(primary_sector, levels = primary_sector))

    conf_colors <- c("High" = "#27AE60", "Medium" = "#F39C12", "Low" = "#C0392B")

    ggplot(df, aes(x = primary_sector, y = mdape, fill = confidence)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = paste0(mdape, "%")), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = conf_colors, name = "Confidence") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
      labs(
        subtitle = paste0(
          "Median absolute percentage error by sector, 2023, ",
          comma(sum(df$n_facilities)), " gated facilities only -- lower = more accurate"
        ),
        x = NULL, y = "MdAPE (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.subtitle = element_text(color = "grey40", size = 11),
        legend.position = "top"
      )
  })

  output$backtest_lastyear_table <- renderDT({
    req(has_backtest_data)
    datatable(
      backtest_lastyear_data(),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "t"),
      colnames = c("Sector", "MdAPE (%)", "Facilities Kept", "Confidence")
    )
  })

  # ---- Validation error ACROSS the full test window, per sector (per
  # Rajat's Aug 6 request: "validation error per sector for the 5-year
  # window") ----
  # HONEST LIMITATION, stated once here: the model was only trained on
  # 2011-2020 and tested against REAL held-out data through 2023 -- a
  # genuine 5-year-out test window doesn't exist yet in this data (that
  # would need the model tested against 2024-2028, years which haven't
  # happened). This shows the full window that DOES have real held-out
  # data (2021-2023), applying the SAME quality gate as everywhere else
  # in this app, so error can be seen trending across the actual tested
  # years per sector -- not fabricated out to a fifth year.
  backtest_all_years_data <- reactive({
    req(has_backtest_data)
    threshold <- if (!is.null(qgate_threshold)) qgate_threshold else 20
    min_n     <- if (!is.null(qgate_min_facilities)) qgate_min_facilities else 2000

    backtest_by_facility %>%
      group_by(year) %>%
      group_modify(~ {
        df <- .x %>% arrange(error_pct)
        passing <- df %>% filter(error_pct <= threshold)
        if (nrow(passing) < min_n) df %>% slice_head(n = min(min_n, nrow(df))) else passing
      }) %>%
      ungroup() %>%
      group_by(year, primary_sector) %>%
      summarise(mdape = round(median(error_pct, na.rm = TRUE), 1), n_facilities = n_distinct(facility_id), .groups = "drop")
  })

  output$backtest_multiyear_plot <- renderPlot({
    df <- backtest_all_years_data()
    req(nrow(df) > 0)

    ggplot(df, aes(x = factor(year), y = mdape, color = primary_sector, group = primary_sector)) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_point(size = 2.2) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        subtitle = "MdAPE by sector, across every year with real held-out test data (2021-2023) -- same quality gate as the chart above",
        x = NULL, y = "MdAPE (%)", color = "Sector"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "right", legend.text = element_text(size = 9))
  })

  # Facility quality gate: keep facilities with 2023 error <= threshold.
  # If that passes fewer than the minimum, relax and keep the best-
  # performing facilities up to the minimum instead -- attr()s record
  # whether that relaxation happened, so the summary can say so plainly
  # rather than silently moving the goalposts.
  backtest_gate_data <- reactive({
    req(has_backtest_data)
    last_test_year <- max(backtest_by_facility$year)
    df <- backtest_by_facility %>% filter(year == last_test_year) %>% arrange(error_pct)

    # Uses the REAL pipeline-computed gate values (settings$quality_gate_*),
    # not adjustable inputs -- the diagnostic sliders and their sidebar
    # were removed, so this chart now reflects the actual gate applied
    # everywhere else in the app, not an exploratory alternative.
    threshold <- if (!is.null(qgate_threshold)) qgate_threshold else 20
    min_n     <- if (!is.null(qgate_min_facilities)) qgate_min_facilities else 2000

    passing <- df %>% filter(error_pct <= threshold)

    if (nrow(passing) < min_n) {
      kept         <- df %>% slice_head(n = min(min_n, nrow(df)))
      floor_forced <- TRUE
    } else {
      kept         <- passing
      floor_forced <- FALSE
    }

    kept <- kept %>% mutate(passed_threshold = error_pct <= threshold)

    attr(kept, "floor_forced")   <- floor_forced
    attr(kept, "n_passing_raw")  <- nrow(passing)
    attr(kept, "n_total")        <- nrow(df)
    kept
  })

  backtest_gate_composition <- reactive({
    kept <- backtest_gate_data()
    last_test_year <- max(backtest_by_facility$year)

    total_by_sector <- backtest_by_facility %>%
      filter(year == last_test_year) %>%
      count(primary_sector, name = "total_facilities")

    kept %>%
      count(primary_sector, name = "kept") %>%
      left_join(total_by_sector, by = "primary_sector") %>%
      mutate(pct_of_sector = round(kept / total_facilities * 100, 1)) %>%
      arrange(desc(pct_of_sector))
  })

  output$backtest_gate_summary <- renderUI({
    kept <- backtest_gate_data()
    req(nrow(kept) > 0)

    floor_forced  <- attr(kept, "floor_forced")
    n_passing_raw <- attr(kept, "n_passing_raw")
    n_total       <- attr(kept, "n_total")

    tagList(
      tags$b("Threshold: "), tags$span("error <= ", input$backtest_max_error, "%"), tags$br(),
      tags$b("Facilities passing threshold: "), tags$span(comma(n_passing_raw), " of ", comma(n_total)), tags$br(),
      tags$b("Facilities kept: "), tags$span(comma(nrow(kept))), tags$br(),
      tags$b("Median error, kept group: "), tags$span(paste0(round(median(kept$error_pct, na.rm = TRUE), 1), "%")),
      if (floor_forced) {
        tagList(
          tags$br(), tags$strong(style = "color:#C0392B;", "FLOOR APPLIED: "),
          tags$em(
            "only ", comma(n_passing_raw), " facilities actually passed the ", input$backtest_max_error,
            "% threshold, below your minimum of ", comma(input$backtest_min_facilities), ". The gate ",
            "relaxed automatically and kept the best ", comma(nrow(kept)), " facilities regardless of ",
            "whether they truly passed -- some of the facilities counted above did NOT meet your threshold."
          )
        )
      } else {
        tagList(tags$br(), tags$em("All kept facilities genuinely passed the threshold -- no relaxation was needed."))
      }
    )
  })

  output$backtest_gate_composition_table <- renderDT({
    datatable(
      backtest_gate_composition(),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "t"),
      colnames = c("Sector", "Kept", "Total Facilities (Sector)", "% of Sector Kept")
    )
  })

  output$backtest_gate_facility_table <- renderDT({
    kept <- backtest_gate_data() %>%
      transmute(
        facility_name, primary_sector,
        error_pct = round(error_pct, 1),
        status = ifelse(passed_threshold, "Pass", "Kept via floor (did not pass)")
      ) %>%
      arrange(error_pct)

    datatable(
      kept,
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tp"),
      colnames = c("Facility", "Sector", "Error (%)", "Status")
    )
  })

  output$backtest_sector_plot <- renderPlot({
    req(has_backtest_data)
    df <- backtest_by_sector %>%
      arrange(mdape) %>%
      mutate(primary_sector = factor(primary_sector, levels = primary_sector))

    conf_colors <- c("High" = "#27AE60", "Medium" = "#F39C12", "Low" = "#C0392B")

    ggplot(df, aes(x = primary_sector, y = mdape, fill = confidence)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = paste0(mdape, "%")), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = conf_colors, name = "Confidence") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
      labs(
        subtitle = "Median absolute percentage error by sector, lower = more accurate (holdout: 2021-2023)",
        x = NULL, y = "MdAPE (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.subtitle = element_text(color = "grey40", size = 11),
        legend.position = "top"
      )
  })

  output$backtest_sector_table <- renderDT({
    req(has_backtest_data)
    datatable(
      backtest_by_sector %>% arrange(mdape),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "t"),
      colnames = c("Sector", "MdAPE (%)", "Facilities Tested", "Confidence")
    )
  })

  # ---------------- LOCATION BREAKDOWN (Facility & Company View tab) ----------------

  output$location_top_facilities_plot <- renderPlot({
    req(nrow(facility_lookup) > 0)

    df <- facility_lookup %>% arrange(desc(emissions_2023)) %>% slice_head(n = 20)
    df <- df %>%
      mutate(label = if (has_city_data) paste0(facility_name, " (", city, ", ", state, ")") else facility_name) %>%
      mutate(label = factor(label, levels = rev(label)))

    ggplot(df, aes(x = label, y = emissions_2023 / 1e3, fill = primary_sector)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        subtitle = "Top 20 facilities by 2023 emissions (gated facilities only)",
        x = NULL, y = "Emissions (kt CO2e)", fill = "Sector"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "right")
  })

  output$location_top_cities_plot <- renderPlot({
    req(nrow(facility_lookup) > 0)
    if (!has_city_data) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "Rerun the data pipeline to populate city-level data (older shiny_data/ export).") +
          theme_void()
      )
    }

    df <- facility_lookup %>%
      filter(!is.na(city), nzchar(city)) %>%
      group_by(city, state) %>%
      summarise(total_emissions = sum(emissions_2023, na.rm = TRUE), n_facilities = n(), .groups = "drop") %>%
      arrange(desc(total_emissions)) %>%
      slice_head(n = 15) %>%
      mutate(label = paste0(city, ", ", state))
    req(nrow(df) > 0)
    df <- df %>% mutate(label = factor(label, levels = rev(label)))

    ggplot(df, aes(x = label, y = total_emissions / 1e3)) +
      geom_col(fill = "#34495E") +
      geom_text(aes(label = paste0(n_facilities, " facilities")), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
      labs(
        subtitle = "Top 15 cities by total 2023 emissions, summed across facilities (gated only)",
        x = NULL, y = "Emissions (kt CO2e)"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
  })

  # ---------------- GOALS TAB ----------------
  # Simplified to a single, always-visible reference table: every sector
  # SBTi publishes a US target for (Part 4C of the pipeline), no facility
  # or company matching. The earlier per-facility comparison (direct/
  # sector-estimate SBTi match, overlay plot, gap plot) was removed --
  # fac_id()/fac_hist()/fac_targ()/fac_sector()/fac_name()/fac_gap() are
  # still defined above and used elsewhere (Facility & Company View,
  # Portfolio Mix Engine), just no longer referenced from this tab.

  # Two plain sector list tables, side by side -- no attempt to match
  # them up. SBTi (green): every sector with a published US target, plus
  # how many companies/targets sit behind it. GHGRP (blue): every sector
  # actually present in our facility data, plus its facility count.
  output$sbti_sector_list_table <- renderDT({
    if (!has_sbti_sector_targets || is.null(sbti_sector_targets) || nrow(sbti_sector_targets) == 0) {
      return(datatable(
        data.frame(Message = "No SBTi reference data found -- run Part 4C of the pipeline script."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- sbti_sector_targets %>%
      arrange(sector) %>%
      select(sector, n_companies, n_targets)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "asc")), dom = "ftip"),
      colnames = c("SBTi Sector", "Companies", "Targets")
    )
  })

  # Same content as sbti_sector_list_table, duplicated under its own output
  # ID rather than reusing the same ID twice in the UI (a single output ID
  # bound to two DTOutput() calls is not reliably supported by Shiny/DT).
  output$sbti_sector_list_table_top <- renderDT({
    if (!has_sbti_sector_targets || is.null(sbti_sector_targets) || nrow(sbti_sector_targets) == 0) {
      return(datatable(
        data.frame(Message = "No SBTi reference data found -- run Part 4C of the pipeline script."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- sbti_sector_targets %>%
      arrange(sector) %>%
      select(sector, n_companies, n_targets)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "asc")), dom = "ftip"),
      colnames = c("SBTi Sector", "Companies", "Targets")
    )
  })

  output$ghgrp_sector_counts_table <- renderDT({
    if (is.null(ghgrp_sector_counts) || nrow(ghgrp_sector_counts) == 0) {
      return(datatable(
        data.frame(Message = "Rerun the pipeline (Part 4C.55) to populate this table."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    datatable(
      ghgrp_sector_counts %>% arrange(desc(n_ghgrp_facilities)),
      rownames = FALSE,
      options = list(pageLength = 15, dom = "ftip"),
      colnames = c("Sector", "Facilities (gated population)")
    )
  })

  output$ghgrp_actual_goals_table <- renderDT({
    if (is.null(ghgrp_actual_goals) || nrow(ghgrp_actual_goals) == 0) {
      return(datatable(
        data.frame(Message = "Rerun the pipeline (Part 4C.55) to populate this table."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- ghgrp_actual_goals %>%
      mutate(
        goal_reduction_display = ifelse(!is.na(goal_reduction_pct), scales::percent(goal_reduction_pct, accuracy = 0.1), "--"),
        source_link_html = ifelse(
          !is.na(goal_source_link),
          paste0('<a href="', goal_source_link, '" target="_blank" rel="noopener noreferrer">Source document</a>'),
          "--"
        )
      ) %>%
      select(primary_sector, n_ghgrp_facilities, goal_source, goal_source_detail,
             source_link_html, goal_reduction_display, goal_target_year)

    datatable(
      df, rownames = FALSE,
      escape = -5,   # column 5 = source_link_html -- the only column allowed to render raw HTML (the <a> link)
      options = list(pageLength = 15, dom = "ftip", scrollX = TRUE),
      colnames = c("Sector", "GHGRP Facilities", "Goal Source", "Source Detail",
                   "Link", "Reduction Goal", "Target Year")
    )
  })

  output$ghgrp_non_sbti_goals_table <- renderDT({
    if (is.null(ghgrp_non_sbti_goals) || nrow(ghgrp_non_sbti_goals) == 0) {
      return(datatable(
        data.frame(Message = "Every sector matched an SBTi target, or rerun the pipeline (Part 4C.55) to populate this table."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- ghgrp_non_sbti_goals %>%
      mutate(
        goal_reduction_display = ifelse(!is.na(goal_reduction_pct), scales::percent(goal_reduction_pct, accuracy = 0.1), "--"),
        source_link_html = ifelse(
          !is.na(goal_source_link),
          paste0('<a href="', goal_source_link, '" target="_blank" rel="noopener noreferrer">Source document</a>'),
          "--"
        )
      ) %>%
      select(primary_sector, n_ghgrp_facilities, goal_source, goal_source_detail,
             source_link_html, goal_reduction_display, goal_target_year)

    datatable(
      df, rownames = FALSE,
      escape = -5,   # column 5 = source_link_html
      options = list(pageLength = 15, dom = "ftip", scrollX = TRUE),
      colnames = c("Sector", "Facilities", "Goal Source", "Source Detail", "Link", "Reduction Goal", "Target Year")
    )
  })

  output$ghgrp_sector_list_table <- renderDT({
    if (!has_ghgrp_industry_reference || is.null(ghgrp_industry_reference) || nrow(ghgrp_industry_reference) == 0) {
      return(datatable(
        data.frame(Message = "No GHGRP industry reference found -- run Part 4C.45 of the pipeline script."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- ghgrp_industry_reference %>%
      arrange(subpart_letter) %>%
      select(subpart_letter, ghgrp_industry, facility_type, n_ghgrp_facilities)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "asc")), dom = "ftip"),
      colnames = c("Subpart", "GHGRP Sector", "Facility Type", "Facilities (our data)")
    )
  })

  # Hierarchical view: our primary_sector categories, each broken into the
  # official GHGRP subparts commonly reported under it, marked present
  # (checkmark) or absent (X) in our actual facility data. Purely additive
  # -- ghgrp_sector_list_table (below) is no longer referenced in the UI,
  # kept as dead code rather than deleted in case the flat table is wanted back.
  output$ghgrp_subpart_presence_status <- renderUI({
    if (has_ghgrp_subpart_presence && !is.null(ghgrp_subpart_presence)) {
      tags$p(
        style = "font-size:11.5px; color:#1B5E20; margin-bottom:0.5rem;",
        em("Live: computed from the actual subparts data, checked against the modeled facility population.")
      )
    } else {
      tags$p(
        style = "font-size:11.5px; color:#B7950B; margin-bottom:0.5rem;",
        em("Reference only: rerun the pipeline (Part 2.5) to compute this live from your actual data instead.")
      )
    }
  })

  output$ghgrp_sector_tree <- renderUI({
    df <- ghgrp_sector_subpart_map

    # Prefer the LIVE, pipeline-computed presence check (real data,
    # checked against the modeled facility population) over the
    # hardcoded reference values, when it's available.
    if (has_ghgrp_subpart_presence && !is.null(ghgrp_subpart_presence)) {
      df <- df %>%
        select(-present) %>%
        left_join(ghgrp_subpart_presence, by = "subpart_letter") %>%
        mutate(present = coalesce(present, FALSE))
    }

    sectors <- unique(ghgrp_sector_subpart_map$main_sector)  # preserve the order given

    sector_blocks <- lapply(sectors, function(sec) {
      rows <- df %>% filter(main_sector == sec)
      n_present <- sum(rows$present)
      n_total   <- nrow(rows)

      items <- lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        icon <- if (r$present) "\u2705" else "\u274C"
        note <- if (!r$present) {
          override <- tryCatch(r$note_override, error = function(e) NA_character_)
          if (is.null(override) || length(override) == 0 || is.na(override)) {
            " - NOT IN DATA"
          } else {
            paste0(" - ", override)
          }
        } else {
          ""
        }
        tags$div(
          style = "padding: 2px 0 2px 1.4rem; font-size: 13px;",
          paste0(icon, " ", r$subpart_letter, " (", r$short_label, ")", note)
        )
      })

      tags$div(
        style = "margin-bottom: 16px;",
        tags$div(
          style = "font-weight:700; font-size:14px; color:#2471A3;",
          toupper(sec),
          tags$span(
            style = "font-weight:400; color:#7F8C8D; font-size:12px;",
            paste0("  (", n_present, "/", n_total, " subparts present in our data)")
          )
        ),
        items
      )
    })

    tags$div(
      style = "background:#FFFFFF; border:1px solid #D5D8DC; border-radius:8px; padding:1rem 1.25rem; max-height:600px; overflow-y:auto;",
      sector_blocks
    )
  })

  # Big-picture bar chart (Part 4D of the pipeline): SBTi's 44 sectors
  # rolled up into 11 broad categories, colored by whether that category
  # has a real GHGRP-style heavy-industry match or not.
  output$sbti_big_sector_plot <- renderPlot({
    if (!has_sbti_big_sector_summary || is.null(sbti_big_sector_summary) || nrow(sbti_big_sector_summary) == 0) {
      return(
        ggplot() + theme_void() +
          labs(title = "No SBTi big-sector summary found -- run Part 4D of the pipeline script.")
      )
    }
    df <- sbti_big_sector_summary %>%
      mutate(big_sector = factor(big_sector, levels = rev(big_sector[order(n_companies)])))

    ggplot(df, aes(x = big_sector, y = n_companies, fill = ghgrp_relevant)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = comma(n_companies)), hjust = -0.15, size = 3.6) +
      coord_flip() +
      scale_fill_manual(
        values = c(`TRUE` = "#1B5E20", `FALSE` = "#BDC3C7"),
        labels = c(`TRUE` = "Matches GHGRP-style heavy industry", `FALSE` = "Rest of corporate economy"),
        name = NULL
      ) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(
        subtitle = "Companies with a validated US SBTi target, by big-picture sector category",
        x = NULL, y = "Companies"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", plot.subtitle = element_text(color = "grey40", size = 11))
  })

  output$sbti_sector_reference_table <- renderDT({
    if (!has_sbti_sector_targets || is.null(sbti_sector_targets) || nrow(sbti_sector_targets) == 0) {
      return(datatable(
        data.frame(Message = "No SBTi reference data found -- run Part 4C of the pipeline script (needs sbti_targets.xlsx)."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    df <- sbti_sector_targets %>%
      arrange(sector) %>%
      mutate(
        avg_reduction_pct    = scales::percent(avg_reduction_pct, accuracy = 0.1),
        median_reduction_pct = scales::percent(median_reduction_pct, accuracy = 0.1)
      )
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "asc"))),
      colnames = c(
        "Sector", "SBTi Companies", "GHGRP Facilities (our data)", "SBTi Targets",
        "Avg Reduction %", "Median Reduction %",
        "Avg Base Year", "Avg Target Year", "Earliest Target Year", "Latest Target Year"
      )
    )
  })

  # ---------------- SBTi CALCULATOR ----------------

  # Auto-route SDA/ACA by sector -- per the meeting decision to merge the
  # two methods into one flow instead of asking the user to pick. Real
  # SBTi 1.5C pathway data (sbti_sda_engine()) only exists in this app for
  # Power and Cement, so the routing is narrow and string-matched against
  # the sector name rather than a hardcoded list (robust to exact
  # sector_list wording without needing to know its full contents).
  # Deliberately still a normal radioButtons the user can override --
  # "auto-route" means a smart default, not a locked choice; a genuinely
  # unusual company (e.g. a diversified conglomerate) may know better
  # than a sector-name string match.
  observeEvent(input$intake_sector, {
    req(input$intake_sector)
    sector <- input$intake_sector
    if (grepl("cement", sector, ignore.case = TRUE)) {
      updateRadioButtons(session, "sbti_calc_method", selected = "Sectoral Decarbonization Approach")
      updateSelectInput(session, "sbti_calc_sda_sector", selected = "Cement")
    } else if (grepl("power", sector, ignore.case = TRUE)) {
      updateRadioButtons(session, "sbti_calc_method", selected = "Sectoral Decarbonization Approach")
      updateSelectInput(session, "sbti_calc_sda_sector", selected = "Power")
    } else {
      updateRadioButtons(session, "sbti_calc_method", selected = "Absolute Contraction Approach")
    }
  }, ignoreInit = TRUE)

  sbti_calc_result <- reactive({
    req(input$sbti_calc_base_year, input$sbti_calc_s1, input$sbti_calc_target_year)

    s2_val     <- if (isTRUE(input$sbti_calc_s2 > 0))     input$sbti_calc_s2     else NA_real_
    mry_s2_val <- if (isTRUE(input$sbti_calc_mry_s2 > 0)) input$sbti_calc_mry_s2 else NA_real_
    s3_val     <- if (isTRUE(input$sbti_calc_s3 > 0))     input$sbti_calc_s3     else NA_real_
    mry_s3_val <- if (isTRUE(input$sbti_calc_mry_s3 > 0)) input$sbti_calc_mry_s3 else NA_real_

    tryCatch(
      sbti_calculate(
        company_name              = input$sbti_calc_company,
        target_setting_method     = input$sbti_calc_method,
        sda_sector                = if (input$sbti_calc_method == "Sectoral Decarbonization Approach") input$sbti_calc_sda_sector else NA,
        base_year                 = input$sbti_calc_base_year,
        base_year_activity_output = if (input$sbti_calc_method == "Sectoral Decarbonization Approach") input$sbti_calc_activity else NA,
        base_year_s1_e            = input$sbti_calc_s1,
        base_year_s2_e            = s2_val,
        target_year               = input$sbti_calc_target_year,
        activity_projection_type  = "Fixed market share",
        most_recent_year          = input$sbti_calc_mry_year,
        mry_s1_e                  = input$sbti_calc_mry_s1,
        mry_s2_e                  = mry_s2_val,
        net_zero_year             = input$sbti_calc_net_zero_year,
        base_year_s3_e            = s3_val,
        mry_s3_e                  = mry_s3_val,
        most_recent_year_s3       = input$sbti_calc_mry_year_s3,
        base_year_s3              = input$sbti_calc_base_year_s3,
        target_year_s3            = input$sbti_calc_target_year_s3,
        s3_method                 = input$sbti_calc_s3_method,
        s3_ambition               = input$sbti_calc_s3_ambition,
        s3_base_year_output       = if (isTRUE(input$sbti_calc_s3_method != "Cross-sector ACA")) input$sbti_calc_s3_output else NA
      ),
      error = function(e) list(error = conditionMessage(e))
    )
  })

  # Combined observed+forecast benchmark series -- the grey line's own
  # data -- used below as the "base emissions" input for the Industry
  # Target line, in place of the company's own emissions.
  intake_benchmark_full_series <- reactive({
    req(input$intake_sector)
    bm <- intake_sector_benchmark() %>% transmute(year, value = avg_emissions)
    fc <- intake_sector_forecast_benchmark() %>% transmute(year, value = avg_p50)
    bind_rows(bm, fc) %>% arrange(year) %>% distinct(year, .keep_all = TRUE)
  })

  # Industry Target -- runs the SAME SBTi engine and the SAME
  # method/base-year/target-year/net-zero-year settings as the "SBTi
  # Calculator" tab, but on the sector's MEDIAN FACILITY (the grey line)
  # instead of the company's own numbers -- a genuine SBTi-methodology
  # industry target, not a flat %-reduction proxy. This REPLACES the
  # earlier target_lookup-based version (intake_industry_pathway()),
  # which turned out to be mathematically IDENTICAL to "Your stated goal"
  # whenever "use sector's published target" was toggled on -- both were
  # just borrowing the same target_lookup row, so they could never
  # actually disagree. Only ACA-based tracks are supported here (Scope 1
  # ACA, Scope 2 always-ACA per sbti_calculate()'s own limitation, Scope 3
  # Cross-sector ACA) -- Sectoral Decarbonization Approach and the Scope 3
  # intensity methods would need industry-wide activity-output data this
  # app doesn't have, so those return an explicit error/note rather than
  # a silently fabricated number.
  intake_industry_sbti_result <- reactive({
    req(input$intake_sector, input$sbti_calc_base_year, input$sbti_calc_target_year)
    if (!identical(input$sbti_calc_method, "Absolute Contraction Approach")) {
      return(list(error = paste0(
        "Industry Target needs the Absolute Contraction Approach method -- Sectoral ",
        "Decarbonization Approach would need industry-wide activity-output data this app doesn't have."
      )))
    }

    bm_full <- intake_benchmark_full_series()
    ratio   <- get_scope23_ratio(input$intake_sector)

    get_bm <- function(yr) {
      if (is.null(yr) || is.na(yr)) return(NA_real_)
      row <- bm_full %>% filter(year == yr)
      if (nrow(row) == 0) return(NA_real_)
      row$value[1]
    }

    base_e <- get_bm(input$sbti_calc_base_year)
    if (is.na(base_e)) {
      return(list(error = paste0("No sector benchmark data at base year ", input$sbti_calc_base_year, ".")))
    }
    mry_e <- get_bm(input$sbti_calc_mry_year)

    a1 <- tryCatch(
      sbti_aca_engine(input$sbti_calc_base_year, base_e, input$sbti_calc_mry_year, mry_e,
                       input$sbti_calc_target_year, nz_ambition = 0.90,
                       nz_year = input$sbti_calc_net_zero_year, min_larr = 0.042),
      error = function(e) NULL
    )
    if (is.null(a1)) return(list(error = "Could not compute the Scope 1 industry SBTi track."))

    path <- data.frame(year = as.numeric(names(a1$trajectory)), scope1_emissions = as.numeric(a1$trajectory))

    if (!is.null(ratio)) {
      # Scope 2 -- always ACA (same limitation sbti_calculate() has for
      # the company's own Scope 2 track); base value ratio-scaled from
      # the Scope 1 benchmark, since GHGRP has no real industry-wide
      # Scope 2 data to anchor to directly.
      base_e2 <- base_e * ratio$scope2_multiplier
      mry_e2  <- if (!is.na(mry_e)) mry_e * ratio$scope2_multiplier else NA_real_
      a2 <- tryCatch(
        sbti_aca_engine(input$sbti_calc_base_year, base_e2, input$sbti_calc_mry_year, mry_e2,
                         input$sbti_calc_target_year, nz_ambition = 1.00,
                         nz_year = min(input$sbti_calc_net_zero_year, 2040), min_larr = 0.042),
        error = function(e) NULL
      )
      if (!is.null(a2)) {
        path <- path %>% full_join(
          data.frame(year = as.numeric(names(a2$trajectory)), scope2_emissions = as.numeric(a2$trajectory)),
          by = "year"
        )
      }

      # Scope 3 -- only the Cross-sector ACA track (same reasoning as SDA
      # above: the intensity methods need industry-wide activity data
      # this app doesn't have).
      if (isTRUE(input$sbti_calc_s3_method == "Cross-sector ACA") &&
          !is.na(input$sbti_calc_base_year_s3) && !is.na(input$sbti_calc_target_year_s3)) {
        base_e3_bm <- get_bm(input$sbti_calc_base_year_s3)
        if (!is.na(base_e3_bm)) {
          base_e3 <- base_e3_bm * ratio$scope3_multiplier
          mry_e3_bm <- get_bm(input$sbti_calc_mry_year_s3)
          mry_e3 <- if (!is.na(mry_e3_bm)) mry_e3_bm * ratio$scope3_multiplier else NA_real_
          nz_amb3 <- if (identical(input$sbti_calc_s3_ambition, "1.5C")) 0.90 else 0.75
          larr3   <- if (identical(input$sbti_calc_s3_ambition, "1.5C")) 0.042 else 0.025
          a3 <- tryCatch(
            sbti_aca_engine(input$sbti_calc_base_year_s3, base_e3, input$sbti_calc_mry_year_s3, mry_e3,
                             input$sbti_calc_target_year_s3, nz_ambition = nz_amb3,
                             nz_year = min(input$sbti_calc_net_zero_year, 2040), min_larr = larr3),
            error = function(e) NULL
          )
          if (!is.null(a3)) {
            path <- path %>% full_join(
              data.frame(year = as.numeric(names(a3$trajectory)), scope3_emissions = as.numeric(a3$trajectory)),
              by = "year"
            )
          }
        }
      }
    }

    list(path = path %>% arrange(year))
  })

  output$sbti_calc_error <- renderUI({
    result <- sbti_calc_result()
    req(!is.null(result$error))
    tags$div(
      style = "background:#FDEDEC; border:1px solid #E74C3C; border-radius:6px; padding:0.6rem 0.9rem; margin-bottom:1rem;",
      tags$b("Calculation error: "), tags$span(result$error)
    )
  })

  output$sbti_calc_scope3_note <- renderUI({
    result <- sbti_calc_result()
    req(is.null(result$error), !is.null(result$scope3_note))
    tags$div(
      style = "background:#EBF5FB; border:1px solid #AED6F1; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:0.75rem; font-size:12.5px;",
      tags$b("Scope 3 method: "), result$scope3_note
    )
  })

  output$sbti_calc_plot <- renderPlot({
    result <- sbti_calc_result()
    req(is.null(result$error))
    df <- result$path
    req(nrow(df) > 0)

    has_scope2 <- "scope2_emissions" %in% names(df)

    plot_df <- df %>%
      select(year, scope1_emissions, any_of(c("scope2_emissions", "scope3_emissions")), total_emissions) %>%
      pivot_longer(-year, names_to = "series", values_to = "value")

    series_labels <- c(
      scope1_emissions = "Scope 1", scope2_emissions = "Scope 2", scope3_emissions = "Scope 3", total_emissions = "Total"
    )
    series_colors <- c(
      scope1_emissions = "#2980B9", scope2_emissions = "#8E44AD", scope3_emissions = "#16A085", total_emissions = "#C0392B"
    )

    ggplot(plot_df, aes(x = year, y = value, color = series)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 1.8) +
      scale_color_manual(values = series_colors, labels = series_labels, name = NULL) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = comma) +
      labs(
        title = if (nzchar(result$company_name)) result$company_name else "SBTi Target Pathway",
        subtitle = paste0(
          result$method,
          if (result$method == "Sectoral Decarbonization Approach") paste0(" (", result$sector, ")") else ""
        ),
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "bottom")
  })

  output$sbti_calc_table <- renderDT({
    result <- sbti_calc_result()
    if (!is.null(result$error)) {
      return(datatable(data.frame(Error = result$error), rownames = FALSE, options = list(dom = "t")))
    }
    df <- result$path %>% mutate(across(-year, ~round(.x)))

    colnames_display <- c("Year")
    if ("scope1_emissions" %in% names(df)) colnames_display <- c(colnames_display, "Scope 1 (tCO2e)")
    if ("scope2_emissions" %in% names(df)) colnames_display <- c(colnames_display, "Scope 2 (tCO2e)")
    if ("scope3_emissions" %in% names(df)) colnames_display <- c(colnames_display, "Scope 3 (tCO2e)")
    colnames_display <- c(colnames_display, "Total (tCO2e)")

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 20, dom = "tp"),
      colnames = colnames_display
    )
  })

  # ---------------- NEW COMPANY INTAKE ----------------

  # Parse the pasted "year,emissions" lines into a tidy data.frame
  # Shared parser -- same logic for Scope 1/2/3 historical data textareas.
  parse_scope_csv <- function(text) {
    if (is.null(text) || !nzchar(trimws(text))) return(NULL)
    lines <- strsplit(trimws(text), "\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) return(NULL)

    parsed <- lapply(lines, function(l) {
      parts <- strsplit(trimws(l), ",")[[1]]
      if (length(parts) != 2) return(NULL)
      yr  <- suppressWarnings(as.numeric(trimws(parts[1])))
      val <- suppressWarnings(as.numeric(trimws(parts[2])))
      if (is.na(yr) || is.na(val)) return(NULL)
      data.frame(year = yr, emissions = val)
    })
    parsed <- parsed[!sapply(parsed, is.null)]
    if (length(parsed) == 0) return(NULL)
    bind_rows(parsed) %>% arrange(year) %>% distinct(year, .keep_all = TRUE)
  }

  # ---- Real-company auto-match (per Avishkar's mandate: typing an
  # existing entity like "Exxon" or "Chevron" must auto-populate its real
  # historical data instead of requiring manual re-entry) ----
  # Same extract_company() heuristic company_lookup itself is built from
  # -- matches the first uppercase word of what's typed against the same
  # first-word key every real GHGRP company was grouped by. Returns NULL
  # (no match) for a genuinely new company, in which case the manual
  # entry path below still works exactly as before.
  # Two-tier match, since real company names don't reliably start with
  # the same first word extract_company() groups on -- "Boeing" typed by
  # a user won't match a real facility named "The Boeing Company --
  # Everett" via the first-word key alone (that facility groups under
  # "THE", not "BOEING"). Tier 1 (fast, common case): exact first-word
  # key match. Tier 2 (fallback): a direct substring search against real
  # facility names themselves, which catches this and similar naming
  # mismatches. Returns NULL for a genuine new company either way.
  intake_matched_facilities <- reactive({
    nm <- trimws(input$intake_company_name)
    req(nzchar(nm))
    key <- extract_company(nm)

    fac <- facility_lookup %>% filter(company == key)

    if (nrow(fac) == 0) {
      nm_upper <- toupper(nm)
      fac <- facility_lookup %>% filter(grepl(nm_upper, toupper(facility_name), fixed = TRUE))
    }

    if (nrow(fac) == 0) return(NULL)
    fac %>% arrange(desc(emissions_2023))
  })

  intake_company_match <- reactive({
    fac <- intake_matched_facilities()
    if (is.null(fac)) return(NULL)
    data.frame(
      company = extract_company(fac$facility_name[1]),
      n_facilities = nrow(fac),
      total_emissions_2023 = sum(fac$emissions_2023, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  # Auto-populate on match: switches "Do you have historical data?" to
  # Yes (the real data replaces the need to paste anything), and routes
  # the sector picker to this company's own dominant sector -- both
  # previously separate manual steps.
  observeEvent(input$intake_company_name, {
    m <- tryCatch(intake_company_match(), error = function(e) NULL)
    if (!is.null(m)) {
      updateRadioButtons(session, "intake_has_data", selected = "yes")
      fac <- intake_matched_facilities()
      if (nrow(fac) > 0) {
        dom_sector <- fac %>% count(primary_sector, wt = emissions_2023, sort = TRUE) %>% slice(1) %>% pull(primary_sector)
        if (length(dom_sector) > 0 && dom_sector %in% sector_list) {
          updateSelectInput(session, "intake_sector", selected = dom_sector)
        }
        # A matched company is always a real GHGRP reporter -- always
        # US-based, regardless of which specific state its facilities
        # sit in.
        updateSelectInput(session, "intake_facility_country", selected = "United States")
        fac_state <- fac$state[1]
        if (!is.null(fac_state) && !is.na(fac_state) && fac_state %in% us_state_choices) {
          updateSelectInput(session, "intake_facility_state", selected = fac_state)
        }
      }
    }
  }, ignoreInit = TRUE)

  output$intake_match_status <- renderUI({
    m <- tryCatch(intake_company_match(), error = function(e) NULL)
    company_typed <- if (is.null(input$intake_company_name)) "" else trimws(input$intake_company_name)
    if (is.null(m)) {
      if (nzchar(company_typed)) {
        tags$div(
          style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:6px; padding:0.5rem 0.8rem; margin-bottom:10px; font-size:12px;",
          em("No match in the real GHGRP database -- treated as a new company. Enter data manually below.")
        )
      } else {
        NULL
      }
    } else {
      fac <- intake_matched_facilities()
      tags$div(
        style = "background:#EAFAF1; border:1px solid #A9DFBF; border-radius:6px; padding:0.5rem 0.8rem; margin-bottom:10px; font-size:12px;",
        tags$b("Matched: "), m$company[1], " -- real GHGRP data found for ", nrow(fac), " facilit",
        if (nrow(fac) == 1) "y" else "ies", ". Historical Scope 1 emissions auto-populated below ",
        "(see the \"Facilities\" tab for the per-facility breakdown)."
      )
    }
  })

  # ---- Facilities tab: company rollup with individual facilities nested
  # beneath it, per Avishkar's mandate -- a drill-down INSIDE the same
  # matched company's view, not a peer selection alongside it. Only
  # populated for a real matched company (Section above); a new/unmatched
  # company has no facility-level detail to show, since it isn't in the
  # GHGRP database at all.
  output$intake_facilities_content <- renderUI({
    m <- tryCatch(intake_company_match(), error = function(e) NULL)
    if (is.null(m)) {
      return(tags$p(em(
        "Facility-level detail is only available for companies already reporting to GHGRP. ",
        "Once the company name above matches a real entity, its individual facilities will ",
        "appear here, nested beneath the company-level summary."
      )))
    }
    fac <- intake_matched_facilities()
    tagList(
      h5(paste0(m$company[1], " -- company rollup (", nrow(fac), if (nrow(fac) == 1) " facility)" else " facilities)")),
      plotOutput("intake_company_rollup_plot", height = "360px"),
      hr(),
      h5("Individual facility detail"),
      selectInput(
        "intake_facility_drill", NULL,
        choices = setNames(fac$facility_id, fac$facility_name)
      ),
      plotOutput("intake_facility_drill_plot", height = "360px"),
      br(),
      DTOutput("intake_facility_drill_table")
    )
  })

  output$intake_company_rollup_plot <- renderPlot({
    m <- tryCatch(intake_company_match(), error = function(e) NULL)
    req(!is.null(m))
    fac <- intake_matched_facilities()
    hist_df <- ghgp_panel_filtered %>% filter(facility_id %in% fac$facility_id) %>%
      group_by(year) %>% summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
    fore_df <- future_pred %>% filter(facility_id %in% fac$facility_id) %>%
      group_by(year) %>% summarise(p50 = sum(p50, na.rm = TRUE), .groups = "drop")

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = "#34495E", linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = "#34495E", size = 1.6)

    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(
        hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions),
        fore_df
      )
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = "#34495E", linewidth = 1.1, linetype = "dashed")
    }

    p + scale_x_continuous(breaks = x_breaks) + scale_y_continuous(labels = comma) +
      labs(subtitle = paste0(nrow(fac), " facilities summed | Solid = observed | Dashed = model forecast"),
           x = NULL, y = "Emissions (tCO2e)") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$intake_facility_drill_plot <- renderPlot({
    req(input$intake_facility_drill)
    fid <- input$intake_facility_drill
    fname <- facility_lookup$facility_name[facility_lookup$facility_id == fid][1]
    sec   <- facility_lookup$primary_sector[facility_lookup$facility_id == fid][1]
    col   <- sector_colors[[sec]]
    if (is.null(col) || is.na(col)) col <- "#34495E"

    hist_df <- ghgp_panel_filtered %>% filter(facility_id == fid) %>%
      group_by(year) %>% summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
    fore_df <- future_pred %>% filter(facility_id == fid) %>% select(year, p50)
    targ_df <- target_pred %>% filter(facility_id == fid) %>% select(year, target)

    p <- ggplot() +
      geom_line(data = hist_df, aes(x = year, y = emissions), color = col, linewidth = 1.1) +
      geom_point(data = hist_df, aes(x = year, y = emissions), color = col, size = 1.6)

    if (nrow(fore_df) > 0) {
      bridge <- bind_rows(hist_df %>% filter(year == max(year)) %>% transmute(year, p50 = emissions), fore_df)
      p <- p + geom_line(data = bridge, aes(x = year, y = p50), color = col, linewidth = 1.1, linetype = "dashed")
    }
    if (nrow(targ_df) > 0) {
      bridge_t <- bind_rows(hist_df %>% filter(year == max(year)) %>% transmute(year, target = emissions), targ_df)
      p <- p + geom_line(data = bridge_t, aes(x = year, y = target), color = target_color, linewidth = 1.1, linetype = "dotted")
    }

    p + scale_x_continuous(breaks = x_breaks) + scale_y_continuous(labels = comma) +
      labs(title = fname, subtitle = paste0(sec, " | Solid = observed | Dashed = forecast | Dotted = target"),
           x = NULL, y = "Emissions (tCO2e)") +
      theme_minimal(base_size = 13) + theme(plot.subtitle = element_text(color = "grey40", size = 10.5))
  })

  output$intake_facility_drill_table <- renderDT({
    req(input$intake_facility_drill)
    fid <- input$intake_facility_drill
    hist_tbl <- ghgp_panel_filtered %>% filter(facility_id == fid) %>%
      group_by(year) %>% summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
      transmute(year, value = round(emissions), series = "Observed")
    fore_tbl <- future_pred %>% filter(facility_id == fid) %>% transmute(year, value = round(p50), series = "Forecast")
    targ_tbl <- target_pred %>% filter(facility_id == fid) %>% transmute(year, value = round(target), series = "Target")

    bind_rows(hist_tbl, fore_tbl, targ_tbl) %>%
      pivot_wider(names_from = series, values_from = value) %>%
      arrange(year) %>%
      datatable(options = list(pageLength = 15, dom = "tp"), rownames = FALSE,
                colnames = c("Year", "Observed (t)", "Forecast (t)", "Target (t)"))
  })

  # ---- Excel template download/upload for Scope 1/2/3 emissions ----
  # Generated on the fly (writexl), not shipped as a separate file
  # alongside the app -- one less file to keep in sync at deploy time.
  # Upload parses the same three columns and feeds the EXISTING
  # intake_emissions_csv / _s2 / _s3 text areas via
  # updateTextAreaInput() -- intake_user_data() and friends below never
  # need to know whether the text came from typing or an upload; they
  # read the same inputs either way.
  output$intake_template_download <- downloadHandler(
    filename = function() "emissions_data_template.xlsx",
    content = function(file) {
      req(has_writexl)
      template_df <- data.frame(
        Year = c(2019:2023, rep(NA_integer_, 15)),
        `Scope 1 (tCO2e)` = c(132000, 128500, 125000, 118000, 110500, rep(NA_real_, 15)),
        `Scope 2 (tCO2e)` = c(19500, 19000, 18000, 17200, 16100, rep(NA_real_, 15)),
        `Scope 3 (tCO2e)` = c(141000, 137500, 135000, 128000, 119000, rep(NA_real_, 15)),
        check.names = FALSE
      )
      writexl::write_xlsx(template_df, file)
    }
  )

  intake_template_parsed <- reactiveVal(NULL)

  observeEvent(input$intake_template_upload, {
    req(has_readxl)
    file_path <- input$intake_template_upload$datapath

    result <- tryCatch({
      df <- readxl::read_excel(file_path, sheet = 1)
      names(df) <- trimws(names(df))
      req_cols <- c("Year", "Scope 1 (tCO2e)")
      missing_cols <- setdiff(req_cols, names(df))
      if (length(missing_cols) > 0) {
        list(error = paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "),
                             ". Use the downloaded template's exact headers."))
      } else {
        df <- df[!is.na(df$Year), ]
        to_csv_lines <- function(col) {
          if (!col %in% names(df)) return("")
          sub_df <- df[!is.na(df[[col]]), c("Year", col)]
          if (nrow(sub_df) == 0) return("")
          paste0(sub_df$Year, ",", sub_df[[col]], collapse = "\n")
        }
        list(
          s1 = to_csv_lines("Scope 1 (tCO2e)"),
          s2 = to_csv_lines("Scope 2 (tCO2e)"),
          s3 = to_csv_lines("Scope 3 (tCO2e)"),
          n_years = nrow(df)
        )
      }
    }, error = function(e) list(error = paste0("Could not read this file: ", conditionMessage(e))))

    intake_template_parsed(result)

    if (is.null(result$error)) {
      if (nzchar(result$s1)) updateTextAreaInput(session, "intake_emissions_csv", value = result$s1)
      if (nzchar(result$s2)) updateTextAreaInput(session, "intake_emissions_csv_s2", value = result$s2)
      if (nzchar(result$s3)) updateTextAreaInput(session, "intake_emissions_csv_s3", value = result$s3)
    }
  })

  output$intake_template_upload_status <- renderUI({
    res <- intake_template_parsed()
    req(!is.null(res))
    if (!is.null(res$error)) {
      tags$div(style = "color:#C0392B; font-size:12px; margin-top:6px;", res$error)
    } else {
      tags$div(style = "color:#1E8449; font-size:12px; margin-top:6px;",
                "Loaded ", res$n_years, " year(s) of data.")
    }
  })

  intake_user_data <- reactive({
    m <- tryCatch(intake_company_match(), error = function(e) NULL)
    if (!is.null(m)) {
      fac <- intake_matched_facilities()
      return(
        ghgp_panel_filtered %>%
          filter(facility_id %in% fac$facility_id) %>%
          group_by(year) %>%
          summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>%
          arrange(year)
      )
    }
    req(input$intake_has_data == "yes", input$intake_emissions_csv)
    parse_scope_csv(input$intake_emissions_csv)
  })

  # Real Scope 2/3 historical data, if provided -- entirely optional.
  # NULL means "no real data for this scope", in which case callers fall
  # back to the Hertwich & Wood ratio estimate; never silently treated
  # as zero.
  intake_user_data_s2 <- reactive({
    req(input$intake_has_data_s23 == "yes")
    parse_scope_csv(input$intake_emissions_csv_s2)
  })

  intake_user_data_s3 <- reactive({
    req(input$intake_has_data_s23 == "yes")
    parse_scope_csv(input$intake_emissions_csv_s3)
  })

  # ---- SBTi field auto-derivation (consolidation fix) ----
  # The SBTi Calculator used to require typing Company Name / Base Year /
  # Base Year Scope 1&2 / MRY / Target Year A SECOND TIME, even though
  # every one of those already exists as real data on New Company Intake
  # (the pasted year,emissions CSVs, and the target year slider). This
  # reactive computes what those SBTi fields SHOULD be, derived entirely
  # from NCI's own inputs -- Base Year = earliest pasted year, MRY = most
  # recent pasted year (standard SBTi convention: a company's baseline
  # reference point vs. its latest actual data), Target Year = shared
  # directly with NCI's own target year control. Falls back to sane
  # defaults (0 emissions, current-year placeholders) only where NCI
  # genuinely has no data yet, matching the SBTi Calculator's own
  # original "0 = excluded" convention for Scope 2/3.
  nci_derived_sbti <- reactive({
    has_data <- isTRUE(input$intake_has_data == "yes")
    ud <- if (has_data) intake_user_data() else NULL

    if (!is.null(ud) && nrow(ud) > 0) {
      base_year <- min(ud$year); base_s1 <- ud$emissions[ud$year == base_year][1]
      mry_year  <- max(ud$year); mry_s1  <- ud$emissions[ud$year == mry_year][1]
    } else {
      base_year <- 2015; base_s1 <- 100000
      mry_year  <- 2015; mry_s1  <- 100000
    }

    target_year <- if (has_data) input$intake_target_year else input$intake_target_year_nodata
    if (is.null(target_year) || is.na(target_year)) target_year <- 2030

    s2_data <- tryCatch(intake_user_data_s2(), error = function(e) NULL)
    if (!is.null(s2_data) && nrow(s2_data) > 0) {
      base_s2 <- s2_data$emissions[s2_data$year == min(s2_data$year)][1]
      mry_s2  <- s2_data$emissions[s2_data$year == max(s2_data$year)][1]
    } else {
      base_s2 <- 0; mry_s2 <- 0
    }

    s3_data <- tryCatch(intake_user_data_s3(), error = function(e) NULL)
    if (!is.null(s3_data) && nrow(s3_data) > 0) {
      base_year_s3 <- min(s3_data$year); base_s3 <- s3_data$emissions[s3_data$year == base_year_s3][1]
      mry_year_s3  <- max(s3_data$year); mry_s3  <- s3_data$emissions[s3_data$year == mry_year_s3][1]
    } else {
      base_year_s3 <- base_year; base_s3 <- 0
      mry_year_s3  <- mry_year;  mry_s3  <- 0
    }

    list(
      company_name = input$intake_company_name,
      base_year = base_year, base_s1 = base_s1, mry_year = mry_year, mry_s1 = mry_s1,
      target_year = target_year, base_s2 = base_s2, mry_s2 = mry_s2,
      base_year_s3 = base_year_s3, base_s3 = base_s3, mry_year_s3 = mry_year_s3, mry_s3 = mry_s3,
      target_year_s3 = target_year
    )
  })

  # Keeps the (now hidden) SBTi Calculator inputs in sync with the
  # derivation above -- every downstream reactive that reads
  # input$sbti_calc_* (sbti_calc_result(), intake_industry_sbti_result(),
  # the auto-routing observer) keeps working completely unchanged; only
  # WHERE these values come from has changed, from manual re-entry to a
  # live derivation off New Company Intake's own data.
  observe({
    d <- nci_derived_sbti()
    updateTextInput(session, "sbti_calc_company", value = if (is.null(d$company_name)) "" else d$company_name)
    updateNumericInput(session, "sbti_calc_base_year", value = d$base_year)
    updateNumericInput(session, "sbti_calc_s1", value = d$base_s1)
    updateNumericInput(session, "sbti_calc_s2", value = d$base_s2)
    updateNumericInput(session, "sbti_calc_target_year", value = d$target_year)
    updateNumericInput(session, "sbti_calc_mry_year", value = d$mry_year)
    updateNumericInput(session, "sbti_calc_mry_s1", value = d$mry_s1)
    updateNumericInput(session, "sbti_calc_mry_s2", value = d$mry_s2)
    updateNumericInput(session, "sbti_calc_base_year_s3", value = d$base_year_s3)
    updateNumericInput(session, "sbti_calc_target_year_s3", value = d$target_year_s3)
    updateNumericInput(session, "sbti_calc_mry_year_s3", value = d$mry_year_s3)
    updateNumericInput(session, "sbti_calc_s3", value = d$base_s3)
    updateNumericInput(session, "sbti_calc_mry_s3", value = d$mry_s3)
  })

  output$sbti_derived_readout <- renderUI({
    d <- nci_derived_sbti()
    has_data <- isTRUE(input$intake_has_data == "yes")
    s2_on <- d$base_s2 > 0 || d$mry_s2 > 0
    s3_on <- d$base_s3 > 0 || d$mry_s3 > 0
    company_display <- if (!is.null(d$company_name) && nzchar(d$company_name)) d$company_name else NULL

    tags$div(
      style = "background:#FFFFFF; border:1px solid #D5D8DC; border-radius:6px; padding:0.6rem 0.9rem; font-size:12px;",
      tags$b("Using from New Company Intake:"),
      tags$ul(
        style = "margin-bottom:0; padding-left:1.1rem;",
        tags$li("Company: ", if (!is.null(company_display)) company_display else tags$em("not set")),
        tags$li(
          if (has_data) {
            paste0("Scope 1: ", comma(round(d$base_s1)), " t (", d$base_year, ") \u2192 ",
                   comma(round(d$mry_s1)), " t (", d$mry_year, ")")
          } else {
            tagList(tags$em("No Scope 1 data entered yet -- paste it on New Company Intake."))
          }
        ),
        tags$li(if (s2_on) paste0("Scope 2: ", comma(round(d$base_s2)), " t \u2192 ", comma(round(d$mry_s2)), " t") else tags$em("Scope 2 not entered -- excluded")),
        tags$li(if (s3_on) paste0("Scope 3: ", comma(round(d$base_s3)), " t (", d$base_year_s3, ") \u2192 ", comma(round(d$mry_s3)), " t (", d$mry_year_s3, ")") else tags$em("Scope 3 not entered -- excluded")),
        tags$li("Target year: ", d$target_year)
      )
    )
  })

  # ---- Scope 3 category breakdown (15 GHG Protocol categories) ----
  # Optional real data: paste "category_id,tCO2e" pairs (one per
  # category, for whichever categories the company actually tracks).
  # Reuses the same parsing shape as parse_scope_csv (two comma-
  # separated numbers per line) but the first number is a category ID
  # (1-15), not a year -- this is a breakdown of ONE year's total, not a
  # time series.
  parse_scope3_category_csv <- function(text) {
    if (is.null(text) || !nzchar(trimws(text))) return(NULL)
    lines <- strsplit(trimws(text), "\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) return(NULL)

    parsed <- lapply(lines, function(l) {
      parts <- strsplit(trimws(l), ",")[[1]]
      if (length(parts) != 2) return(NULL)
      cat_id <- suppressWarnings(as.numeric(trimws(parts[1])))
      val    <- suppressWarnings(as.numeric(trimws(parts[2])))
      if (is.na(cat_id) || is.na(val) || cat_id < 1 || cat_id > 15) return(NULL)
      data.frame(cat_id = cat_id, emissions = val)
    })
    parsed <- parsed[!sapply(parsed, is.null)]
    if (length(parsed) == 0) return(NULL)
    bind_rows(parsed) %>% arrange(cat_id) %>% distinct(cat_id, .keep_all = TRUE)
  }

  intake_scope3_category_data <- reactive({
    parse_scope3_category_csv(input$intake_s3_category_csv)
  })

  # Combines real category-level data (wherever entered) with the
  # illustrative split for any category NOT entered -- so the chart
  # always shows all 15 categories, but only the ones with real data are
  # colored/labeled as real; the rest are clearly marked as the v1
  # estimate, never silently blended into one undifferentiated number.
  # The aggregate total being split is THIS scope's own current total
  # (last real year if entered, else the ratio-estimated forecast for
  # the first forecast year) -- always ties back to the SAME Scope 3
  # number shown at the top of this tab, never a separate figure.
  intake_scope3_breakdown <- reactive({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)

    real_s3 <- intake_scope3_category_data()
    real_cat_ids <- if (!is.null(real_s3)) real_s3$cat_id else integer(0)

    # Aggregate Scope 3 total to split across the categories NOT covered
    # by real category-level data -- last real Scope 3 year if entered,
    # else the current forecast year's estimate.
    s3_real_agg <- tryCatch(intake_user_data_s3(), error = function(e) NULL)
    if (!is.null(s3_real_agg) && nrow(s3_real_agg) > 0) {
      total_year  <- max(s3_real_agg$year)
      total_value <- s3_real_agg$emissions[s3_real_agg$year == total_year][1]
    } else {
      row <- df %>% filter(!is.na(scope3_forecast)) %>% arrange(year) %>% head(1)
      req(nrow(row) > 0)
      total_year  <- row$year[1]
      total_value <- row$scope3_forecast[1]
    }

    # Real category entries are subtracted from the aggregate BEFORE
    # splitting the remainder across the estimated categories -- so the
    # 15 categories always sum back to the same aggregate total shown at
    # the top, whether real or estimated.
    real_total <- if (length(real_cat_ids) > 0) sum(real_s3$emissions[real_s3$cat_id %in% real_cat_ids]) else 0
    remainder  <- max(total_value - real_total, 0)

    share_info <- get_scope3_category_shares(input$intake_sector)
    cats_with_shares <- scope3_categories %>%
      mutate(illustrative_share = share_info$shares[as.character(cat_id)])
    est_cats   <- cats_with_shares %>% filter(!cat_id %in% real_cat_ids)
    est_share_sum <- sum(est_cats$illustrative_share)

    result <- cats_with_shares %>%
      mutate(
        is_real = cat_id %in% real_cat_ids,
        value = ifelse(
          is_real,
          sapply(cat_id, function(id) real_s3$emissions[real_s3$cat_id == id][1]),
          ifelse(est_share_sum > 0, remainder * illustrative_share / est_share_sum, 0)
        )
      ) %>%
      arrange(desc(value))

    list(
      data = result, total_value = total_value, total_year = total_year,
      any_real = length(real_cat_ids) > 0, sourced = share_info$sourced,
      match_type = share_info$match_type, source = share_info$source
    )
  })

  # ---- FORECASTED 15-category trend (not just one year's snapshot) ----
  # Same ratio logic as intake_scope3_breakdown() above, applied to EVERY
  # year in the Scope 3 series (real observed years + forecast years),
  # not just the latest one. Real per-category data (if entered) only
  # ever applies to the SAME single year it always did -- there's no real
  # per-category data for FUTURE years, since nobody has measured a
  # forecast; every other year (past or future) uses the illustrative
  # CDP-anchored split of that year's own aggregate Scope 3 total.
  intake_scope3_category_trend <- reactive({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)

    s3_real_agg <- tryCatch(intake_user_data_s3(), error = function(e) NULL)
    real_years_df <- if (!is.null(s3_real_agg) && nrow(s3_real_agg) > 0) {
      s3_real_agg %>% transmute(year, value = emissions)
    } else {
      NULL
    }
    forecast_years_df <- df %>% filter(!is.na(scope3_forecast)) %>% transmute(year, value = scope3_forecast)
    full_series <- if (!is.null(real_years_df)) {
      bind_rows(real_years_df, forecast_years_df %>% filter(!year %in% real_years_df$year)) %>% arrange(year)
    } else {
      forecast_years_df %>% arrange(year)
    }
    req(nrow(full_series) > 0)

    real_cat <- intake_scope3_category_data()
    real_cat_ids <- if (!is.null(real_cat)) real_cat$cat_id else integer(0)
    real_cat_total <- if (length(real_cat_ids) > 0) sum(real_cat$emissions[real_cat$cat_id %in% real_cat_ids]) else 0
    latest_real_year <- if (!is.null(real_years_df)) max(real_years_df$year) else NA_integer_

    share_info <- get_scope3_category_shares(input$intake_sector)
    cats_with_shares <- scope3_categories %>%
      mutate(illustrative_share = share_info$shares[as.character(cat_id)])
    est_cats <- cats_with_shares %>% filter(!cat_id %in% real_cat_ids)
    est_share_sum <- sum(est_cats$illustrative_share)

    rows <- lapply(seq_len(nrow(full_series)), function(i) {
      yr    <- full_series$year[i]
      total <- full_series$value[i]
      has_real_this_year <- !is.na(latest_real_year) && yr == latest_real_year && length(real_cat_ids) > 0
      remainder <- if (has_real_this_year) max(total - real_cat_total, 0) else total

      cats_with_shares %>%
        transmute(
          year = yr, cat_id, cat_name,
          is_real = has_real_this_year & (cat_id %in% real_cat_ids),
          value = if (has_real_this_year) {
            ifelse(
              cat_id %in% real_cat_ids,
              sapply(cat_id, function(id) real_cat$emissions[real_cat$cat_id == id][1]),
              if (est_share_sum > 0) remainder * illustrative_share / est_share_sum else 0
            )
          } else {
            total * illustrative_share
          }
        )
    })
    bind_rows(rows)
  })

  output$intake_s3_category_trend_plot <- renderPlot({
    trend <- tryCatch(intake_scope3_category_trend(), error = function(e) NULL)
    req(!is.null(trend), nrow(trend) > 0)

    # Order categories by their OWN total across the series (largest at
    # the bottom of the stack) so the biggest drivers anchor the chart,
    # rather than an arbitrary category-number order.
    cat_order <- trend %>% group_by(cat_name) %>% summarise(total = sum(value), .groups = "drop") %>%
      arrange(total) %>% pull(cat_name)
    trend <- trend %>% mutate(cat_name = factor(cat_name, levels = cat_order), year = as.integer(year))

    ggplot(trend, aes(x = year, y = value, fill = cat_name)) +
      geom_area(position = "stack", alpha = 0.9, color = "white", linewidth = 0.15) +
      scale_fill_manual(values = scales::hue_pal()(15), name = NULL) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = comma) +
      labs(
        subtitle = "Forecasted Scope 3 by category -- stacked, sums to the same total shown at the top of this tab",
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.subtitle = element_text(color = "grey40", size = 11),
        legend.position = "right", legend.text = element_text(size = 9)
      )
  })

  output$intake_s3_category_trend_table <- renderDT({
    trend <- tryCatch(intake_scope3_category_trend(), error = function(e) NULL)
    req(!is.null(trend), nrow(trend) > 0)

    wide <- trend %>%
      mutate(value = round(value)) %>%
      select(cat_id, cat_name, year, value) %>%
      tidyr::pivot_wider(names_from = year, values_from = value)

    datatable(
      wide, rownames = FALSE,
      options = list(pageLength = 15, dom = "t", scrollX = TRUE),
      colnames = c("Category #", "GHG Protocol Category", as.character(sort(unique(trend$year))))
    )
  })

  # Plain reference table of the ratios themselves (not tied to any
  # company's actual total) -- lets you see exactly what split is being
  # applied before/regardless of entering any real category data.
  # Sector-aware: shows real numbers for Chemicals, the honest equal-
  # split default (with that fact stated in its own column) elsewhere.
  output$intake_s3_ratio_table <- renderDT({
    share_info <- get_scope3_category_shares(input$intake_sector)
    source_label <- if (share_info$match_type == "exact") {
      paste0("EXACT match: ", share_info$source)
    } else if (share_info$match_type == "approximate") {
      paste0("APPROXIMATE sector match: ", share_info$source)
    } else {
      "No CDP sector match found -- equal 1/15 split"
    }
    df <- scope3_categories %>%
      mutate(
        share_pct = paste0(round(share_info$shares[as.character(cat_id)] * 100, 1), "%"),
        source = source_label
      ) %>%
      select(cat_id, cat_name, share_pct, source)
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, dom = "t"),
      colnames = c("Category #", "GHG Protocol Category", "Share", "Basis")
    )
  })

  # ---- DIAGNOSTIC (temporary) -- shows exactly what was parsed from the
  # textbox and the intermediate values feeding the breakdown table, so a
  # mismatch between "what you typed" and "what the table shows" can be
  # pinpointed to a specific step instead of guessed at.
  output$intake_s3_debug <- renderUI({
    raw_text <- input$intake_s3_category_csv
    parsed   <- tryCatch(intake_scope3_category_data(), error = function(e) paste("ERROR:", conditionMessage(e)))
    result   <- tryCatch(intake_scope3_breakdown(), error = function(e) paste("ERROR:", conditionMessage(e)))

    parsed_txt <- if (is.data.frame(parsed)) {
      paste(capture.output(print(parsed, row.names = FALSE)), collapse = "\n")
    } else if (is.null(parsed)) {
      "NULL (nothing parsed -- either empty box, or every line failed the 'number,number' format check)"
    } else {
      as.character(parsed)
    }

    total_val_txt <- if (is.list(result) && !is.null(result$total_value)) comma(round(result$total_value)) else "N/A"

    tags$div(
      style = "background:#F4F6F7; border:1px dashed #AAB7B8; border-radius:6px; padding:0.6rem 0.9rem; margin-bottom:8px; font-size:11.5px; font-family:monospace; white-space:pre-wrap;",
      tags$b("DIAGNOSTIC (temporary):\n"),
      "Raw textbox content:\n", if (is.null(raw_text) || !nzchar(raw_text)) "(empty)" else raw_text, "\n\n",
      "Parsed by parse_scope3_category_csv():\n", parsed_txt, "\n\n",
      "Scope 3 total_value used for the split: ", total_val_txt
    )
  })

  output$intake_s3_category_plot <- renderPlot({
    result <- tryCatch(intake_scope3_breakdown(), error = function(e) NULL)
    req(!is.null(result))
    df <- result$data %>% mutate(cat_name = factor(cat_name, levels = rev(cat_name)))

    ggplot(df, aes(x = cat_name, y = value, fill = is_real)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(
        values = c(`TRUE` = "#C0392B", `FALSE` = "#D5D8DC"),
        labels = c(`TRUE` = "Your real data", `FALSE` = "Estimate (CDP-anchored split, not sector-specific)"),
        name = NULL
      ) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(
        subtitle = paste0(
          "Scope 3 by GHG Protocol category, ", result$total_year, " -- sums to the same ",
          comma(round(result$total_value)), " t shown above"
        ),
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
  })

  output$intake_s3_category_table <- renderDT({
    result <- tryCatch(intake_scope3_breakdown(), error = function(e) NULL)
    req(!is.null(result))
    estimate_label <- switch(
      result$match_type,
      "exact" = "Estimate (CDP-researched, exact sector match)",
      "approximate" = "Estimate (CDP-researched, approximate sector match)",
      "Estimate (no sector research -- equal split)"
    )
    df <- result$data %>%
      mutate(
        source = ifelse(is_real, "Your real data", estimate_label),
        share_pct = round(value / result$total_value * 100, 1)
      ) %>%
      select(cat_id, cat_name, value, share_pct, source)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, dom = "t"),
      colnames = c("Category #", "GHG Protocol Category", "tCO2e", "% of Scope 3", "Source")
    )
  })

  # Per-facility MEDIAN emissions for the sector, by year -- computed
  # directly from the granular facility-year data (ghgp_panel_filtered),
  # not derived from a sector total divided by facility count. A mean
  # (the previous approach) gets pulled way up by a handful of very large
  # facilities in a skewed sector like Chemicals; a median is far more
  # robust to that and better represents what a genuinely typical
  # facility looks like. Column names kept as avg_emissions/avg_p50 even
  # though they're now medians, to avoid renaming everywhere downstream
  # that already reads these two reactives.
  intake_sector_benchmark <- reactive({
    req(input$intake_sector)
    ghgp_panel_filtered %>%
      filter(primary_sector == input$intake_sector) %>%
      group_by(year) %>%
      summarise(avg_emissions = median(emissions, na.rm = TRUE), .groups = "drop")
  })

  # Reported median stat: the actual number driving the grey benchmark
  # line, shown directly rather than left for the user to read off the
  # chart -- plus how many facilities it's based on, for transparency
  # (a median of 3 facilities is much less robust than one of 300).
  output$intake_median_stat <- renderUI({
    req(input$intake_sector)
    bm <- intake_sector_benchmark()
    req(nrow(bm) > 0)

    n_fac <- ghgp_panel_filtered %>%
      filter(primary_sector == input$intake_sector, year == last_hist_year) %>%
      distinct(facility_id) %>%
      nrow()

    median_val <- bm$avg_emissions[bm$year == last_hist_year][1]
    req(!is.na(median_val))

    tags$div(
      style = "background:#F4F6F7; border-radius:6px; padding:0.5rem 0.75rem; margin-bottom:0.75rem; font-size:12.5px;",
      tags$b("Median ", input$intake_sector, " facility (", last_hist_year, "): "),
      tags$span(comma(round(median_val)), " tCO2e"),
      tags$br(),
      tags$span(style = "color:#7F8C8D;", "Based on ", comma(n_fac), " gated facilities in this sector.")
    )
  })

  # Sector-level forecast median, same logic -- computed directly from
  # future_pred's per-facility forecasts, not a sector total divided by count.
  intake_sector_forecast_benchmark <- reactive({
    req(input$intake_sector)
    future_pred %>%
      filter(primary_sector == input$intake_sector) %>%
      group_by(year) %>%
      summarise(avg_p50 = median(p50, na.rm = TRUE), .groups = "drop")
  })

  # For companies with their own data: no company-specific model exists yet,
  # so we scale the sector's forecast trend onto the company's own baseline
  # (same % change year over year as the sector-average forecast implies).
  intake_user_forecast <- reactive({
    req(isTRUE(input$intake_has_data == "yes"))
    ud <- intake_user_data()
    req(!is.null(ud))

    baseline_year  <- max(ud$year)
    baseline_value <- ud$emissions[ud$year == baseline_year][1]

    bm <- intake_sector_benchmark()
    bm_base <- bm$avg_emissions[bm$year == baseline_year][1]
    if (length(bm_base) == 0 || is.na(bm_base)) {
      bm_base <- bm$avg_emissions[bm$year == last_hist_year][1]
    }
    req(!is.na(bm_base), bm_base != 0)

    fc <- intake_sector_forecast_benchmark() %>% filter(year > baseline_year)
    if (nrow(fc) == 0) return(NULL)

    fc %>% transmute(year, p50 = baseline_value * (avg_p50 / bm_base))
  })

  # Baseline + target pathway, using either the user's own data or,
  # if they have none, the sector benchmark as the anchor
  intake_target_pathway <- reactive({
    req(input$intake_sector)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())

    if (has_data) {
      ud <- intake_user_data()
      baseline_year  <- max(ud$year)
      baseline_value <- ud$emissions[ud$year == baseline_year][1]
      use_sector_target <- isTRUE(input$intake_use_sector_target)
      target_year_in     <- input$intake_target_year
      target_reduction_in <- input$intake_target_reduction
    } else {
      bm <- intake_sector_benchmark()
      req(nrow(bm) > 0)
      baseline_year  <- max(bm$year)
      baseline_value <- bm$avg_emissions[bm$year == baseline_year][1]
      use_sector_target <- isTRUE(input$intake_use_sector_target_nodata)
      target_year_in     <- input$intake_target_year_nodata
      target_reduction_in <- input$intake_target_reduction_nodata
    }

    tl <- target_lookup %>% filter(primary_sector == input$intake_sector)

    if (use_sector_target && nrow(tl) > 0) {
      target_year <- tl$target_year[1]
      reduction   <- tl$reduction_fraction[1]
    } else {
      target_year <- target_year_in
      reduction   <- target_reduction_in / 100
    }
    req(!is.na(target_year), !is.na(reduction), target_year > baseline_year)

    annual_rate <- 1 - (1 - reduction)^(1 / (target_year - baseline_year))
    years <- baseline_year:target_year
    data.frame(
      year   = years,
      target = baseline_value * (1 - annual_rate)^(years - baseline_year)
    ) %>%
      mutate(annual_rate = annual_rate, baseline_year = baseline_year, has_data = has_data)
  })

  output$intake_meta <- renderUI({
    req(input$intake_sector)
    path <- intake_target_pathway()
    has_data <- path$has_data[1]

    source_label <- if (has_data) {
      "your submitted data"
    } else {
      paste0("industry benchmark (typical facility in ", input$intake_sector, ")")
    }

    tagList(
      tags$b("Baseline: "),
      tags$span(source_label, ", ", path$baseline_year[1], " = ",
                comma(round(path$target[1])), " tCO2e"),
      tags$br(),
      tags$b("Implied annual reduction rate: "),
      tags$span(scales::percent(path$annual_rate[1], accuracy = 0.1)),
      if (!has_data) {
        tagList(
          tags$br(),
          tags$em("This is an interim benchmark only, based on sector averages -- not your actual footprint.")
        )
      }
    )
  })

  # Shared builder behind intake_plot/intake_plot_s2/intake_plot_s3 --
  # scope_mult rescales every non-SBTi series (benchmark, user data,
  # stated goal) by the SAME Hertwich & Wood ratio used in the gap
  # analysis (1.0 for Scope 1, i.e. no change). The SBTi line instead
  # pulls whichever REAL scope column exists in sbti_calc_result() --
  # never ratio-derived, since a target should reflect actual reported
  # data. If that scope's SBTi data isn't configured, the SBTi line is
  # simply omitted from that chart, not synthesized.
  # Scope 1's own full year series (historical + forecast), or the sector
  # benchmark's growth shape if the company has no Scope 1 data of its own.
  # Extracted as ONE shared reactive so the trend chart and the gap chart
  # are guaranteed to borrow the exact same growth shape for Option B --
  # previously each computed this inline, which is how they drifted apart.
  intake_s1_full_series <- reactive({
    req(input$intake_sector)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())
    bm <- intake_sector_benchmark()

    s1_hist <- if (has_data) intake_user_data() %>% transmute(year, value = emissions) else NULL
    s1_fore <- if (has_data) intake_user_forecast() else NULL
    s1_fore <- if (!is.null(s1_fore) && nrow(s1_fore) > 0) s1_fore %>% transmute(year, value = p50) else NULL
    s1_full <- bind_rows(s1_hist, s1_fore)

    if (is.null(s1_full) || nrow(s1_full) == 0) {
      s1_full <- bind_rows(
        bm %>% transmute(year, value = avg_emissions),
        intake_sector_forecast_benchmark() %>% transmute(year, value = avg_p50)
      )
    }
    s1_full %>% arrange(year) %>% distinct(year, .keep_all = TRUE)
  })

  # Option B: anchor a scope's REAL data to its own last real year, then
  # extend forward using the SAME year-over-year growth shape as Scope 1's
  # own series. Returns NULL (no forecast) if Scope 1's series doesn't have
  # a value at exactly this scope's last real year -- no forecast line is
  # safer than a silently wrong anchor point. Shared by the trend chart AND
  # the gap chart so they can never show two different "forecast" numbers
  # for the same real data again.
  compute_option_b_forecast <- function(real_data, s1_full) {
    if (is.null(real_data) || nrow(real_data) == 0) return(NULL)
    last_real_year  <- max(real_data$year)
    last_real_value <- real_data$emissions[real_data$year == last_real_year][1]

    anchor_row <- s1_full %>% filter(year == last_real_year)
    if (nrow(anchor_row) != 1 || is.na(anchor_row$value[1]) || anchor_row$value[1] <= 0) return(NULL)

    anchor_value <- anchor_row$value[1]
    fwd <- s1_full %>% filter(year > last_real_year) %>%
      transmute(year, value = last_real_value * (value / anchor_value))
    if (nrow(fwd) == 0) return(NULL)

    bind_rows(data.frame(year = last_real_year, value = last_real_value), fwd)
  }

  # Own Goal target series: THIS scope's own real baseline (its last real
  # year), decayed forward at the SAME annual rate as Scope 1's own target
  # -- when real data exists for this scope. Falls back to Scope 1's own
  # target level rescaled by the sector ratio (v1 estimate) otherwise.
  # Shared by the trend chart's green line AND the gap chart's Own Goal
  # bars so they can never show two different numbers for the same real
  # data again (this is exactly how the forecast/Option B split was fixed
  # earlier -- same pattern, applied to the target side).
  compute_own_goal_series <- function(real_data, path, ratio_mult) {
    base_df <- path %>% transmute(year, value = target * ratio_mult)
    if (is.null(real_data) || nrow(real_data) == 0) return(base_df)

    own_target_year    <- max(path$year)
    annual_rate        <- path$annual_rate[1]
    own_baseline_year  <- max(real_data$year)
    own_baseline_value <- real_data$emissions[real_data$year == own_baseline_year][1]
    if (own_baseline_year >= own_target_year) return(base_df)

    # BUGFIX (was): bind_rows(own_series, base_df %>% filter(!year %in%
    # own_series$year)) tried to "fill in" any base_df years own_series
    # didn't cover. That's fine when own_baseline_year <= path's own
    # start (own_series is then a strict superset, nothing left to fill).
    # But if THIS scope's real data extends one year later than Scope 1's
    # own baseline (path starts later than own_baseline_year), a single
    # leftover base_df year slips through -- Scope-1-scaled (e.g.
    # ~120,000), sitting directly next to this scope's real-scaled values
    # (e.g. ~15,000). That one wrong-scale point created a near-vertical
    # jump, which a dotted linetype rendered as stray isolated dots.
    # Once real data exists for a scope, its target line should be built
    # entirely from ITS OWN baseline -- never stitched with a leftover
    # point from a different scope's scale.
    own_years <- own_baseline_year:own_target_year
    data.frame(
      year  = own_years,
      value = own_baseline_value * (1 - annual_rate)^(own_years - own_baseline_year)
    )
  }

  build_intake_trend_plot <- function(scope_mult, sbti_col, scope_label, real_data_reactive = NULL, mode = "absolute") {
    req(input$intake_sector)
    col <- sector_colors[[input$intake_sector]]
    if (is.null(col) || is.na(col)) col <- "#34495E"

    bm   <- intake_sector_benchmark()
    path <- intake_target_pathway()
    has_data <- path$has_data[1]

    # Real scope-specific data, if the user provided it -- takes
    # precedence over the ratio estimate entirely for "Your emissions".
    real_data <- if (!is.null(real_data_reactive)) real_data_reactive() else NULL
    using_real_data <- !is.null(real_data) && nrow(real_data) > 0

    series_list <- list()

    series_list[["Industry benchmark (observed)"]] <- bm %>% transmute(year, value = avg_emissions * scope_mult)
    if (isTRUE(input$intake_show_forecast)) {
      bm_last_year <- max(bm$year)
      bridge_bm_fore <- bind_rows(
        bm %>% filter(year == bm_last_year) %>% transmute(year, avg_p50 = avg_emissions),
        intake_sector_forecast_benchmark() %>% filter(year > bm_last_year)
      )
      if (nrow(bridge_bm_fore) > 1) {
        series_list[["Industry benchmark (forecast)"]] <- bridge_bm_fore %>% transmute(year, value = avg_p50 * scope_mult)
      }
    }

    if (using_real_data) {
      # Real data provided for this scope -- shown as-is, no rescaling.
      series_list[["Your emissions (observed)"]] <- real_data %>% transmute(year, value = emissions)

      if (isTRUE(input$intake_show_forecast)) {
        # Option B: anchor to this scope's own last real year, then extend
        # forward using the SAME year-over-year growth shape as Scope 1's
        # own series. Now a shared helper (intake_s1_full_series() /
        # compute_option_b_forecast()) -- intake_gap_multiscope() calls the
        # exact same functions, so the gap chart can never diverge from
        # what's drawn here again.
        fwd_series <- compute_option_b_forecast(real_data, intake_s1_full_series())
        if (!is.null(fwd_series)) series_list[["Your forecast"]] <- fwd_series
      }
    } else if (has_data) {
      ud <- intake_user_data()
      series_list[["Your emissions (observed)"]] <- ud %>% transmute(year, value = emissions * scope_mult)

      if (isTRUE(input$intake_show_forecast)) {
        uf <- intake_user_forecast()
        if (!is.null(uf) && nrow(uf) > 0) {
          bridge_user_fore <- bind_rows(
            ud %>% filter(year == max(year)) %>% transmute(year, p50 = emissions),
            uf
          )
          series_list[["Your forecast"]] <- bridge_user_fore %>% transmute(year, value = p50 * scope_mult)
        }
      }
    }

    target_label <- if (has_data) "Your stated goal" else "Interim goal (benchmark-anchored)"
    # BUGFIX (was): always path$target * scope_mult, ignoring real Scope
    # 2/3 data entirely -- with a ~1.0 Scope 3 ratio this plotted Scope
    # 1's own target almost unchanged on the Scope 3 chart, wildly off
    # the real Scope 3 data's own scale. Now shares the same real-data
    # logic as the gap chart's Own Goal bars.
    series_list[[target_label]] <- compute_own_goal_series(real_data, path, scope_mult)

    # SBTi line -- real per-scope data only, never ratio-derived. Omitted
    # entirely (not estimated) if this scope isn't configured on the
    # Calculator tab.
    sbti_result <- sbti_calc_result()
    if (is.null(sbti_result$error) && !is.null(sbti_result$path) && nrow(sbti_result$path) > 0 &&
        sbti_col %in% names(sbti_result$path)) {
      sbti_df <- sbti_result$path %>% transmute(year, value = .data[[sbti_col]])
      series_list[["SBTi-calculated goal"]] <- sbti_df
    }

    # Industry Target line -- runs the SAME SBTi engine as the purple
    # line above, but on the sector's median facility (grey line) instead
    # of the company's own numbers. Distinct from the grey "Industry
    # benchmark" line itself: grey is where the industry currently IS;
    # this yellow line is where the industry's own SBTi-methodology
    # trajectory says it's headed. Omitted entirely if that can't be
    # computed (e.g. Sectoral Decarbonization Approach selected, or this
    # scope isn't configured) -- same discipline as the purple line.
    industry_result <- tryCatch(intake_industry_sbti_result(), error = function(e) list(error = conditionMessage(e)))
    if (is.null(industry_result$error) && !is.null(industry_result$path) && nrow(industry_result$path) > 0 &&
        sbti_col %in% names(industry_result$path)) {
      industry_df <- industry_result$path %>% transmute(year, value = .data[[sbti_col]])
      series_list[["Industry-calculated goal"]] <- industry_df
    }

    plot_df <- bind_rows(
      lapply(names(series_list), function(nm) series_list[[nm]] %>% mutate(series = nm))
    ) %>%
      mutate(series = factor(series, levels = names(series_list)))

    # Percent-change view -- per the meeting decision to show a SEPARATE
    # percentage-reduction graph alongside the total-emissions one, not a
    # toggle replacing it (both plotOutputs exist side by side). Each
    # series is normalized to ITS OWN first plotted year as 0% -- e.g.
    # "Your emissions" shows % change from your own first real year,
    # "SBTi-calculated goal" shows % change from ITS OWN starting value
    # -- since the different series don't all start at the same year
    # (SBTi/Industry targets can start well before your own baseline
    # year), anchoring everything to one shared year would leave some
    # series without a valid 0% reference point at all.
    if (identical(mode, "percent")) {
      plot_df <- plot_df %>%
        group_by(series) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(value = {
          base_val <- dplyr::first(value)
          if (is.na(base_val) || base_val == 0) NA_real_ else (value / base_val - 1) * 100
        }) %>%
        ungroup()
    }

    point_df <- plot_df %>% filter(series %in% c("Industry benchmark (observed)", "Your emissions (observed)"))

    series_colors <- c(
      "Industry benchmark (observed)" = "grey55",
      "Industry benchmark (forecast)" = "grey55",
      "Your emissions (observed)"     = col,
      "Your forecast"                 = col,
      "Your stated goal"               = target_color,
      "Interim goal (benchmark-anchored)" = target_color,
      "SBTi-calculated goal"           = "#8E44AD",
      "Industry-calculated goal"       = "#F39C12"
    )
    series_linetypes <- c(
      "Industry benchmark (observed)" = "solid",
      "Industry benchmark (forecast)" = "dashed",
      "Your emissions (observed)"     = "solid",
      "Your forecast"                 = "dashed",
      "Your stated goal"               = "dotted",
      "Interim goal (benchmark-anchored)" = "dotted",
      "SBTi-calculated goal"           = "dashed",
      "Industry-calculated goal"       = "dotdash"
    )

    # BUGFIX (was): linewidth and alpha were ALSO mapped as discrete
    # aes() scales keyed to the same "series" variable as color and
    # linetype, plus a custom guides(color = guide_legend(nrow = 2)).
    # ggplotly() is known to throw "subscript out of bounds" on exactly
    # this combination -- 4 discrete aesthetics on one variable plus a
    # multi-row legend guide is more than its legend-building logic
    # reliably handles, even though plain ggplot2 renders it fine. Fixed
    # by dropping linewidth/alpha as separate mapped scales (a minor,
    # acceptable cosmetic simplification -- color + linetype alone still
    # fully distinguish all 8 series) and removing the custom multi-row
    # legend guide, letting plotly wrap the legend on its own.
    # Shaded +/-10% error band around "Your forecast" only -- per
    # Avishkar's note (Jul 31 sync) that projections need a visible
    # error margin. Applied only to the forecast series, not the
    # observed/real data (an error margin on real measured data doesn't
    # mean the same thing as one on a projection), and only in absolute
    # mode (a +/-10% band on a % value would need its own separate
    # interpretation this app doesn't currently make). Fixed at 10% --
    # not derived from any actual uncertainty estimate in the model, so
    # labeled explicitly as illustrative in the subtitle rather than
    # implied to be a real statistical confidence interval.
    forecast_band_df <- if (identical(mode, "absolute")) {
      plot_df %>% filter(series == "Your forecast") %>% mutate(ymin = value * 0.9, ymax = value * 1.1)
    } else {
      NULL
    }

    ggplot(plot_df, aes(x = year, y = value, color = series, linetype = series, group = series)) +
      { if (!is.null(forecast_band_df) && nrow(forecast_band_df) > 0) {
          geom_ribbon(data = forecast_band_df, aes(x = year, ymin = ymin, ymax = ymax),
                      inherit.aes = FALSE, fill = col, alpha = 0.15)
        } } +
      geom_line(linewidth = 1.1) +
      geom_point(
        data = point_df,
        aes(x = year, y = value, color = series),
        inherit.aes = FALSE, size = 1.8
      ) +
      scale_color_manual(values = series_colors, name = NULL) +
      scale_linetype_manual(values = series_linetypes, name = NULL) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = if (identical(mode, "percent")) function(x) paste0(x, "%") else comma) +
      labs(
        title = if (nzchar(input$intake_company_name)) input$intake_company_name else "New Company",
        subtitle = paste0(
          input$intake_sector, " -- ", scope_label,
          if (scope_mult != 1 && !using_real_data) " (v1 estimate: Hertwich & Wood 2018 sector ratio, not measured)" else if (scope_mult != 1 && using_real_data) " (your real data; forecast borrows Scope 1's growth shape)" else "",
          if (identical(mode, "percent")) " -- % change from each series' own starting year" else "",
          if (!is.null(forecast_band_df) && nrow(forecast_band_df) > 0) " -- shaded band = illustrative +/-10% projection error margin" else ""
        ),
        x = NULL, y = if (identical(mode, "percent")) "% change from baseline" else "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.subtitle = element_text(color = "grey40", size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
      )
  }

  output$intake_plot <- renderPlot({
    build_intake_trend_plot(scope_mult = 1, sbti_col = "scope1_emissions", scope_label = "Scope 1")
  })

  output$intake_plot_pct <- renderPlot({
    build_intake_trend_plot(scope_mult = 1, sbti_col = "scope1_emissions", scope_label = "Scope 1", mode = "percent")
  })

  output$intake_plot_s2 <- renderPlot({
    ratio <- get_scope23_ratio(input$intake_sector)
    req(!is.null(ratio))
    build_intake_trend_plot(scope_mult = ratio$scope2_multiplier, sbti_col = "scope2_emissions",
                             scope_label = "Scope 2", real_data_reactive = intake_user_data_s2)
  })

  output$intake_plot_s2_pct <- renderPlot({
    ratio <- get_scope23_ratio(input$intake_sector)
    req(!is.null(ratio))
    build_intake_trend_plot(scope_mult = ratio$scope2_multiplier, sbti_col = "scope2_emissions",
                             scope_label = "Scope 2", real_data_reactive = intake_user_data_s2, mode = "percent")
  })

  output$intake_plot_s3 <- renderPlot({
    ratio <- get_scope23_ratio(input$intake_sector)
    req(!is.null(ratio))
    build_intake_trend_plot(scope_mult = ratio$scope3_multiplier, sbti_col = "scope3_emissions",
                             scope_label = "Scope 3", real_data_reactive = intake_user_data_s3)
  })

  output$intake_plot_s3_pct <- renderPlot({
    ratio <- get_scope23_ratio(input$intake_sector)
    req(!is.null(ratio))
    build_intake_trend_plot(scope_mult = ratio$scope3_multiplier, sbti_col = "scope3_emissions",
                             scope_label = "Scope 3", real_data_reactive = intake_user_data_s3, mode = "percent")
  })

  # BUGFIX (was): these were hardcoded h5("Scope 2 (v1 estimate)") /
  # h5("Scope 3 (v1 estimate)") in the UI, so they kept saying "v1
  # estimate" even after you'd entered real Scope 2/3 data and every
  # chart below had switched over to using it. Now reactive, matching
  # the same using_real_data check the trend chart's own subtitle uses.
  output$intake_scope2_header <- renderUI({
    real <- intake_user_data_s2()
    h5(if (!is.null(real) && nrow(real) > 0) "Scope 2 (your real data)" else "Scope 2 (v1 estimate)")
  })

  output$intake_scope3_header <- renderUI({
    real <- intake_user_data_s3()
    h5(if (!is.null(real) && nrow(real) > 0) "Scope 3 (your real data)" else "Scope 3 (v1 estimate)")
  })

  output$intake_legend_note <- renderUI({
    req(input$intake_sector)
    path <- intake_target_pathway()
    has_data <- path$has_data[1]
    sbti_result <- sbti_calc_result()

    sbti_note <- if (!is.null(sbti_result$error)) {
      tagList(tags$br(), tags$b("Purple"), " = SBTi-calculated goal -- ", tags$em("could not be computed: ", sbti_result$error))
    } else {
      tagList(tags$br(), tags$b("Purple dashed"), " = SBTi-calculated goal, using SBTi's own ",
              input$sbti_calc_method, " methodology -- same calculation as the \"SBTi Calculator\" tab, not a sector proxy. ",
              "Shown per-scope: Scope 1 & 2 share one Base Year/Target Year/MRY there, Scope 3 has its own independent set. ",
              "A scope's line is omitted here if it isn't configured on that tab.")
    }

    industry_note <- {
      industry_result <- tryCatch(intake_industry_sbti_result(), error = function(e) list(error = conditionMessage(e)))
      if (is.null(industry_result$error) && !is.null(industry_result$path) && nrow(industry_result$path) > 0) {
        tagList(tags$br(), tags$b("Yellow dot-dash"), " = Industry Target -- the SAME SBTi methodology as the purple line, ",
                "run on the sector's median facility (grey line) instead of your own numbers. ",
                "NOT the same as the grey line itself: grey is where the industry currently ", tags$em("is"), "; this is where the industry's ",
                "own SBTi trajectory says it's headed.")
      } else {
        tagList(tags$br(), tags$b("Yellow"), " = Industry Target -- ",
                tags$em("could not be computed: ", if (!is.null(industry_result$error)) industry_result$error else "no sector benchmark data available."))
      }
    }

    tagList(
      tags$b("Grey"), " = industry benchmark (", tags$b("median"), " facility in ", input$intake_sector, ") -- solid is observed, dashed is forecast. ",
      if (has_data) {
        tagList(
          tags$b("Blue"), " = ", tags$b(if (nzchar(input$intake_company_name)) input$intake_company_name else "your company"),
          "'s own numbers -- solid is what you pasted, dashed is its forecast (your baseline, scaled by the sector's growth trend). "
        )
      } else {
        tagList(tags$em("You haven't submitted your own data, so the industry benchmark also doubles as your interim baseline. "))
      },
      tags$b("Green dotted"), " = ", if (has_data) "your stated goal. " else "the interim goal, anchored to the benchmark. ",
      industry_note,
      sbti_note
    )
  })

  output$intake_table <- renderDT({
    path <- intake_target_pathway() %>% transmute(year, target = round(target))
    bm   <- intake_sector_benchmark() %>% transmute(year, sector_avg = round(avg_emissions))
    out  <- path %>% full_join(bm, by = "year") %>% arrange(year)

    sector_fc <- intake_sector_forecast_benchmark() %>%
      transmute(year, sector_forecast = round(avg_p50))
    out <- out %>% full_join(sector_fc, by = "year") %>% arrange(year)

    if (isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())) {
      ud  <- intake_user_data() %>% transmute(year, your_data = round(emissions))
      out <- out %>% full_join(ud, by = "year") %>% arrange(year)

      uf <- intake_user_forecast()
      if (!is.null(uf) && nrow(uf) > 0) {
        uf_tbl <- uf %>% transmute(year, your_forecast = round(p50))
        out <- out %>% full_join(uf_tbl, by = "year") %>% arrange(year)
      }
    }

    datatable(out, options = list(pageLength = 15, dom = "tp"), rownames = FALSE)
  })

  # Gap = forecast minus target. Uses the user's own scaled forecast when
  # available (has data + forecast toggled on); otherwise falls back to the
  # sector benchmark forecast, same source as what's drawn on the dashed line.
  intake_gap <- reactive({
    req(input$intake_sector)
    path <- intake_target_pathway() %>% select(year, target)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())

    fc <- NULL
    if (has_data) {
      uf <- intake_user_forecast()
      if (!is.null(uf) && nrow(uf) > 0) fc <- uf
    }
    if (is.null(fc)) {
      fc <- intake_sector_forecast_benchmark() %>% rename(p50 = avg_p50)
    }

    req(!is.null(fc), nrow(fc) > 0)
    inner_join(fc %>% select(year, p50), path, by = "year") %>%
      mutate(gap = p50 - target)
  })

  # Gap against the SBTi-calculated goal instead of the stated goal --
  # same forecast source as intake_gap(), but joined against
  # sbti_calc_result()'s Scope 1 trajectory (the exact same source the
  # purple line on the 4-line chart uses) rather than intake_target_pathway().
  # This is deliberately a SEPARATE reactive, not a parameter on intake_gap(),
  # since the two targets can and often do diverge substantially -- SBTi's
  # real methodology is frequently more ambitious than a sector-proxy goal,
  # so the implied gap can be meaningfully larger.
  intake_gap_sbti <- reactive({
    req(input$intake_sector)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())

    fc <- NULL
    if (has_data) {
      uf <- intake_user_forecast()
      if (!is.null(uf) && nrow(uf) > 0) fc <- uf
    }
    if (is.null(fc)) {
      fc <- intake_sector_forecast_benchmark() %>% rename(p50 = avg_p50)
    }
    req(!is.null(fc), nrow(fc) > 0)

    sbti_result <- sbti_calc_result()
    req(is.null(sbti_result$error), !is.null(sbti_result$path), nrow(sbti_result$path) > 0)
    sbti_path <- sbti_result$path %>% transmute(year, target = scope1_emissions)

    inner_join(fc %>% select(year, p50), sbti_path, by = "year") %>%
      mutate(gap = p50 - target)
  })

  # Multi-scope gap -- Scope 2/3 now derived via the REAL Hertwich & Wood
  # (2018) sector ratios (get_scope23_ratio()), same methodology as the
  # pipeline's Part 4C, not a manually-typed percentage split. Applied to
  # BOTH the forecast and the stated ("Own Goal") target, since both are
  # fundamentally Scope 1-based (GHGRP only measures Scope 1) and need
  # the same extension. The SBTi target is NOT derived this way -- it
  # still comes directly from sbti_calc_result(), which requires the
  # company's own real Scope 2/3 base-year data on the SBTi Calculator
  # tab, since a target should reflect real data, not an estimated ratio.
  intake_gap_multiscope <- reactive({
    req(input$intake_sector)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())

    fc <- NULL
    if (has_data) {
      uf <- intake_user_forecast()
      if (!is.null(uf) && nrow(uf) > 0) fc <- uf
    }
    if (is.null(fc)) {
      fc <- intake_sector_forecast_benchmark() %>% rename(p50 = avg_p50)
    }
    req(!is.null(fc), nrow(fc) > 0)

    sbti_result <- sbti_calc_result()
    req(is.null(sbti_result$error), !is.null(sbti_result$path), nrow(sbti_result$path) > 0)
    sbti_path <- sbti_result$path

    own_path <- intake_target_pathway() %>% select(year, own_target = target, annual_rate)

    ratio <- get_scope23_ratio(input$intake_sector)
    req(!is.null(ratio))  # unmapped sector (Other, fluorinated-GHG-equipment) -- no Scope 2/3 estimate possible

    has_s2_target <- "scope2_emissions" %in% names(sbti_path)
    has_s3_target <- "scope3_emissions" %in% names(sbti_path)
    target_cols <- c("year", "scope1_emissions",
                      if (has_s2_target) "scope2_emissions",
                      if (has_s3_target) "scope3_emissions")
    target_df <- sbti_path[, target_cols, drop = FALSE]

    # Real Scope 2/3 data, if provided -- same source the trend charts
    # use. When present, it REPLACES the ratio estimate here too, so the
    # trend chart and the credit bars below it are never showing two
    # different numbers for the same scope.
    real_s2 <- intake_user_data_s2()
    real_s3 <- intake_user_data_s3()

    # BUGFIX (was): `fc` only ever holds FUTURE years (intake_user_forecast()
    # filters year > baseline_year), so a left_join of real_s2/real_s3's
    # historical years onto it could never match -- the "real data override"
    # silently had zero effect, and the fallback ratio estimate (a
    # DIFFERENT number than the trend chart's Option B forecast) was what
    # actually drove the gap bars. Fixed by building each scope's forecast
    # as its own full series (real historical years + Option B projection,
    # exactly like the trend chart), THEN outer-joining those series
    # together instead of constraining everything to fc's future-only years.
    s1_full <- intake_s1_full_series()

    build_scope_forecast_series <- function(real_data, ratio_mult) {
      # Ratio-based v1 estimate across the full forecast horizon -- the
      # fallback / base layer when there's no real data (or no real data
      # for years outside what Option B covers).
      base_df <- fc %>% transmute(year, value = p50 * ratio_mult)

      ob <- compute_option_b_forecast(real_data, s1_full)
      if (is.null(ob)) return(base_df)

      # Real historical years + Option B's forward projection -- the SAME
      # series drawn on the trend chart -- REPLACE the ratio estimate for
      # every year they cover.
      combined <- bind_rows(
        real_data %>% transmute(year, value = emissions),
        ob %>% filter(year > max(real_data$year))
      ) %>% arrange(year) %>% distinct(year, .keep_all = TRUE)

      bind_rows(combined, base_df %>% filter(!year %in% combined$year)) %>% arrange(year)
    }

    scope2_series <- build_scope_forecast_series(real_s2, ratio$scope2_multiplier) %>%
      rename(scope2_forecast = value)
    scope3_series <- build_scope_forecast_series(real_s3, ratio$scope3_multiplier) %>%
      rename(scope3_forecast = value)

    # Own Goal for Scope 2/3 -- shared helper (compute_own_goal_series),
    # same one the trend chart's green line uses, so the two can't
    # diverge. Needs a $target-named column, unlike own_path's
    # own_target rename below, so pass the pathway directly.
    path_full <- intake_target_pathway()
    scope2_own_series <- compute_own_goal_series(real_s2, path_full, ratio$scope2_multiplier) %>%
      rename(scope2_target_own = value)
    scope3_own_series <- compute_own_goal_series(real_s3, path_full, ratio$scope3_multiplier) %>%
      rename(scope3_target_own = value)

    # Industry Target -- runs the SAME SBTi engine as the SBTi Target
    # (gap1_sbti etc.) above, but on the sector's median facility (grey
    # line) instead of the company's own numbers. NOT the grey benchmark
    # line itself (that's where the industry currently IS; this is where
    # its own SBTi-methodology trajectory says it's headed). Replaces the
    # earlier target_lookup %-reduction version, which could end up
    # mathematically identical to Own Goal -- see intake_industry_sbti_result()
    # for why. Left-joined (not full_join) like target_df below, since
    # this is a target to compare against df's own year range, not a
    # series that should extend it.
    industry_result <- tryCatch(intake_industry_sbti_result(), error = function(e) list(error = conditionMessage(e)))
    has_industry <- is.null(industry_result$error) && !is.null(industry_result$path) && nrow(industry_result$path) > 0
    if (has_industry) {
      ipath <- industry_result$path
      has_i_s2 <- "scope2_emissions" %in% names(ipath)
      has_i_s3 <- "scope3_emissions" %in% names(ipath)
      icols <- c("year", "scope1_emissions", if (has_i_s2) "scope2_emissions", if (has_i_s3) "scope3_emissions")
      industry_target_df <- ipath[, icols, drop = FALSE] %>% rename(scope1_target_industry = scope1_emissions)
      if (has_i_s2) industry_target_df <- industry_target_df %>% rename(scope2_target_industry = scope2_emissions)
      if (has_i_s3) industry_target_df <- industry_target_df %>% rename(scope3_target_industry = scope3_emissions)
    }

    # full_join (not fc's year range alone) -- real historical years from
    # scope2_series/scope3_series now genuinely extend df's year coverage,
    # so they can actually reach the SBTi target join below instead of
    # being silently dropped.
    df <- fc %>%
      transmute(year, scope1_forecast = p50) %>%
      full_join(scope2_series, by = "year") %>%
      full_join(scope3_series, by = "year") %>%
      full_join(scope2_own_series, by = "year") %>%
      full_join(scope3_own_series, by = "year") %>%
      arrange(year)

    if (has_industry) {
      df <- df %>% left_join(industry_target_df, by = "year")
      if (!has_i_s2) df$scope2_target_industry <- NA_real_
      if (!has_i_s3) df$scope3_target_industry <- NA_real_
    }

    # LEFT join, not inner -- if the SBTi Calculator's year range doesn't
    # fully overlap with the forecast's, this keeps every forecast year
    # and shows NA (missing bar) only for the years that genuinely don't
    # have a matching target, rather than silently dropping the ENTIRE
    # scope's bars the moment any single year fails to line up.
    df <- df %>%
      left_join(target_df, by = "year") %>%
      left_join(own_path %>% select(year, own_target), by = "year") %>%
      rename(scope1_target_sbti = scope1_emissions) %>%
      mutate(scope1_target_own = own_target)

    if (has_s2_target) df <- df %>% rename(scope2_target_sbti = scope2_emissions) else df$scope2_target_sbti <- NA_real_
    if (has_s3_target) df <- df %>% rename(scope3_target_sbti = scope3_emissions) else df$scope3_target_sbti <- NA_real_

    result <- df %>% mutate(
      gap1_sbti = scope1_forecast - scope1_target_sbti,
      gap2_sbti = scope2_forecast - scope2_target_sbti,
      gap3_sbti = scope3_forecast - scope3_target_sbti,
      gap1_own  = scope1_forecast - scope1_target_own,
      gap2_own  = scope2_forecast - scope2_target_own,
      gap3_own  = scope3_forecast - scope3_target_own
    )
    if (has_industry) {
      result <- result %>% mutate(
        gap1_industry = scope1_forecast - scope1_target_industry,
        gap2_industry = scope2_forecast - scope2_target_industry,
        gap3_industry = scope3_forecast - scope3_target_industry
      )
    }
    attr(result, "has_industry_target") <- has_industry
    attr(result, "confidence") <- ratio$confidence
    attr(result, "forecast_years") <- range(fc$year)
    # PER-SCOPE year ranges -- where sbti_path itself actually has
    # non-NA data for that specific scope, not the overall merged range.
    # CORRECTED: only Scope 3 has its OWN independent Base Year/Target
    # Year/MRY on the SBTi Calculator tab. Scope 2 shares Scope 1's
    # Base Year/Target Year/MRY (just its own emissions values within
    # that shared timeline) -- confirmed against the Calculator tab's
    # actual inputs. Kept as separate attrs regardless, since Scope 3's
    # genuinely independent range still needs its own overlap check.
    s1_years <- sbti_path$year[!is.na(sbti_path$scope1_emissions)]
    attr(result, "s1_sbti_years") <- if (length(s1_years) > 0) range(s1_years) else c(NA, NA)
    if (has_s2_target) {
      s2_years <- sbti_path$year[!is.na(sbti_path$scope2_emissions)]
      attr(result, "s2_sbti_years") <- if (length(s2_years) > 0) range(s2_years) else c(NA, NA)
    }
    if (has_s3_target) {
      s3_years <- sbti_path$year[!is.na(sbti_path$scope3_emissions)]
      attr(result, "s3_sbti_years") <- if (length(s3_years) > 0) range(s3_years) else c(NA, NA)
    }
    result
  })

  output$intake_multiscope_note <- renderUI({
    err_msg <- NULL
    result <- tryCatch(intake_gap_multiscope(), error = function(e) { err_msg <<- conditionMessage(e); NULL })
    if (is.null(result)) {
      # BUGFIX (was): this always blamed "no ratio mapping for this
      # sector" regardless of the ACTUAL failure reason -- misleading
      # when the sector genuinely has one (e.g. Chemicals) and something
      # else inside intake_gap_multiscope() failed instead. Now checks
      # the real, specific cause before falling back to that message.
      ratio <- tryCatch(get_scope23_ratio(input$intake_sector), error = function(e) NULL)
      if (is.null(ratio)) {
        return(tags$div(
          style = "background:#FDEBD0; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:0.5rem; font-size:12.5px;",
          em("Scope 2/3 not estimated for this sector -- no Hertwich & Wood (2018) ratio mapping exists ",
             "for it (this applies to \"Other\" and fluorinated-GHG-equipment).")
        ))
      }
      sbti_result <- tryCatch(sbti_calc_result(), error = function(e) list(error = conditionMessage(e)))
      if (!is.null(sbti_result$error)) {
        return(tags$div(
          style = "background:#FDEBD0; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:0.5rem; font-size:12.5px;",
          em("Gap calculation unavailable -- the SBTi Calculator tab hasn't produced a valid target yet ",
             "(", sbti_result$error, "). Set it there and this section will populate.")
        ))
      }
      return(tags$div(
        style = "background:#FDEBD0; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:0.5rem; font-size:12.5px;",
        em("Gap calculation unavailable -- ", if (!is.null(err_msg)) paste0("error: ", err_msg) else "unknown cause",
           ". If this persists after checking the SBTi Calculator tab's inputs, this may be a bug worth reporting.")
      ))
    }
    has_s2 <- !all(is.na(result$scope2_target_sbti))
    has_s3 <- !all(is.na(result$scope3_target_sbti))
    fc_yrs   <- attr(result, "forecast_years")
    s1_yrs <- attr(result, "s1_sbti_years")
    s2_yrs <- attr(result, "s2_sbti_years")
    s3_yrs <- attr(result, "s3_sbti_years")

    # Per-scope overlap check. Scope 3 has its OWN independent Base
    # Year/Target Year on the SBTi Calculator, genuinely different from
    # Scope 1/2's shared timeline -- a mismatch in Scope 3's years does
    # not mean Scope 1/2 are broken too (and vice versa).
    describe_scope <- function(label, yrs, configured, shares_s1_years = FALSE) {
      if (!configured) return(paste0(label, ": NOT configured on the SBTi Calculator tab."))
      if (all(is.na(yrs))) return(paste0(label, ": configured, but produced no valid years at all -- check its inputs."))
      overlap <- yrs[1] <= fc_yrs[2] && yrs[2] >= fc_yrs[1]
      if (overlap) {
        paste0(label, ": years ", yrs[1], "-", yrs[2], " -- overlaps the forecast, bar included below.")
      } else if (shares_s1_years) {
        paste0(label, ": years ", yrs[1], "-", yrs[2], " -- does NOT overlap the forecast (",
               fc_yrs[1], "-", fc_yrs[2], "). Scope 2 shares Scope 1's Base Year/Target Year on the ",
               "SBTi Calculator, so that shared Base Year/Target Year needs to cover this range.")
      } else {
        paste0(label, ": years ", yrs[1], "-", yrs[2], " -- does NOT overlap the forecast (",
               fc_yrs[1], "-", fc_yrs[2], "). Its OWN independent Base Year/Target Year on the ",
               "SBTi Calculator need to cover this range for its bar to appear.")
      }
    }

    tags$div(
      style = "background:#F4F6F7; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:0.5rem; font-size:12.5px;",
      em(
        "Scope 2/3 forecast and \"Own Goal\" target: real data where you've entered it, otherwise a v1 ",
        "ESTIMATE (Hertwich & Wood 2018 sector ratio, confidence: ", attr(result, "confidence"), ")."
      ),
      tags$br(), tags$b("Forecast year range: "), fc_yrs[1], "-", fc_yrs[2],
      tags$br(), tags$b("Scope 1 SBTi -- "), describe_scope("", s1_yrs, TRUE),
      tags$br(), tags$b("Scope 2 SBTi -- "), describe_scope("", s2_yrs, has_s2, shares_s1_years = TRUE),
      tags$br(), tags$b("Scope 3 SBTi -- "), describe_scope("", s3_yrs, has_s3)
    )
  })

  # Shared builder -- 3-scenario grouped bars: green "SBTi Target", yellow
  # "Industry Target" (the sector's published decarbonization target from
  # target_lookup, distinct from the grey industry BENCHMARK line -- this
  # is where the industry says it's headed, not where it currently is),
  # and red "Your Own Goal". SBTi ordered FIRST (per Rajat, Jul 31 sync)
  # since it's the achievement goal -- seeing it low on the level-bar
  # chart above primes the reader for why IT shows the biggest gap here,
  # rather than the reverse (a green bar reading "high/good" here being
  # the counterintuitive thing Avishkar flagged). A negative value
  # (forecast already below target -- a genuine surplus, not a gap) draws
  # as a real bar extending BELOW zero, rather than floored to 0 with a
  # text label -- so a surplus is visually distinct from "no data" at a
  # glance, not just in the tooltip text. industry_col is optional --
  # omitted (not NULL-filled) when the sector has no target_lookup entry,
  # same discipline as SBTi.
  make_scope_gap_pair_plot <- function(df, own_col, sbti_col, industry_col = NULL) {
    own  <- df %>% filter(!is.na(.data[[own_col]])) %>%
      transmute(year, gap = .data[[own_col]], scenario = "Your Own Goal")
    sbti <- df %>% filter(!is.na(.data[[sbti_col]])) %>%
      transmute(year, gap = .data[[sbti_col]], scenario = "SBTi Target")
    industry <- if (!is.null(industry_col) && industry_col %in% names(df)) {
      df %>% filter(!is.na(.data[[industry_col]])) %>%
        transmute(year, gap = .data[[industry_col]], scenario = "Industry Target")
    } else {
      NULL
    }

    req(nrow(own) > 0 || nrow(sbti) > 0 || (!is.null(industry) && nrow(industry) > 0))

    scenario_levels <- c("SBTi Target", "Industry Target", "Your Own Goal")
    plot_df <- bind_rows(sbti, industry, own) %>%
      mutate(
        scenario = factor(scenario, levels = scenario_levels),
        year     = factor(year),
        # Label sits just outside the bar's own tip -- above for a gap
        # (positive), below for a surplus (negative) -- and calls out a
        # surplus explicitly so it's never mistaken for a small gap.
        label    = ifelse(gap < 0, paste0("Surplus: ", comma(round(-gap))), comma(round(gap))),
        label_vjust = ifelse(gap < 0, 1.3, -0.3)
      )

    ggplot(plot_df, aes(x = year, y = gap, fill = scenario)) +
      geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = label, vjust = label_vjust), position = position_dodge(width = 0.75), size = 3.4) +
      scale_fill_manual(
        values = c("SBTi Target" = "#27AE60", "Industry Target" = "#F39C12", "Your Own Goal" = "#C0392B"),
        name = NULL, drop = FALSE
      ) +
      scale_y_continuous(labels = comma, expand = expansion(mult = 0.15)) +
      labs(
        subtitle = "Credits needed per year, by target scenario -- bars below zero mean you're already ahead of that target",
        x = NULL, y = "Gap (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top",
            legend.text = element_text(size = 13))
  }

  # NEW per the Jul 31 sync: the line graph, converted to a bar chart, as
  # the SECOND visual (line graph first, this second, the gap chart
  # third) -- per Avishkar's fix for the psychological confusion where a
  # tall "green" bar in the GAP chart reads as good/high when it's
  # actually the biggest shortfall. Showing the raw EMISSION LEVELS as
  # bars first -- with SBTi Target low (it's the most aggressive target)
  # -- primes the reader for why the SAME SBTi category shows the
  # tallest bar in the gap chart right after: a low target vs. a high
  # forecast IS a big gap, and seeing the low bar first makes that click.
  # Same SBTi-first ordering as the gap chart, plus "Your Forecast" (the
  # actual trajectory being compared against all three targets) last.
  make_scope_level_bar_plot <- function(df, forecast_col, own_col, sbti_col, industry_col = NULL, real_data = NULL) {
    sbti <- df %>% filter(!is.na(.data[[sbti_col]])) %>%
      transmute(year, value = .data[[sbti_col]], scenario = "SBTi Target")
    industry <- if (!is.null(industry_col) && industry_col %in% names(df)) {
      df %>% filter(!is.na(.data[[industry_col]])) %>%
        transmute(year, value = .data[[industry_col]], scenario = "Industry Target")
    } else {
      NULL
    }
    own <- df %>% filter(!is.na(.data[[own_col]])) %>%
      transmute(year, value = .data[[own_col]], scenario = "Your Own Goal")

    # BUGFIX: forecast_col alone only covers FUTURE years (e.g.
    # 2024-2028 for Scope 1) -- it never includes your real observed
    # historical data, so the "emissions" bar had a gap for every
    # earlier year, unlike the line chart's red line (solid observed +
    # dashed forecast, no gap). This blends real_data (your actual
    # entered values) with forecast_col into ONE continuous series --
    # real data wins for any year both cover. "Your Forecast" renamed
    # to "Your Emissions" since it's now observed+forecast combined,
    # not forecast-only.
    forecast_df <- df %>% filter(!is.na(.data[[forecast_col]])) %>%
      transmute(year, value = .data[[forecast_col]])
    if (!is.null(real_data) && nrow(real_data) > 0) {
      real_df <- real_data %>% transmute(year, value = emissions)
      forecast_df <- bind_rows(real_df, forecast_df %>% filter(!year %in% real_df$year)) %>% arrange(year)
    }

    # BUGFIX: the model's own forecast horizon (e.g. 2024-2028) doesn't
    # start right where your real data ends (e.g. 2017) -- there's a
    # real gap in between (2018-2023) with no data point at all. The
    # LINE chart's dashed red segment LOOKS continuous through that gap
    # only because geom_line() draws a straight segment directly from
    # the last real point to the first forecast point -- not real data,
    # just how connected lines render across missing years. To make the
    # bar chart show the exact same story (not a gap the line chart
    # visually papers over), linearly interpolate the missing years the
    # same way that connecting line implies, rather than leaving them
    # blank.
    if (nrow(forecast_df) >= 2) {
      full_years <- min(forecast_df$year):max(forecast_df$year)
      missing_years <- setdiff(full_years, forecast_df$year)
      if (length(missing_years) > 0) {
        interp <- approx(forecast_df$year, forecast_df$value, xout = missing_years)
        forecast_df <- bind_rows(forecast_df, data.frame(year = interp$x, value = interp$y)) %>% arrange(year)
      }
    }
    forecast <- forecast_df %>% mutate(scenario = "Your Emissions")

    req(nrow(sbti) > 0 || nrow(own) > 0 || nrow(forecast) > 0 || (!is.null(industry) && nrow(industry) > 0))

    # Cap at the forecast's own last year -- matches the gap chart
    # below, which only ever covers the forecast horizon; showing SBTi/
    # Industry years beyond that (years the forecast itself doesn't
    # reach) just adds width without a like-for-like comparison there.
    # Derived from the data itself (not hardcoded to e.g. 2028) so this
    # stays correct if the baseline year, and so the forecast horizon,
    # ever shifts.
    max_year <- if (nrow(forecast) > 0) max(forecast$year) else max(df$year[!is.na(df[[forecast_col]])])

    scenario_levels <- c("SBTi Target", "Industry Target", "Your Own Goal", "Your Emissions")
    plot_df <- bind_rows(sbti, industry, own, forecast) %>%
      filter(year <= max_year) %>%
      mutate(
        scenario = factor(scenario, levels = scenario_levels),
        year     = factor(year)
      )

    ggplot(plot_df, aes(x = year, y = value, fill = scenario)) +
      geom_col(position = position_dodge(width = 0.85), width = 0.8) +
      scale_fill_manual(
        values = c("SBTi Target" = "#27AE60", "Industry Target" = "#F39C12",
                   "Your Own Goal" = "#8E44AD", "Your Emissions" = "#C0392B"),
        name = NULL, drop = FALSE
      ) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(
        subtitle = "Same lines as the chart below, as bars -- SBTi Target shown low is intentional: it's the most ambitious target",
        x = NULL, y = "Emissions (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top",
            axis.text.x = element_text(size = 10))
  }

  output$intake_level_bar_s1_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    real_data <- if (isTRUE(input$intake_has_data == "yes")) intake_user_data() else NULL
    make_scope_level_bar_plot(df, "scope1_forecast", "scope1_target_own", "scope1_target_sbti", "scope1_target_industry", real_data)
  })

  output$intake_level_bar_s2_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    real_data <- tryCatch(intake_user_data_s2(), error = function(e) NULL)
    make_scope_level_bar_plot(df, "scope2_forecast", "scope2_target_own", "scope2_target_sbti", "scope2_target_industry", real_data)
  })

  output$intake_level_bar_s3_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    real_data <- tryCatch(intake_user_data_s3(), error = function(e) NULL)
    make_scope_level_bar_plot(df, "scope3_forecast", "scope3_target_own", "scope3_target_sbti", "scope3_target_industry", real_data)
  })

  output$intake_gap_s1_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    make_scope_gap_pair_plot(df, "gap1_own", "gap1_sbti", "gap1_industry")
  })

  output$intake_gap_s2_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    make_scope_gap_pair_plot(df, "gap2_own", "gap2_sbti", "gap2_industry")
  })

  output$intake_gap_s3_plot <- renderPlot({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    make_scope_gap_pair_plot(df, "gap3_own", "gap3_sbti", "gap3_industry")
  })

  output$intake_gap_plot <- renderPlot({
    df <- intake_gap()
    req(nrow(df) > 0)
    make_credit_bar(df, "gap", "Gap (tCO2e)")
  })

  output$intake_gap_sbti_plot <- renderPlot({
    df <- intake_gap_sbti()
    req(nrow(df) > 0)
    make_credit_bar(df, "gap", "Gap (tCO2e)")
  })

  # Combined grouped bar chart: both target scenarios' gap, side by side
  # per year -- red for Your Own Goal, green for SBTi Target, same
  # underlying data as the two separate charts above (kept, unused, in
  # case the split view is wanted back).
  output$intake_gap_combined_plot <- renderPlot({
    own  <- intake_gap() %>% transmute(year, gap = pmax(gap, 0), scenario = "Your Own Goal")
    sbti <- tryCatch(
      intake_gap_sbti() %>% transmute(year, gap = pmax(gap, 0), scenario = "SBTi Target"),
      error = function(e) NULL
    )
    req(nrow(own) > 0)

    df <- if (!is.null(sbti) && nrow(sbti) > 0) bind_rows(own, sbti) else own
    df <- df %>% mutate(
      scenario = factor(scenario, levels = c("Your Own Goal", "SBTi Target")),
      year     = factor(year)
    )

    ggplot(df, aes(x = year, y = gap, fill = scenario)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      scale_fill_manual(values = c("Your Own Goal" = "#C0392B", "SBTi Target" = "#27AE60"), name = NULL) +
      scale_y_continuous(labels = comma) +
      labs(
        subtitle = "Credits needed per year, by target scenario -- taller bar means a bigger gap to close",
        x = NULL, y = "Gap (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
  })

  # ---- Shared baseline + two more scenario pathways, for the 3-scenario
  # portfolio comparison (Current Goal / Industry Standard / SBTi) ----

  # Same has_data / baseline_year / baseline_value logic intake_target_pathway()
  # already computes -- extracted so Industry Standard and SBTi can anchor
  # to the exact same starting point without duplicating that branch.
  intake_baseline <- reactive({
    req(input$intake_sector)
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())

    if (has_data) {
      ud <- intake_user_data()
      baseline_year  <- max(ud$year)
      baseline_value <- ud$emissions[ud$year == baseline_year][1]
    } else {
      bm <- intake_sector_benchmark()
      req(nrow(bm) > 0)
      baseline_year  <- max(bm$year)
      baseline_value <- bm$avg_emissions[bm$year == baseline_year][1]
    }

    list(baseline_year = baseline_year, baseline_value = baseline_value, has_data = has_data)
  })

  # NOTE: the SBTi-calculated goal (line 4 in the plot below) is NOT
  # computed here. It intentionally reuses sbti_calc_result() -- the same
  # reactive the "SBTi Detail" sub-tab (under Company Profile) produces --
  # so the two are guaranteed identical, not two independent calculations
  # that could drift apart. Company name/historical emissions/target year
  # come from the shared New Company Intake fields; method/SDA/net-zero
  # year are set on the same Company Profile sidebar; this tab's plot
  # just displays whatever sbti_calc_result() returns.

  # Industry Standard: ALWAYS the raw sector-level proxy target (target_lookup),
  # regardless of whether the user customized their own goal above -- this is
  # deliberately independent of intake_target_pathway()'s use_sector_target choice.
  intake_industry_pathway <- reactive({
    b  <- intake_baseline()
    tl <- target_lookup %>% filter(primary_sector == input$intake_sector)
    req(nrow(tl) > 0, tl$target_year[1] > b$baseline_year)

    annual_rate <- 1 - (1 - tl$reduction_fraction[1])^(1 / (tl$target_year[1] - b$baseline_year))
    years <- b$baseline_year:tl$target_year[1]
    data.frame(year = years, target = b$baseline_value * (1 - annual_rate)^(years - b$baseline_year)) %>%
      mutate(annual_rate = annual_rate, reduction_fraction = tl$reduction_fraction[1], target_year = tl$target_year[1])
  })

  # SBTi source: try a direct, exact-match-only company-name lookup first
  # (same discipline as the pipeline's join -- no fuzzy matching); if the
  # typed company name doesn't match any real SBTi entry, fall back to the
  # sector-level SBTi aggregate (median across SBTi-committed peers in the
  # same sector) so a scenario is still available for comparison.
  intake_sbti_source <- reactive({
    req(input$intake_sector)

    direct <- NULL
    if (has_sbti_data && !is.null(sbti_company_lookup) && nzchar(input$intake_company_name)) {
      company_key <- normalize_company_name(input$intake_company_name)
      m <- sbti_company_lookup %>% filter(company_key == !!company_key)
      if (nrow(m) > 0) direct <- m
    }

    if (!is.null(direct)) {
      list(
        reduction_fraction = direct$target_value[1], target_year = direct$target_year[1],
        source_label = paste0("Direct match: ", direct$company_name[1])
      )
    } else if (has_sbti_data && !is.null(sbti_sector_benchmark)) {
      sec <- sbti_sector_benchmark %>% filter(primary_sector == input$intake_sector)
      if (nrow(sec) > 0) {
        list(
          reduction_fraction = sec$reduction_fraction[1], target_year = sec$target_year[1],
          source_label = paste0("Sector average (", sec$n_companies[1], " SBTi-committed companies in ", input$intake_sector, ")")
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  intake_sbti_pathway <- reactive({
    src <- intake_sbti_source()
    req(!is.null(src))
    b <- intake_baseline()
    req(src$target_year > b$baseline_year)

    annual_rate <- 1 - (1 - src$reduction_fraction)^(1 / (src$target_year - b$baseline_year))
    years <- b$baseline_year:src$target_year
    data.frame(year = years, target = b$baseline_value * (1 - annual_rate)^(years - b$baseline_year)) %>%
      mutate(annual_rate = annual_rate, reduction_fraction = src$reduction_fraction,
             target_year = src$target_year, source_label = src$source_label)
  })

  # Same forecast-selection logic as intake_gap() (own scaled forecast if
  # available, else the sector benchmark forecast) -- extracted so all 3
  # scenarios compare against the identical forecast, only the target differs.
  intake_forecast_series <- reactive({
    has_data <- isTRUE(input$intake_has_data == "yes") && !is.null(intake_user_data())
    fc <- NULL
    if (has_data) {
      uf <- intake_user_forecast()
      if (!is.null(uf) && nrow(uf) > 0) fc <- uf
    }
    if (is.null(fc)) fc <- intake_sector_forecast_benchmark() %>% rename(p50 = avg_p50)
    req(!is.null(fc), nrow(fc) > 0)
    fc %>% select(year, p50)
  })

  intake_gap_industry <- reactive({
    fc   <- intake_forecast_series()
    path <- intake_industry_pathway() %>% select(year, target)
    inner_join(fc, path, by = "year") %>% mutate(gap = p50 - target)
  })

  # ---- Portfolio Mix Engine (active, simple version) ----

  output$pme_facility_readout <- renderUI({
    facility_country <- input$intake_facility_country
    facility_state   <- input$intake_facility_state
    facility_county  <- input$intake_facility_county
    location_label <- if (is.null(facility_country) || !nzchar(facility_country)) {
      "not specified"
    } else if (identical(facility_country, "United States") && !is.null(facility_county) && nzchar(facility_county)) {
      paste0(facility_county, " County, ", facility_state, ", United States")
    } else if (identical(facility_country, "United States") && !is.null(facility_state) && nzchar(facility_state)) {
      paste0(facility_state, ", United States")
    } else {
      facility_country
    }
    tags$div(
      style = "font-size:12.5px; color:#7F8C8D; margin-bottom:6px;",
      tags$b("Facility: "),
      tags$span(style = "color:#2C3E50; font-weight:600;", location_label),
      tags$br(),
      tags$em("Set on the \"Company Profile\" tab.")
    )
  })

  output$pme_forward_discount_readout <- renderUI({
    discount <- input$pme_forward_discount
    if (is.null(discount) || discount == 0) return(NULL)
    tags$div(
      style = "background:#EAFAF1; border:1px solid #A9DFBF; border-radius:6px; padding:0.5rem 0.9rem; margin-bottom:10px; font-size:12.5px;",
      tags$b(discount, "% forward pricing discount applied. "),
      em("Every price below is spot price x ", 1 - discount / 100, " -- realized prices and total spend already reflect it.")
    )
  })

  output$pme_context <- renderUI({
    req(input$intake_sector)
    name <- if (nzchar(input$intake_company_name)) input$intake_company_name else "(unnamed company)"
    tagList(
      tags$b("Sizing portfolio for: "), tags$span(name),
      tags$br(),
      tags$b("Sector: "), tags$span(input$intake_sector)
    )
  })

  # Keep the year dropdown in sync with whatever years the multi-scope gap
  # covers -- was intake_gap() (Scope 1 only); now intake_gap_multiscope().
  # BUGFIX (was): intake_gap_multiscope()'s df$year spans a WIDER range
  # than the actual forecast horizon -- it also carries years from the
  # SBTi target's own range (e.g. 2015-2030) via left_join, even for
  # years where scope1/2/3_forecast is NA (no real forecast data). The
  # dropdown was offering those years too, defaulting to the max (e.g.
  # 2030) where every forecast column is NA -- and get_gap() below
  # treats a missing/NA value as "no gap" (0), so it silently showed
  # "0 t gap, 100% covered" for a year with NO forecast at all, not a
  # year where the target was genuinely met. Now restricted to years
  # where AT LEAST ONE scope actually has forecast data.
  observe({
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    valid <- df %>% filter(!is.na(scope1_forecast) | !is.na(scope2_forecast) | !is.na(scope3_forecast))
    yrs <- sort(unique(valid$year))
    req(length(yrs) > 0)
    updateSelectInput(session, "pme_year", choices = yrs, selected = max(yrs))
  })

  # Per-scope gap breakdown for the selected year/target scenario -- the
  # SAME single scenario choice (input$pme_gap_source) applies to all 3
  # scopes, per how this was scoped.
  pme_scope_gaps <- reactive({
    req(input$pme_year, input$pme_gap_source)
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    row <- df %>% filter(year == as.numeric(input$pme_year))
    req(nrow(row) > 0)

    col_suffix <- input$pme_gap_source  # "own" | "industry" | "sbti"
    fc_col <- function(scope_n) paste0("scope", scope_n, "_forecast")

    # "configured" distinguishes two genuinely different reasons a scope
    # can show 0: (a) this scenario truly isn't set up for this scope
    # (e.g. SBTi not configured, or Industry Target unavailable) -- a
    # real 0, no credits needed because there's nothing to compare
    # against; vs (b) the forecast itself is missing for this year --
    # NOT a real 0, just no data (the year dropdown above already
    # excludes years with zero forecast coverage, but a single scope can
    # still lack forecast data for a year another scope does have).
    get_gap <- function(scope_n) {
      col <- paste0("gap", scope_n, "_", col_suffix)
      fcol <- fc_col(scope_n)
      forecast_missing <- !(fcol %in% names(row)) || is.na(row[[fcol]][1])
      if (!(col %in% names(row)) || is.na(row[[col]][1])) {
        return(list(gap = 0, configured = !forecast_missing))
      }
      list(gap = max(row[[col]][1], 0), configured = TRUE)
    }

    g1 <- get_gap(1); g2 <- get_gap(2); g3 <- get_gap(3)
    data.frame(
      scope      = c("Scope 1", "Scope 2", "Scope 3"),
      gap_tons   = c(g1$gap, g2$gap, g3$gap),
      configured = c(g1$configured, g2$configured, g3$configured)
    )
  })

  # Combined 3-scope gap -- the ONE number that drives the shared budget
  # and the real LP optimizer (pce_alloc()) downstream. Was Scope 1 only
  # (intake_gap()/intake_gap_sbti()); now sums all 3 scopes for whichever
  # scenario is selected, per the shared-budget/shared-scenario design.
  pme_gap_tons <- reactive({
    sum(pme_scope_gaps()$gap_tons, na.rm = TRUE)
  })

  # ---- 5-Year Outlook -- aggregated gap and portfolio sizing across a
  # 5-year forward window, not just the single selected year. ----

  # Per-year, per-scope gap for the 5 consecutive years starting at
  # input$pme_year -- same scenario/combined-scope logic as
  # pme_scope_gaps(), just repeated across years instead of one. Years
  # with no forecast data for a given scope contribute 0 to that scope
  # (same "configured" distinction as the single-year view), and a year
  # with NO forecast data at all for any scope is dropped from the
  # window entirely (noted in the summary) rather than silently
  # counted as a real 0.
  pme_5yr_scope_gaps <- reactive({
    req(input$pme_year, input$pme_gap_source)
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)

    start_year <- as.numeric(input$pme_year)
    window_years <- start_year:(start_year + 4)
    col_suffix <- input$pme_gap_source

    rows <- lapply(window_years, function(yr) {
      row <- df %>% filter(year == yr)
      if (nrow(row) == 0) return(NULL)

      get_one <- function(scope_n) {
        col  <- paste0("gap", scope_n, "_", col_suffix)
        fcol <- paste0("scope", scope_n, "_forecast")
        forecast_val <- if (fcol %in% names(row) && !is.na(row[[fcol]][1])) max(row[[fcol]][1], 0) else NA_real_
        forecast_missing <- is.na(forecast_val)
        if (!(col %in% names(row)) || is.na(row[[col]][1])) {
          return(data.frame(year = yr, scope = paste0("Scope ", scope_n),
                             gap_tons = 0, forecast = forecast_val, configured = !forecast_missing))
        }
        data.frame(year = yr, scope = paste0("Scope ", scope_n),
                   gap_tons = max(row[[col]][1], 0), forecast = forecast_val, configured = TRUE)
      }
      bind_rows(get_one(1), get_one(2), get_one(3))
    })

    out <- bind_rows(rows)
    req(nrow(out) > 0)
    # Drop years where NOTHING was configured/forecast at all (every
    # scope's forecast missing) -- a year truly outside the forecast
    # horizon, not a real zero-gap year.
    year_has_any <- out %>% group_by(year) %>% summarise(any_ok = any(configured), .groups = "drop")
    valid_years <- year_has_any$year[year_has_any$any_ok]
    out %>% filter(year %in% valid_years)
  })

  # Per-scope TOTALS across the whole 5-year window (not per-year) -- the
  # 5-year equivalent of pme_scope_gaps(), used for the per-scope
  # breakdown cards and % decrease cards on the Long-Term tab.
  pme_scope_gaps_5yr_total <- reactive({
    pme_5yr_scope_gaps() %>%
      group_by(scope) %>%
      summarise(
        gap_tons   = sum(gap_tons, na.rm = TRUE),
        forecast   = sum(forecast, na.rm = TRUE),
        configured = any(configured),
        .groups = "drop"
      )
  })

  # Aggregate total gap across the whole 5-year window (all valid years,
  # all 3 scopes) -- the ONE number sizing the 5-year portfolio estimate.
  pme_5yr_total_gap <- reactive({
    sum(pme_5yr_scope_gaps()$gap_tons, na.rm = TRUE)
  })

  # 5-year allocation -- SAME real LP as the 1-year recommendation
  # (run_lp_alloc), just given the summed 5-year gap and 5x budget
  # instead of one year's numbers. See run_lp_alloc's own comment for
  # why this no longer uses a separate simplified engine. The one thing
  # that DOES differ from the 1-year call: a forward-pricing discount
  # (per the Jul 31 sync) -- a multi-year commitment can typically be
  # priced below spot, which genuinely buys more tons for the same
  # budget, not just a smaller number on paper.
  pme_5yr_alloc <- reactive({
    scope3_gap_5yr <- pme_scope_gaps_5yr_total() %>% filter(scope == "Scope 3") %>% pull(gap_tons)
    scope3_gap_5yr <- if (length(scope3_gap_5yr) > 0) scope3_gap_5yr[1] else 0
    run_lp_alloc(
      pme_5yr_total_gap(), input$pme_budget * 5, price_discount = input$pme_forward_discount / 100,
      scope3_gap_tons = scope3_gap_5yr
    )
  })

  pme_5yr_outcome <- reactive({
    alloc <- pme_5yr_alloc()
    gap_tons    <- attr(alloc, "gap_tons")
    funded_tons <- sum(alloc$funded_tons)
    funded_cost <- sum(alloc$funded_cost)
    coverage_pct <- if (gap_tons > 0) funded_tons / gap_tons * 100 else 100
    list(gap_tons = gap_tons, funded_tons = funded_tons, funded_cost = funded_cost, coverage_pct = coverage_pct)
  })

  # ---- INTERNAL demand-trend projection (per Avishkar, Jul 31 sync) ----
  # HONEST LIMITATION, stated once here rather than buried: the panel
  # model's own forecast (future_pred) genuinely covers only ~5 years.
  # There is no real 10-year model output to draw on. Rather than either
  # refuse the 10-year view entirely or quietly fabricate it, this fits a
  # simple linear trend to the REAL ~5-year gap total and extrapolates
  # that trend forward to fill years 6-10 -- clearly labeled wherever
  # it's shown as an extrapolation, not real model output. If a genuine
  # 10-year model forecast is built later, this function is the only
  # place that needs to change.
  pme_extrapolated_total_gap <- function(n_years) {
    base <- pme_5yr_scope_gaps() %>%
      group_by(year) %>% summarise(gap_tons = sum(gap_tons, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
    real_years <- base$year
    req(length(real_years) >= 1)
    start_year <- min(real_years)
    target_years <- start_year:(start_year + n_years - 1)

    if (length(real_years) < 2) {
      # Can't fit a trend from a single point -- hold flat rather than
      # guess a slope from nothing.
      flat_val <- base$gap_tons[1]
      return(list(total = flat_val * length(target_years), extrapolated = length(target_years) > 1))
    }

    fit <- lm(gap_tons ~ year, data = base)
    vals <- sapply(target_years, function(y) {
      if (y %in% real_years) base$gap_tons[base$year == y] else max(predict(fit, newdata = data.frame(year = y)), 0)
    })
    list(total = sum(vals), extrapolated = any(!target_years %in% real_years))
  }

  # "Ideal" (unconstrained) allocation at a given horizon -- the budget
  # passed is a placeholder large enough to never bind (req() inside
  # run_lp_alloc needs a real number, but ideal_tons/ideal_cost are
  # computed from run_lp_alloc's OWN internal unconstrained solve
  # regardless of what budget is passed in, so this placeholder never
  # actually affects the numbers used below).
  pme_internal_alloc <- function(n_years) {
    gap_info <- pme_extrapolated_total_gap(n_years)
    alloc <- run_lp_alloc(gap_info$total, budget = 1e12)
    attr(alloc, "extrapolated") <- gap_info$extrapolated
    alloc
  }

  # Bucket-level IDEAL demand share at a horizon -- what the model would
  # actually want to buy of each category if budget weren't limiting, not
  # what a real (budget-constrained) client would fund. This is the
  # "demand trend" signal: comparing this share at 5 years vs 10 years
  # shows which categories the model leans into MORE over time (growing
  # demand) vs less (phasing out), independent of any one client's
  # budget size.
  build_demand_rollup <- function(alloc) {
    bucket_labels <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )
    rollup <- alloc %>% group_by(key) %>% summarise(ideal_tons = sum(ideal_tons, na.rm = TRUE), .groups = "drop")
    tibble(key = names(bucket_labels)) %>%
      left_join(rollup, by = "key") %>%
      mutate(
        ideal_tons = coalesce(ideal_tons, 0),
        category   = bucket_labels[key],
        share_pct  = if (sum(ideal_tons) > 0) round(ideal_tons / sum(ideal_tons) * 100, 1) else 0
      ) %>%
      select(category, key, ideal_tons, share_pct)
  }

  pme_demand_5yr <- reactive({ pme_internal_alloc(5) })
  pme_demand_10yr <- reactive({ pme_internal_alloc(10) })

  pme_demand_comparison <- reactive({
    d5  <- build_demand_rollup(pme_demand_5yr())  %>% rename(ideal_tons_5 = ideal_tons, share_5 = share_pct)
    d10 <- build_demand_rollup(pme_demand_10yr()) %>% rename(ideal_tons_10 = ideal_tons, share_10 = share_pct)
    d5 %>%
      left_join(d10 %>% select(key, ideal_tons_10, share_10), by = "key") %>%
      mutate(share_delta = round(share_10 - share_5, 1)) %>%
      arrange(desc(share_10))
  })

  output$pme_demand_note <- renderUI({
    extrapolated_5  <- isTRUE(attr(pme_demand_5yr(), "extrapolated"))
    extrapolated_10 <- isTRUE(attr(pme_demand_10yr(), "extrapolated"))
    tagList(
      tags$div(
        style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:6px; padding:0.6rem 0.9rem; margin-bottom:10px; font-size:12.5px;",
        tags$b("Internal use -- not client-facing. "),
        "Shows IDEAL demand (unconstrained by any budget) by category, at 5 and 10 years, for this company's own gap trajectory only -- ",
        "not a market-wide/multi-client demand forecast, which this app has no data to build. ",
        if (extrapolated_5) "The 5-year figure uses real forecast data throughout. " else "",
        if (extrapolated_10) tags$b("The 10-year figure is a LINEAR EXTRAPOLATION beyond the model's real ~5-year forecast horizon -- not real model output.") else ""
      )
    )
  })

  output$pme_demand_plot <- renderPlot({
    df <- pme_demand_comparison() %>%
      select(category, share_5, share_10) %>%
      pivot_longer(cols = c(share_5, share_10), names_to = "horizon", values_to = "share") %>%
      mutate(horizon = ifelse(horizon == "share_5", "5-year", "10-year"),
             horizon = factor(horizon, levels = c("5-year", "10-year")),
             category = factor(category, levels = rev(pme_demand_comparison()$category)))

    ggplot(df, aes(x = category, y = share, fill = horizon)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      coord_flip() +
      scale_fill_manual(values = c("5-year" = "#5DADE2", "10-year" = "#1B4F72"), name = NULL) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
      labs(subtitle = "Ideal demand share by category -- 5yr vs 10yr horizon", x = NULL, y = "Share of ideal demand (%)") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
  })

  output$pme_demand_table <- renderDT({
    df <- pme_demand_comparison() %>%
      mutate(trend = ifelse(share_delta > 0.5, "\u2191 Growing", ifelse(share_delta < -0.5, "\u2193 Phasing out", "\u2192 Stable"))) %>%
      select(category, ideal_tons_5, share_5, ideal_tons_10, share_10, share_delta, trend)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      colnames = c("Category", "5yr Ideal Tons", "5yr Share (%)", "10yr Ideal Tons", "10yr Share (%)", "Share Change (pp)", "Trend")
    )
  })

  # NOTE: an earlier version of this app auto-rebalanced the other 4
  # sliders live whenever one moved (forcing the total to stay at 100).
  # Removed -- it fought the user's own dragging in real time (every touch
  # triggered 4 other sliders to jump), and it actively contradicted the
  # "these are relative weights, not required to sum to 100" framing below.
  # pme_weights() already normalizes silently; no live slider-jumping is
  # needed to make that true.

  pme_weights <- reactive({
    w <- c(
      nat_avoid    = input$pme_wt_nat_avoid,
      nat_removal  = input$pme_wt_nat_removal,
      tech_avoid   = input$pme_wt_tech_avoid,
      tech_removal = input$pme_wt_tech_removal,
      comm_avoid   = input$pme_wt_comm_avoid
    )
    w[is.na(w)] <- 0
    if (sum(w) == 0) w[] <- 20
    w / sum(w)
  })

  # Direct answer to "these don't sum to 100" -- shows the ACTUAL normalized
  # share each slider becomes, live, so the sliders never need to be
  # hand-tuned to add up to anything in particular.
  output$pme_normalized_readout <- renderUI({
    w <- pme_weights()
    labels <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )
    tags$div(
      style = "background:#F4F6F7; border-radius:6px; padding:0.5rem 0.75rem; margin-bottom:0.75rem; font-size:12.5px;",
      tags$b("Actual normalized mix (this is what the optimizer uses):"),
      tags$ul(
        style = "margin-bottom:0;",
        lapply(names(w), function(k) tags$li(labels[[k]], ": ", tags$b(scales::percent(w[[k]], accuracy = 0.1))))
      )
    )
  })

  # Live donut preview -- updates as the sliders move, so the mix is
  # SEEN (relative slice sizes), not just read as five numbers that
  # still need mentally converting into a sense of overall balance.
  output$pme_weight_donut <- renderPlot({
    w <- pme_weights()
    labels <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )
    df <- tibble(key = names(w), share = as.numeric(w)) %>%
      mutate(category = labels[key], category = factor(category, levels = labels[names(w)]))

    ggplot(df, aes(x = 2, y = share, fill = category)) +
      geom_col(color = "white", linewidth = 1) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      scale_fill_manual(values = pme_colors, name = NULL) +
      theme_void(base_size = 11) +
      theme(legend.position = "right", legend.text = element_text(size = 9))
  })

  # Prices are no longer exogenous/manually typed -- derived as a
  # supply-weighted average of the REAL per-methodology prices in the
  # 58-methodology catalog (catalog_rv()), one average per bucket.
  # Weighted by supply_tons rather than a plain mean, so a bucket
  # dominated by one high-supply, cheap methodology (e.g. REDD+ under
  # nat_avoid) isn't skewed by a handful of small, expensive niche entries.
  # Falls back to a plain mean if supply data is entirely missing for
  # that bucket (should not happen with the current catalog, but kept
  # as a safety net rather than silently producing NA/0).
  pme_prices <- reactive({
    catalog <- catalog_rv()
    bucket_keys <- c("nat_avoid", "nat_removal", "tech_avoid", "tech_removal", "comm_avoid")

    wavg <- catalog %>%
      group_by(key) %>%
      summarise(
        total_supply = sum(supply_tons, na.rm = TRUE),
        wavg_price   = if (sum(supply_tons, na.rm = TRUE) > 0) {
          sum(buyer_price * supply_tons, na.rm = TRUE) / sum(supply_tons, na.rm = TRUE)
        } else {
          mean(buyer_price, na.rm = TRUE)
        },
        .groups = "drop"
      )

    out <- setNames(rep(NA_real_, length(bucket_keys)), bucket_keys)
    out[wavg$key] <- wavg$wavg_price
    out
  })

  # Ideal (full-gap) recipe at the target mix, priced out, then scaled down
  # uniformly to fit budget -- the exact mechanism just walked through:
  # the ratio between categories never changes, only the total size does.
  # Recommended Credit Portfolio (the bucket-level chart/table below) is now
  # a ROLLUP of the Portfolio Curation Engine's real per-methodology
  # optimization (pce_alloc()) -- grouped back up into the 5 buckets for
  # display -- rather than an independent fixed-ratio calculation using
  # flat bucket-level prices. This fixes the disagreement between the two
  # tabs: there is now only one real calculation (the 58-methodology LP,
  # with real per-methodology prices and supply caps); this tab just
  # displays it summarized. price_per_ton below is a DERIVED realized
  # average (total cost / total tons actually funded in that bucket), not
  # an input -- editing the "Price assumptions" numbers further down no
  # longer changes this chart at all (they now only drive the separate
  # "Three-scenario comparison" section, which still uses its own
  # simpler, non-LP costing logic).
  # Shared bucket-level rollup from an LP allocation (pce_alloc() or
  # pme_5yr_alloc()) -- used by both the 1-year and 5-year mix views so
  # they're built from identical logic, just fed different allocations.
  build_mix_rollup <- function(alloc) {
    bucket_labels <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )

    rollup <- alloc %>%
      group_by(key) %>%
      summarise(funded_tons = sum(funded_tons, na.rm = TRUE), funded_cost = sum(funded_cost, na.rm = TRUE), .groups = "drop")

    tibble(key = names(bucket_labels)) %>%
      left_join(rollup, by = "key") %>%
      mutate(
        funded_tons   = coalesce(funded_tons, 0),
        funded_cost   = coalesce(funded_cost, 0),
        category      = bucket_labels[key],
        price_per_ton = ifelse(funded_tons > 0, round(funded_cost / funded_tons, 2), NA_real_),
        weight_pct    = if (sum(funded_tons) > 0) round(funded_tons / sum(funded_tons) * 100, 1) else 0
      ) %>%
      mutate(spend_share_pct = if (sum(funded_cost) > 0) round(funded_cost / sum(funded_cost) * 100, 1) else 0) %>%
      select(category, key, weight_pct, price_per_ton, funded_tons, funded_cost, spend_share_pct)
  }

  pme_mix <- reactive({
    build_mix_rollup(pce_alloc())
  })

  pme_mix_5yr <- reactive({
    build_mix_rollup(pme_5yr_alloc())
  })

  # ---- 3-scenario comparison: same portfolio mix/prices/budget, sized
  # against 3 different targets (Current Goal / Industry Standard / SBTi) ----

  pme_scenario_gaps <- reactive({
    req(input$pme_year)
    yr <- as.numeric(input$pme_year)

    # Combined 3-scope gap per scenario -- was Scope 1 only via
    # intake_gap()/intake_gap_industry()/intake_gap_sbti(); now sums
    # gap1/2/3 for each scenario from intake_gap_multiscope(), so this
    # comparison stays consistent with the main summary above (which
    # sums the same 3 scopes for whichever ONE scenario is selected).
    combined_gap_for <- function(col_suffix) {
      tryCatch({
        df <- intake_gap_multiscope()
        row <- df %>% filter(year == yr)
        if (nrow(row) == 0) return(NA_real_)
        cols <- paste0("gap", 1:3, "_", col_suffix)
        vals <- sapply(cols, function(cl) if (cl %in% names(row) && !is.na(row[[cl]][1])) max(row[[cl]][1], 0) else 0)
        sum(vals)
      }, error = function(e) NA_real_)
    }

    own_gap      <- combined_gap_for("own")
    industry_gap <- combined_gap_for("industry")
    sbti_gap     <- combined_gap_for("sbti")

    sbti_label <- tryCatch({
      src <- intake_sbti_source()
      if (!is.null(src)) src$source_label else "No SBTi data available for this sector"
    }, error = function(e) "No SBTi data available for this sector")

    data.frame(
      scenario  = c("Your Current Goal", "Industry Target", "SBTi-Recommended"),
      gap_tons  = c(own_gap, industry_gap, sbti_gap),
      detail    = c("As set on New Company Intake", "SBTi methodology run on the industry benchmark", sbti_label),
      stringsAsFactors = FALSE
    )
  })

  pme_scenario_comparison <- reactive({
    gaps   <- pme_scenario_gaps()
    w      <- pme_weights()
    prices <- pme_prices()
    budget <- input$pme_budget
    req(!is.na(budget), budget >= 0)

    outcomes <- lapply(gaps$gap_tons, compute_portfolio_outcome, weights = w, prices = prices, budget = budget)

    gaps %>%
      mutate(
        funded_tons  = sapply(outcomes, function(o) o$funded_tons),
        coverage_pct = sapply(outcomes, function(o) round(o$coverage_pct, 1)),
        funded_cost  = sapply(outcomes, function(o) o$funded_cost)
      )
  })

  output$pme_scenario_table <- renderDT({
    df <- pme_scenario_comparison() %>%
      mutate(
        gap_tons_fmt     = ifelse(is.na(gap_tons), "--", comma(round(gap_tons))),
        funded_tons_fmt  = ifelse(is.na(funded_tons), "--", comma(round(funded_tons))),
        coverage_pct_fmt = ifelse(is.na(coverage_pct), "--", paste0(coverage_pct, "%")),
        funded_cost_fmt  = ifelse(is.na(funded_cost), "--", paste0("$", comma(round(funded_cost))))
      ) %>%
      select(scenario, detail, gap_tons_fmt, funded_tons_fmt, coverage_pct_fmt, funded_cost_fmt)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      colnames = c("Scenario", "Target Source", "Gap (tCO2e)", "Tons Funded", "% of Gap Covered", "Est. Spend")
    )
  })

  output$pme_scenario_plot <- renderPlot({
    df <- pme_scenario_comparison() %>% filter(!is.na(gap_tons))
    req(nrow(df) > 0)
    df <- df %>% mutate(scenario = factor(scenario, levels = c("Your Current Goal", "Industry Target", "SBTi-Recommended")))

    plot_df <- bind_rows(
      df %>% transmute(scenario, tons = gap_tons, type = "Gap (needed)"),
      df %>% transmute(scenario, tons = funded_tons, type = "Funded by this portfolio")
    )

    ggplot(plot_df, aes(x = scenario, y = tons, fill = type)) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Gap (needed)" = "#C0392B", "Funded by this portfolio" = "#27AE60"), name = NULL) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(
        subtitle = "Same mix, prices, and budget -- sized against 3 different targets",
        x = NULL, y = "Tons (tCO2e)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
  })

  # Shared summary-card builder -- used by BOTH the 1-year (pme_summary)
  # and 5-year (pme_5yr_full_summary) views so they're structurally
  # IDENTICAL, not two different layouts. Only the data fed in differs:
  # mix_df/gap_tons/budget/scope_gaps_df/pct_result for one year vs.
  # summed across the 5-year window.
  build_portfolio_summary_ui <- function(mix_df, gap_tons, budget, scope_gaps_df, pct_result, period_label, no_gap_note, alloc = NULL) {
    total_tons <- sum(mix_df$funded_tons)
    total_cost <- sum(mix_df$funded_cost)
    coverage_raw <- if (gap_tons > 0) total_tons / gap_tons * 100 else 100
    coverage <- if (coverage_raw > 0 && coverage_raw < 1) round(coverage_raw, 3) else round(coverage_raw, 1)

    # Capital allocation suggestion (per Avishkar, Jul 31 sync): what
    # budget would ACTUALLY be needed to fully cover the gap, not just
    # what the current budget buys. Reuses ideal_cost -- already
    # computed inside run_lp_alloc()'s unconstrained (budget=NULL) solve
    # for a different purpose (development-priority ranking) -- rather
    # than running a second LP solve. Same catalog, weights, and
    # concentration caps as the real recommendation; the only thing
    # relaxed is the budget ceiling itself, so this is the actual
    # cheapest-available-supply cost to close the gap, not a rough
    # multiplier estimate.
    capital_needed <- if (!is.null(alloc)) sum(alloc$ideal_cost) else NA_real_

    stat_card <- function(value, label, accent = "#2C3E50") {
      tags$div(
        style = "background:#F4F6F7; border-radius:8px; padding:0.85rem 1rem; text-align:center;",
        tags$div(style = paste0("font-size:20px; font-weight:700; color:", accent, "; white-space:nowrap;"), value),
        tags$div(style = "font-size:11.5px; color:#7F8C8D; margin-top:2px;", label)
      )
    }

    coverage_color <- if (coverage_raw >= 100) "#1E8449" else if (coverage_raw >= 50) "#B7950B" else "#C0392B"

    status_note <- if (gap_tons > 0 && coverage_raw < 100) {
      tags$div(
        style = "background:#FDEBD0; border-radius:6px; padding:0.5rem 0.8rem; font-size:12.5px; margin-top:10px;",
        em(
          "Budget doesn't cover the full gap at this mix and these prices -- increase the budget, ",
          "shift the tilt toward cheaper categories, or spread purchases across more years."
        )
      )
    } else if (gap_tons > 0) {
      tags$div(
        style = "background:#EAFAF1; border-radius:6px; padding:0.5rem 0.8rem; font-size:12.5px; margin-top:10px;",
        em("Budget fully covers the gap at this mix and these prices; any remainder is unallocated.")
      )
    } else {
      tags$div(
        style = "background:#EBF5FB; border-radius:6px; padding:0.5rem 0.8rem; font-size:12.5px; margin-top:10px;",
        em(no_gap_note)
      )
    }

    scope_accent <- c("Scope 1" = "#C0392B", "Scope 2" = "#2980B9", "Scope 3" = "#8E44AD")

    stat_row <- function(label, value) {
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:baseline; padding:4px 0; border-bottom:1px solid #F0F1F1;",
        tags$span(style = "font-size:12px; color:#7F8C8D;", label),
        tags$span(style = "font-size:14px; font-weight:600; color:#2C3E50; white-space:nowrap; margin-left:10px;", value)
      )
    }

    scope_cards <- lapply(seq_len(nrow(scope_gaps_df)), function(i) {
      sg          <- scope_gaps_df$gap_tons[i]
      configured  <- scope_gaps_df$configured[i]
      nm          <- scope_gaps_df$scope[i]
      share       <- if (gap_tons > 0) sg / gap_tons else 0
      accent      <- scope_accent[[nm]]

      card_body <- if (!configured) {
        scenario_label <- switch(input$pme_gap_source, industry = "Industry Target", sbti = "SBTi", "Own Goal")
        tags$div(
          style = "font-size:12px; color:#7F8C8D; padding:8px 0;",
          em(scenario_label, " not configured for this scope -- excluded from the combined gap.")
        )
      } else {
        tagList(
          stat_row("Gap", paste0(comma(round(sg)), " t")),
          stat_row("Budget share", paste0("$", comma(round(budget * share)))),
          stat_row("Tons funded", paste0(comma(round(total_tons * share)), " t")),
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:baseline; padding:4px 0;",
            tags$span(style = "font-size:12px; color:#7F8C8D;", "Spend"),
            tags$span(style = "font-size:14px; font-weight:600; color:#2C3E50; white-space:nowrap; margin-left:10px;",
                       paste0("$", comma(round(total_cost * share))))
          )
        )
      }

      tags$div(
        style = paste0(
          "background:#FFFFFF; border:1px solid #E5E8E8; border-left:4px solid ", accent, "; ",
          "border-radius:8px; padding:0.75rem 1rem;"
        ),
        tags$div(style = paste0("font-size:13px; font-weight:700; color:", accent, "; margin-bottom:6px;"), nm),
        card_body
      )
    })

    pct_decrease_fmt <- if (is.null(pct_result) || is.na(pct_result$total_pct)) "N/A" else paste0(round(pct_result$total_pct, 1), "%")

    pct_scope_cards <- NULL
    if (!is.null(pct_result)) {
      ps <- pct_result$per_scope
      pct_scope_cards <- lapply(seq_len(nrow(ps)), function(i) {
        r <- ps[i, ]
        accent <- scope_accent[[r$scope]]
        if (!r$configured) {
          stat_card("N/A", paste0(r$scope, " -- not configured for this scenario"), accent = "#7F8C8D")
        } else if (is.na(r$pct_decrease)) {
          stat_card("N/A", paste0(r$scope, " -- no forecast data"), accent = "#7F8C8D")
        } else {
          stat_card(
            paste0(round(r$pct_decrease, 1), "%"),
            paste0(r$scope, " -- ", comma(round(r$funded_tons)), " t / ", comma(round(r$forecast)), " t forecast"),
            accent = accent
          )
        }
      })
    }
    if (is.null(pct_scope_cards)) {
      pct_scope_cards <- lapply(c("Scope 1", "Scope 2", "Scope 3"), function(nm) {
        stat_card("N/A", paste0(nm, " -- unavailable"), accent = "#7F8C8D")
      })
    }

    tagList(
      tags$div(
        style = "display:grid; grid-template-columns:repeat(4, 1fr); gap:12px; margin-bottom:12px;",
        stat_card(paste0(comma(round(gap_tons)), " t"), paste0("Combined gap to cover (", period_label, ")")),
        stat_card(paste0("$", comma(budget)), "Budget"),
        stat_card(paste0(comma(total_tons), " t"), paste0(coverage, "% of gap covered"), accent = coverage_color),
        stat_card(paste0("$", comma(total_cost)), "Estimated spend"),
        stat_card(pct_decrease_fmt, paste0("Combined % decrease (", period_label, ")"), accent = "#1E8449"),
        pct_scope_cards[[1]], pct_scope_cards[[2]], pct_scope_cards[[3]]
      ),
      if (!is.na(capital_needed)) {
        tags$div(
          style = "background:#EBF5FB; border:1px solid #AED6F1; border-radius:8px; padding:0.7rem 1rem; margin-bottom:12px;",
          tags$b("Capital needed to fully close this gap: "),
          tags$span(style = "font-size:16px; font-weight:700; color:#1B4F72;", paste0("$", comma(round(capital_needed))))
        )
      },
      # Uncovered emissions visualization -- per Rajat's request (Aug 6
      # sync): show explicitly which PORTION of the gap the current
      # recommendation leaves unaddressed, not just a "% covered" number
      # buried in a stat card. A single segmented bar: covered (green)
      # vs. uncovered (red), to scale, so the size of what's left
      # reads visually, not just numerically.
      if (gap_tons > 0) {
        covered_pct <- min(coverage_raw, 100)
        uncovered_pct <- max(100 - covered_pct, 0)
        uncovered_tons <- max(gap_tons - total_tons, 0)
        tags$div(
          style = "margin-bottom:12px;",
          tags$div(
            style = "font-size:12.5px; color:#5D6D7E; margin-bottom:4px; display:flex; justify-content:space-between;",
            tags$span(tags$b(comma(round(total_tons)), " t covered")),
            tags$span(tags$b(comma(round(uncovered_tons)), " t uncovered"))
          ),
          tags$div(
            style = "display:flex; height:22px; border-radius:4px; overflow:hidden; border:1px solid #D5D8DC;",
            tags$div(style = paste0("width:", covered_pct, "%; background:#27AE60;")),
            tags$div(style = paste0("width:", uncovered_pct, "%; background:#E74C3C;"))
          )
        )
      },
      # Recommend additional projects/locations to close any remaining
      # deficit -- per Rajat's request (Aug 6 sync). Not a generic "add
      # budget" message: actually looks at what's constraining THIS
      # recommendation (remaining unused supply by bucket, proximity to
      # the facility) so the suggestion is specific to what would
      # actually help, not boilerplate.
      if (!is.null(alloc) && gap_tons > total_tons) {
        remaining_by_bucket <- alloc %>%
          mutate(remaining_supply = pmax(coalesce(supply_tons, 0) - funded_tons, 0)) %>%
          filter(remaining_supply > 0) %>%
          group_by(key) %>%
          summarise(remaining_supply = sum(remaining_supply), min_price = min(buyer_price, na.rm = TRUE), .groups = "drop") %>%
          arrange(min_price)

        bucket_labels <- c(
          nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
          tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
          comm_avoid = "Community-based avoidance"
        )

        recs <- list()
        if (nrow(remaining_by_bucket) > 0) {
          cheapest <- remaining_by_bucket[1, ]
          recs <- c(recs, list(paste0(
            "Cheapest unused supply is in ", bucket_labels[[cheapest$key]], " (from $",
            round(cheapest$min_price), "/t, ", comma(round(cheapest$remaining_supply)),
            " t still available in the catalog) -- raising the bucket cap or preference weight for this ",
            "category would let the optimizer use more of it."
          )))
        }
        no_supply_left <- setdiff(names(bucket_labels), remaining_by_bucket$key)
        if (length(no_supply_left) > 0) {
          recs <- c(recs, list(paste0(
            "No unused catalog supply left in: ", paste(bucket_labels[no_supply_left], collapse = ", "),
            " -- closing more of the gap in these categories needs new project supply added to the catalog, not just a bigger budget."
          )))
        }
        recs <- c(recs, list(paste0(
          "Or: increasing the budget by ~$", comma(round(max(capital_needed - total_cost, 0))),
          " would fund the remaining gap at current prices (see \"Capital needed\" above)."
        )))

        tags$div(
          style = "background:#FEF9E7; border:1px solid #F7DC6F; border-radius:8px; padding:0.7rem 1rem; margin-bottom:12px; font-size:12.5px;",
          tags$b("To close the remaining gap:"),
          tags$ul(style = "margin-bottom:0; margin-top:4px;", lapply(recs, tags$li))
        )
      },
      tags$div(
        style = "margin-top:12px; padding-top:10px; border-top:1px solid #E5E8E8;",
        tags$div(style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(230px, 1fr)); gap:14px;", scope_cards)
      )
    )
  }

  output$pme_summary <- renderUI({
    build_portfolio_summary_ui(
      pme_mix(), pme_gap_tons(), input$pme_budget,
      pme_scope_gaps(), tryCatch(pme_pct_decrease(), error = function(e) NULL),
      input$pme_year, "No credits needed this year -- forecast is already at or below target.",
      alloc = tryCatch(pce_alloc(), error = function(e) NULL)
    )
  })

  output$pme_5yr_full_summary <- renderUI({
    scope_gaps <- pme_scope_gaps_5yr_total()
    years <- sort(unique(pme_5yr_scope_gaps()$year))
    period_label <- if (length(years) > 0) paste0(min(years), "-", max(years)) else "5yr"
    build_portfolio_summary_ui(
      pme_mix_5yr(), pme_5yr_total_gap(), input$pme_budget * 5,
      scope_gaps, tryCatch(pme_pct_decrease_5yr(), error = function(e) NULL),
      period_label, "No credits needed across this window -- forecast is already at or below target.",
      alloc = tryCatch(pme_5yr_alloc(), error = function(e) NULL)
    )
  })

  # ---- % Decrease tab -- per the meeting decision to add a percentage-
  # framed view alongside the tons/dollars Summary tab. This is a
  # DIFFERENT percentage than "% of gap covered" on the Summary tab: that
  # one measures the portfolio against the GAP (forecast minus target);
  # this one measures it against each scope's FULL forecast emissions --
  # "what fraction of my actual footprint does this portfolio offset,"
  # not "what fraction of the shortfall does it close." Both are real,
  # different questions -- kept as two separate numbers rather than
  # conflated into one. ----
  # Shared % decrease calc -- what fraction of EACH scope's forecast (not
  # just the gap) the funded tons offset, proportionally split by each
  # scope's share of the combined gap. Used by both the 1-year and
  # 5-year views.
  build_pct_decrease <- function(scope_gaps_df, total_tons, total_gap_tons) {
    per_scope <- scope_gaps_df %>%
      mutate(
        share        = ifelse(total_gap_tons > 0, gap_tons / total_gap_tons, 0),
        funded_tons  = total_tons * share,
        pct_decrease = ifelse(configured & !is.na(forecast) & forecast > 0, funded_tons / forecast * 100, NA_real_)
      )
    total_forecast <- sum(scope_gaps_df$forecast, na.rm = TRUE)
    total_pct <- if (total_forecast > 0) total_tons / total_forecast * 100 else NA_real_
    list(per_scope = per_scope, total_forecast = total_forecast, total_pct = total_pct, total_tons = total_tons)
  }

  pme_pct_decrease <- reactive({
    req(input$pme_year)
    scope_gaps <- pme_scope_gaps()
    mix        <- pme_mix()
    gap_tons   <- pme_gap_tons()

    yr <- as.numeric(input$pme_year)
    df <- tryCatch(intake_gap_multiscope(), error = function(e) NULL)
    req(!is.null(df), nrow(df) > 0)
    row <- df %>% filter(year == yr)
    req(nrow(row) > 0)

    get_forecast <- function(scope_n) {
      col <- paste0("scope", scope_n, "_forecast")
      if (!(col %in% names(row)) || is.na(row[[col]][1])) return(NA_real_)
      max(row[[col]][1], 0)
    }
    scope_gaps$forecast <- c(get_forecast(1), get_forecast(2), get_forecast(3))

    build_pct_decrease(scope_gaps, sum(mix$funded_tons), gap_tons)
  })

  pme_pct_decrease_5yr <- reactive({
    scope_gaps <- pme_scope_gaps_5yr_total()
    mix        <- pme_mix_5yr()
    gap_tons   <- pme_5yr_total_gap()
    build_pct_decrease(scope_gaps, sum(mix$funded_tons), gap_tons)
  })

  output$pme_pct_summary <- renderUI({
    result <- pme_pct_decrease()
    ps     <- result$per_scope

    stat_card <- function(value, label, accent = "#2C3E50") {
      tags$div(
        style = "background:#F4F6F7; border-radius:8px; padding:0.85rem 1rem; text-align:center;",
        tags$div(style = paste0("font-size:20px; font-weight:700; color:", accent, "; white-space:nowrap;"), value),
        tags$div(style = "font-size:11.5px; color:#7F8C8D; margin-top:2px;", label)
      )
    }

    total_pct_fmt <- if (is.na(result$total_pct)) "N/A" else paste0(round(result$total_pct, 1), "%")

    scope_accent <- c("Scope 1" = "#C0392B", "Scope 2" = "#2980B9", "Scope 3" = "#8E44AD")
    scope_cards <- lapply(seq_len(nrow(ps)), function(i) {
      r <- ps[i, ]
      accent <- scope_accent[[r$scope]]
      body <- if (!r$configured) {
        tags$div(style = "font-size:12px; color:#7F8C8D; padding:8px 0;",
                 em("Not configured for this scenario."))
      } else if (is.na(r$pct_decrease)) {
        tags$div(style = "font-size:12px; color:#7F8C8D; padding:8px 0;",
                 em("No forecast data available for this year."))
      } else {
        tagList(
          tags$div(style = "font-size:26px; font-weight:700; color:#2C3E50;", paste0(round(r$pct_decrease, 1), "%")),
          tags$div(style = "font-size:11.5px; color:#7F8C8D;",
                   comma(round(r$funded_tons)), " t offsetting ", comma(round(r$forecast)), " t forecast")
        )
      }
      tags$div(
        style = paste0("background:#FFFFFF; border:1px solid #E5E8E8; border-left:4px solid ", accent,
                        "; border-radius:8px; padding:0.75rem 1rem; text-align:center;"),
        tags$div(style = paste0("font-size:13px; font-weight:700; color:", accent, "; margin-bottom:6px;"), r$scope),
        body
      )
    })

    tagList(
      tags$div(
        style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(150px, 1fr)); gap:12px; margin-bottom:16px;",
        stat_card(paste0(comma(round(result$total_tons)), " t"), "Total tons funded"),
        stat_card(paste0(comma(round(result$total_forecast)), " t"), "Combined forecast (all 3 scopes)")
      ),
      tags$div(style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(200px, 1fr)); gap:14px;", scope_cards)
    )
  })

  output$pme_pct_plot <- renderPlot({
    ps <- pme_pct_decrease()$per_scope %>% filter(configured, !is.na(pct_decrease))
    req(nrow(ps) > 0)

    p <- ggplot(ps, aes(x = scope, y = pct_decrease, fill = scope)) +
      geom_col(width = 0.55) +
      geom_text(aes(label = paste0(round(pct_decrease, 1), "%")), vjust = -0.4, size = 4) +
      scale_fill_manual(values = c("Scope 1" = "#C0392B", "Scope 2" = "#2980B9", "Scope 3" = "#8E44AD"), guide = "none") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
      labs(subtitle = "% of forecast emissions offset by this portfolio, per scope", x = NULL, y = "% decrease") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
    p
  })

  pme_colors <- c(
    "Nature-based avoidance"     = "#7FB069",
    "Nature-based removal"       = "#2D6A4F",
    "Technology-based avoidance" = "#4A90A4",
    "Technology-based removal"   = "#264653",
    "Community-based avoidance"  = "#E8A33D"
  )

  # Shared tons/spend/table builders -- used by both the 1-year and
  # 5-year mix views so they're built identically, just from different
  # mix data (pme_mix() vs pme_mix_5yr()).
  build_tons_plot <- function(df) {
    req(nrow(df) > 0, sum(df$funded_tons) > 0)
    df <- df %>% filter(funded_tons > 0)
    req(nrow(df) > 0)
    df <- df %>% mutate(category = factor(category, levels = category[order(funded_tons)]))

    ggplot(df, aes(category, funded_tons, fill = category)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = comma(funded_tons)), hjust = -0.15, size = 3.5) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      scale_fill_manual(values = pme_colors, guide = "none") +
      labs(subtitle = "Tons recommended by category (tCO2e)", x = NULL, y = "Tons") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11))
  }

  build_spend_plot <- function(df) {
    req(nrow(df) > 0, sum(df$funded_cost) > 0)
    df <- df %>% filter(funded_cost > 0) %>% mutate(category = factor(category, levels = category[order(-funded_cost)]))

    ggplot(df, aes(x = "", y = funded_cost, fill = category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(spend_share_pct, "%")), position = position_stack(vjust = 0.5), size = 4, color = "white", fontface = "bold") +
      scale_fill_manual(values = pme_colors, name = NULL) +
      labs(subtitle = "Spend by category ($) -- this is where the budget is actually going") +
      theme_void(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11, hjust = 0.5), legend.position = "bottom")
  }

  # Category table with PROJECT sub-rows -- each category's summary row
  # (bold, shaded) is immediately followed by the specific funded
  # projects that make it up (indented, smaller), so the rollup and what
  # actually drove it are in one table instead of needing to cross-
  # reference the separate project-level table. alloc is the underlying
  # per-project allocation (pce_alloc() or pme_5yr_alloc()) the mix_df
  # rollup (df) was itself built from -- same source, just two views of it.
  build_mix_table <- function(df, alloc) {
    row_blocks <- lapply(seq_len(nrow(df)), function(i) {
      cat_row <- data.frame(
        row_type   = "category",
        category   = df$category[i],
        weight_pct = as.character(df$weight_pct[i]),
        price      = if (is.na(df$price_per_ton[i])) "" else as.character(df$price_per_ton[i]),
        tons       = comma(df$funded_tons[i]),
        cost       = comma(df$funded_cost[i]),
        spend_pct  = as.character(df$spend_share_pct[i]),
        stringsAsFactors = FALSE
      )

      projects <- alloc %>%
        filter(key == df$key[i], funded_tons > 0) %>%
        arrange(desc(funded_tons))

      if (nrow(projects) == 0) return(cat_row)

      proj_rows <- data.frame(
        row_type   = "project",
        category   = paste0("\u2001\u2001", projects$project_type),
        weight_pct = "",
        price      = as.character(projects$buyer_price),
        tons       = comma(projects$funded_tons),
        cost       = comma(projects$funded_cost),
        spend_pct  = "",
        stringsAsFactors = FALSE
      )
      bind_rows(cat_row, proj_rows)
    })

    full_df <- bind_rows(row_blocks)

    datatable(
      full_df,
      options = list(pageLength = 50, dom = "t", columnDefs = list(list(visible = FALSE, targets = 0))),
      rownames = FALSE,
      colnames = c("_row_type", "Category / Project", "Actual Mix (%, by tons)", "Realized Price ($/ton)",
                   "Tons Recommended", "Est. Cost ($)", "Share of Spend (%)")
    ) %>%
      formatStyle(
        "row_type", target = "row",
        fontWeight = styleEqual("category", "bold"),
        backgroundColor = styleEqual("category", "#F4F6F7"),
        color = styleEqual("project", "#5D6D7E")
      )
  }

  output$pme_tons_plot <- renderPlot({ build_tons_plot(pme_mix()) })

  # NOTE: kept as a static ggplot (renderPlot), not converted to plotly --
  # ggplotly() does not render coord_polar (pie/donut) charts correctly,
  # a known plotly limitation, so converting this one would have shipped
  # a visibly broken chart rather than a hover-interactive improvement.
  output$pme_spend_plot <- renderPlot({ build_spend_plot(pme_mix()) })

  output$pme_table <- renderDT({ build_mix_table(pme_mix(), pce_alloc()) })

  # ---- 5-year mirrors of the above -- SAME structure as the 1-year tab,
  # fed pme_mix_5yr() (rolled up from pme_5yr_alloc(), the real LP run
  # against the summed 5-year gap and 5x budget) instead of pme_mix(). ----
  output$pme_5yr_tons_plot <- renderPlot({ build_tons_plot(pme_mix_5yr()) })
  output$pme_5yr_spend_plot <- renderPlot({ build_spend_plot(pme_mix_5yr()) })
  output$pme_5yr_category_table <- renderDT({ build_mix_table(pme_mix_5yr(), pme_5yr_alloc()) })

  # ---- 5-Year Outlook outputs ----

  output$pme_5yr_summary <- renderUI({
    scope_gaps <- pme_5yr_scope_gaps()
    total_gap  <- pme_5yr_total_gap()
    outcome    <- pme_5yr_outcome()
    budget5    <- input$pme_budget * 5
    years      <- sort(unique(scope_gaps$year))

    coverage_raw <- if (!is.na(outcome$coverage_pct)) outcome$coverage_pct else 100
    coverage <- if (coverage_raw > 0 && coverage_raw < 1) round(coverage_raw, 3) else round(coverage_raw, 1)
    coverage_color <- if (coverage_raw >= 100) "#1E8449" else if (coverage_raw >= 50) "#B7950B" else "#C0392B"

    stat_card <- function(value, label, accent = "#2C3E50") {
      tags$div(
        style = "background:#F4F6F7; border-radius:8px; padding:0.85rem 1rem; text-align:center;",
        tags$div(style = paste0("font-size:20px; font-weight:700; color:", accent, "; white-space:nowrap;"), value),
        tags$div(style = "font-size:11.5px; color:#7F8C8D; margin-top:2px;", label)
      )
    }

    year_note <- if (length(years) < 5) {
      tags$div(
        style = "background:#FDEBD0; border-radius:6px; padding:0.5rem 0.8rem; font-size:12.5px; margin-top:10px;",
        em("Only ", length(years), " of 5 years (", paste(range(years), collapse = "-"), ") have forecast ",
           "data available for this scenario -- the window was truncated rather than treating missing years as zero-gap.")
      )
    } else NULL

    tagList(
      tags$div(
        style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(150px, 1fr)); gap:12px; margin-bottom:6px;",
        stat_card(paste0(comma(round(total_gap)), " t"), paste0("Combined gap, ", min(years), "-", max(years))),
        stat_card(paste0("$", comma(budget5)), "5-year budget (annual x 5)"),
        stat_card(paste0(comma(round(outcome$funded_tons)), " t"), paste0(coverage, "% of 5yr gap covered"), accent = coverage_color),
        stat_card(paste0("$", comma(round(outcome$funded_cost))), "Estimated 5yr spend")
      ),
      year_note
    )
  })

  output$pme_5yr_plot <- renderPlot({
    df <- pme_5yr_scope_gaps() %>% filter(configured)
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = factor(year), y = gap_tons, fill = scope)) +
      geom_col(position = "stack", width = 0.65) +
      scale_fill_manual(values = c("Scope 1" = "#C0392B", "Scope 2" = "#2980B9", "Scope 3" = "#8E44AD"), name = NULL) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(subtitle = "Gap to cover per year, stacked by scope", x = NULL, y = "Gap (tCO2e)") +
      theme_minimal(base_size = 13) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "top")
    p
  })

  output$pme_5yr_table <- renderDT({
    df <- pme_5yr_scope_gaps() %>%
      mutate(gap_tons = round(gap_tons)) %>%
      pivot_wider(id_cols = year, names_from = scope, values_from = gap_tons, values_fill = 0) %>%
      arrange(year)

    scope_cols <- setdiff(names(df), "year")
    df <- df %>% mutate(Total = rowSums(across(all_of(scope_cols))))

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      colnames = c("Year", scope_cols, "Total (tCO2e)")
    )
  })

  # ---- Portfolio Curation Engine (methodology-level) ----
  # Reuses the SAME gap, budget, and bucket weights already set on the
  # "Portfolio Mix Engine" tab (pme_gap_tons()/input$pme_budget/pme_weights())
  # -- this is the granular, per-methodology view of that same underlying
  # problem, not a separate/disconnected tool. The bucket weights become
  # the LP's per-project objective weight (via each project's `key`), so a
  # project's own price/supply/margin determines exactly how much of it
  # gets recommended within its bucket's share of the budget.

  # Shared LP allocation logic -- used for BOTH the 1-year (pce_alloc) and
  # 5-year (pme_5yr_alloc) recommendations. Same catalog, same preference/
  # bucket-cap/claim-tier settings, same constraints -- only gap_tons and
  # budget differ between the two callers. Originally the 5-year view used
  # a separate, simpler engine on the assumption that a real multi-year
  # solve would need year-varying prices/supply; turned out prices are
  # already flat/static regardless of year and supply_tons has no year
  # dimension either, so there was no actual reason to keep two different
  # engines. Treats supply_tons as a ceiling across the WHOLE gap_tons
  # window passed in (a single 5-year gap here, not re-granted per year)
  # -- the conservative reading, not an inflated one.
  #
  # Geographic proximity (per the Jul 31 sync, extended for county-level
  # granularity, extended again for Phase 1 global country support):
  # projects near the client's facility get a soft preference, now a
  # 5-tier hierarchy -- same COUNTY (US-to-US, finest available), same
  # STATE (US-to-US, different county), same COUNTRY (the finest tier
  # available for any non-US facility), same macro-REGION, or elsewhere
  # globally. Purely a soft nudge on the SAME objective the bucket-
  # preference slider already uses, not a hard constraint -- a much
  # cheaper or much-needed distant project can still win. All 3
  # preference dimensions (cost-effectiveness, bucket mix, proximity)
  # are normalized to sum to 1 so the sliders behave as genuinely
  # independent weights, not a nested/interacting blend.
  compute_proximity_score <- function(catalog, facility_country, facility_state = NA_character_, facility_county = NA_character_) {
    if (is.null(facility_country) || !nzchar(facility_country)) return(rep(0, nrow(catalog)))
    facility_region <- country_region_lookup[[facility_country]]

    same_county <- !is.na(catalog$county) & !is.na(facility_county) & nzchar(facility_county) &
      !is.na(facility_state) & nzchar(facility_state) &
      catalog$state == facility_state & catalog$county == facility_county
    same_state <- !is.na(catalog$state) & !is.na(facility_state) & nzchar(facility_state) &
      catalog$state == facility_state
    same_country <- !is.na(catalog$country) & catalog$country == facility_country
    same_region <- !is.na(catalog$geography) & !is.na(facility_region) & catalog$geography == facility_region

    case_when(
      same_county  ~ 1.0,    # Local: same county (US-to-US only -- the finest precision available today)
      same_state   ~ 0.7,    # Same state, different county (US-to-US)
      same_country ~ 0.6,    # Same country -- the finest precision available for any non-US facility
      same_region  ~ 0.35,   # Same macro-region (e.g. both in Europe), different country
      TRUE         ~ 0.1     # Elsewhere globally
    )
  }

  # price_discount (per the Jul 31 sync): forward pricing typically runs
  # cheaper than spot -- buying credits now for future delivery. Applied
  # BEFORE the LP solve (not after), so a discount genuinely changes what
  # the optimizer can afford within the same budget -- more tons, not
  # just a lower total on paper. Only the 5-year view passes a non-zero
  # discount (pme_5yr_alloc()); the 1-year view (pce_alloc()) has no
  # forward-pricing concept to apply, since it's not a multi-year
  # commitment.
  # scope3_gap_tons (optional): when provided, splits THAT gap across the
  # 15 GHG Protocol categories (same CDP-anchored shares as the Scope 3
  # breakdown on New Company Intake) to cap how much any category-
  # RESTRICTED intervention row can be credited -- see solve_portfolio_lp's
  # own comment for why. NULL (the default) means no category caps are
  # applied -- restricted rows still can't exceed their OWN supply_tons,
  # but nothing stops them from being over-relied on beyond their
  # category's real need. Left NULL for the "ideal demand" internal tab,
  # deliberately, since that tool isn't scoped to one point-in-time gap.
  run_lp_alloc <- function(gap_tons, budget, price_discount = 0, scope3_gap_tons = NULL) {
    req(!is.na(budget), budget >= 0)
    catalog <- catalog_rv()
    if (price_discount > 0) {
      catalog <- catalog %>% mutate(buyer_price = round(buyer_price * (1 - price_discount), 2))
    }
    catalog <- catalog %>% mutate(margin_per_ton = buyer_price - dev_cost)
    w <- pme_weights()

    facility_country <- input$intake_facility_country
    facility_state   <- input$intake_facility_state
    facility_county  <- input$intake_facility_county
    has_facility <- !is.null(facility_country) && nzchar(facility_country)
    alpha <- input$pce_preference_strength / 100                            # bucket-mix weight
    beta  <- if (has_facility) input$pme_proximity_weight / 100 else 0     # proximity weight (0 if no facility set)
    if (alpha + beta > 1) {  # normalize so the two soft preferences never crowd out ALL cost-effectiveness weight
      scale_down <- 1 / (alpha + beta)
      alpha <- alpha * scale_down
      beta  <- beta * scale_down
    }
    uniform_weight <- 1 - alpha - beta

    preference_weights <- as.numeric(w[catalog$key])
    preference_weights[is.na(preference_weights)] <- 0
    proximity_score <- compute_proximity_score(catalog, facility_country, facility_state, facility_county)
    catalog$proximity_score <- proximity_score
    catalog$proximity_tier  <- if (!has_facility) {
      "Not evaluated (no facility set)"
    } else {
      case_when(
        proximity_score == 1.0  ~ "Local (same county)",
        proximity_score == 0.7  ~ "Regional (same state)",
        proximity_score == 0.6  ~ "Same country",
        proximity_score == 0.35 ~ "Same region",
        TRUE                    ~ "Elsewhere globally"
      )
    }

    obj_weights <- uniform_weight * rep(1, nrow(catalog)) + alpha * preference_weights + beta * proximity_score

    bucket_cap <- input$pce_bucket_cap / 100
    tier_min_frac <- switch(input$pce_claim_tier, "silver" = 0.10, "gold" = 0.50, "platinum" = 1.00, 0)

    category_caps <- NULL
    if (!is.null(scope3_gap_tons) && scope3_gap_tons > 0) {
      shares <- get_scope3_category_shares(input$intake_sector)$shares
      category_caps <- setNames(shares[as.character(scope3_categories$cat_id)] * scope3_gap_tons,
                                 paste0("scope3_cat", scope3_categories$cat_id))
    }

    ideal_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = NULL, tier_min_frac = 0, bucket_cap = bucket_cap, category_caps = category_caps)
    catalog$ideal_tons <- floor(ideal_sol$tons)

    funded_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = budget, tier_min_frac = tier_min_frac, bucket_cap = bucket_cap, category_caps = category_caps)
    tier_shortfall <- tier_min_frac > 0 && funded_sol$status != 0
    if (tier_shortfall) {
      funded_sol <- solve_portfolio_lp(catalog, gap_tons, obj_weights, budget = budget, tier_min_frac = 0, bucket_cap = bucket_cap, category_caps = category_caps)
    }
    catalog$funded_tons <- floor(funded_sol$tons)
    catalog$funded_cost <- round(catalog$funded_tons * catalog$buyer_price)
    catalog$ideal_cost   <- round(catalog$ideal_tons * catalog$buyer_price)
    catalog$profit_ideal  <- round(catalog$ideal_tons * catalog$margin_per_ton)

    attr(catalog, "tier_shortfall")   <- tier_shortfall
    attr(catalog, "tier_min_frac")    <- tier_min_frac
    attr(catalog, "gap_tons")         <- gap_tons
    attr(catalog, "budget")           <- budget
    attr(catalog, "facility_country") <- facility_country
    attr(catalog, "facility_state")   <- facility_state
    attr(catalog, "price_discount")   <- price_discount
    catalog
  }

  pce_alloc <- reactive({
    scope3_gap <- pme_scope_gaps() %>% filter(scope == "Scope 3") %>% pull(gap_tons)
    scope3_gap <- if (length(scope3_gap) > 0) scope3_gap[1] else 0
    run_lp_alloc(pme_gap_tons(), input$pme_budget, scope3_gap_tons = scope3_gap)
  })

  # ---- Coverage audit (per follow-up request): with the CURRENT catalog
  # inventory, which parts of the gap can actually be addressed and which
  # can't -- Scope 1, Scope 2, and each of the 15 Scope 3 categories
  # individually, not just one combined "% covered" number. ----
  pce_coverage_audit <- reactive({
    catalog <- catalog_rv()
    scope_gaps <- pme_scope_gaps()
    get_gap <- function(nm) {
      v <- scope_gaps$gap_tons[scope_gaps$scope == nm]
      if (length(v) > 0) v[1] else 0
    }
    gap1 <- get_gap("Scope 1"); gap2 <- get_gap("Scope 2"); gap3 <- get_gap("Scope 3")

    agnostic_supply <- sum(catalog$supply_tons[catalog$applicable_scope == "any"], na.rm = TRUE)
    scope12_gap <- gap1 + gap2

    # Each Scope 3 category's own gap ceiling -- SAME sector-aware split
    # already used for the LP's category_caps, so this audit reflects
    # exactly what the optimizer itself is actually constrained by, not a
    # separately-invented number.
    share_info <- get_scope3_category_shares(input$intake_sector)
    cat_rows <- scope3_categories %>%
      mutate(
        cat_gap = share_info$shares[as.character(cat_id)] * gap3,
        dedicated_supply = sapply(cat_id, function(id) {
          lbl <- paste0("scope3_cat", id)
          sum(catalog$supply_tons[catalog$applicable_scope == lbl], na.rm = TRUE)
        }),
        has_dedicated  = dedicated_supply > 0,
        dedicated_covers_fully = dedicated_supply >= cat_gap,
        status = case_when(
          cat_gap <= 0             ~ "No gap",
          dedicated_covers_fully   ~ "Covered (dedicated)",
          has_dedicated            ~ "Partial (dedicated + generic needed)",
          TRUE                     ~ "Generic credits only (no dedicated project type)"
        )
      )

    # Real RECOMMENDED portfolio coverage per scope -- same proportional-
    # split method already used in the Portfolio Mix summary cards
    # (funded tons attributed to each scope by its share of the combined
    # gap), reused here rather than a separate invented calculation, so
    # this can't show a different "coverage" number than what the
    # Portfolio Mix tabs already say.
    alloc <- tryCatch(pce_alloc(), error = function(e) NULL)
    total_funded <- if (!is.null(alloc)) sum(alloc$funded_tons) else 0
    combined_gap <- gap1 + gap2 + gap3
    scope_share <- function(g) if (combined_gap > 0) g / combined_gap else 0
    funded1 <- total_funded * scope_share(gap1)
    funded2 <- total_funded * scope_share(gap2)
    funded3 <- total_funded * scope_share(gap3)

    list(
      gap1 = gap1, gap2 = gap2, gap3 = gap3, scope12_gap = scope12_gap,
      funded1 = funded1, funded2 = funded2, funded3 = funded3,
      agnostic_supply = agnostic_supply,
      scope12_feasible = agnostic_supply >= scope12_gap,
      cat_rows = cat_rows
    )
  })

  output$pce_coverage_summary <- renderUI({
    a <- pce_coverage_audit()
    n_dedicated_full <- sum(a$cat_rows$status == "Covered (dedicated)")
    n_partial <- sum(a$cat_rows$status == "Partial (dedicated + generic needed)")
    n_generic_only <- sum(a$cat_rows$status == "Generic credits only (no dedicated project type)")

    tagList(
      tags$div(
        style = "display:grid; grid-template-columns:repeat(4, 1fr); gap:10px; margin-bottom:10px;",
        tags$div(
          style = paste0("background:#F4F6F7; border-radius:6px; padding:0.6rem; text-align:center;"),
          tags$div(style = paste0("font-size:16px; font-weight:700; color:", if (a$scope12_feasible) "#1E8449" else "#C0392B", ";"),
                    if (a$scope12_feasible) "Feasible" else "Short"),
          tags$div(style = "font-size:11px; color:#7F8C8D;", "Scope 1+2 combined (shared pool)")
        ),
        tags$div(
          style = "background:#F4F6F7; border-radius:6px; padding:0.6rem; text-align:center;",
          tags$div(style = "font-size:16px; font-weight:700; color:#1E8449;", n_dedicated_full),
          tags$div(style = "font-size:11px; color:#7F8C8D;", "Scope 3 categories fully covered (dedicated)")
        ),
        tags$div(
          style = "background:#F4F6F7; border-radius:6px; padding:0.6rem; text-align:center;",
          tags$div(style = "font-size:16px; font-weight:700; color:#B7950B;", n_partial),
          tags$div(style = "font-size:11px; color:#7F8C8D;", "Categories partially covered")
        ),
        tags$div(
          style = "background:#F4F6F7; border-radius:6px; padding:0.6rem; text-align:center;",
          tags$div(style = "font-size:16px; font-weight:700; color:#7F8C8D;", n_generic_only),
          tags$div(style = "font-size:11px; color:#7F8C8D;", "Categories with no dedicated project type")
        )
      )
    )
  })

  # Three small donut charts -- Scope 1, Scope 2, Scope 3 -- each showing
  # what fraction of THAT scope's gap the CURRENT recommended portfolio
  # actually covers (green) vs what's left (red). Answers "how much are
  # we covering with the current projects" directly and at a glance,
  # using the same real funded-tons numbers already shown in the
  # Portfolio Mix summary cards -- not a separate calculation that could
  # tell a different story.
  build_coverage_donut <- function(covered, gap, label) {
    covered <- min(covered, gap)
    remaining <- max(gap - covered, 0)
    pct <- if (gap > 0) round(covered / gap * 100) else 100
    df <- tibble(
      slice = factor(c("Covered", "Remaining"), levels = c("Covered", "Remaining")),
      value = c(covered, remaining)
    )
    ggplot(df, aes(x = 2, y = value, fill = slice)) +
      geom_col(color = "white", linewidth = 1.2) +
      coord_polar(theta = "y") +
      xlim(0.2, 2.5) +
      scale_fill_manual(values = c("Covered" = "#27AE60", "Remaining" = "#E5E8E8"), name = NULL) +
      annotate("text", x = 0.2, y = 0, label = paste0(pct, "%"), size = 7, fontface = "bold", color = "#2C3E50") +
      labs(title = label) +
      theme_void(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
        legend.position = "bottom"
      )
  }

  output$pce_coverage_pie_s1 <- renderPlot({
    a <- pce_coverage_audit()
    build_coverage_donut(a$funded1, a$gap1, "Scope 1")
  })
  output$pce_coverage_pie_s2 <- renderPlot({
    a <- pce_coverage_audit()
    build_coverage_donut(a$funded2, a$gap2, "Scope 2")
  })
  output$pce_coverage_pie_s3 <- renderPlot({
    a <- pce_coverage_audit()
    build_coverage_donut(a$funded3, a$gap3, "Scope 3 (all categories combined)")
  })

  # % covered by DEDICATED supply, per Scope 3 category -- deliberately
  # a PERCENTAGE (0-100%), not raw tons. The previous version plotted raw
  # tons on one shared axis across Scope 1+2's combined pool AND all 15
  # categories -- Scope 1+2's pool was orders of magnitude larger than
  # any single category's gap, which crushed every category bar to
  # invisible on that shared scale. A 0-100% axis has no such problem,
  # and answers the actual question -- "how much of THIS category's own
  # gap is covered" -- directly.
  output$pce_coverage_category_plot <- renderPlot({
    a <- pce_coverage_audit()
    df <- a$cat_rows %>%
      filter(cat_gap > 0) %>%
      mutate(
        pct_covered = pmin(dedicated_supply / cat_gap * 100, 100),
        label = paste0("Cat ", cat_id, ": ", cat_name)
      ) %>%
      arrange(pct_covered)
    req(nrow(df) > 0)
    df <- df %>% mutate(label = factor(label, levels = label))

    ggplot(df, aes(x = label, y = pct_covered, fill = status)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = paste0(round(pct_covered), "%")), hjust = -0.15, size = 3.2) +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Covered (dedicated)" = "#27AE60",
          "Partial (dedicated + generic needed)" = "#F1C40F",
          "Generic credits only (no dedicated project type)" = "#BFC5CA"
        ),
        name = NULL
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105), expand = c(0, 0)) +
      labs(
        subtitle = "% of each category's own gap covered by DEDICATED supply -- the rest, if any, needs generic credits",
        x = NULL, y = "% Covered"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.subtitle = element_text(color = "grey40", size = 10.5), legend.position = "top")
  })

  output$pce_coverage_table <- renderDT({
    a <- pce_coverage_audit()
    df <- a$cat_rows %>%
      transmute(
        cat_id, cat_name,
        cat_gap = round(cat_gap),
        dedicated_supply = round(dedicated_supply),
        status
      )
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 15, dom = "t"),
      colnames = c("Category #", "GHG Protocol Category", "This Category's Gap (t)", "Dedicated Supply Available (t)", "Status")
    ) %>%
      formatStyle(
        "status", target = "row",
        backgroundColor = styleEqual(
          c("Covered (dedicated)", "Partial (dedicated + generic needed)", "Generic credits only (no dedicated project type)"),
          c("#EAFAF1", "#FEF9E7", "#FDEDEC")
        )
      )
  })

  output$pce_coverage_note <- renderUI({
    a <- pce_coverage_audit()
    tags$div(
      style = "background:#F4F6F7; border-radius:6px; padding:0.6rem 0.9rem; font-size:12.5px;",
      tags$b("Scope 1: "), comma(round(a$gap1)), " t gap. ",
      tags$b("Scope 2: "), comma(round(a$gap2)), " t gap. Both draw on ",
      comma(round(a$agnostic_supply)), " t of scope-agnostic catalog supply (combined pool, ",
      "also shared with any Scope 3 category that has no dedicated project type)."
    )
  })

  output$pce_context <- renderUI({
    alloc <- pce_alloc()
    gap_tons <- attr(alloc, "gap_tons")
    budget   <- attr(alloc, "budget")
    facility_country <- attr(alloc, "facility_country")
    facility_state   <- attr(alloc, "facility_state")
    has_facility <- !is.null(facility_country) && nzchar(facility_country)
    facility_label <- if (!has_facility) {
      NA_character_
    } else if (identical(facility_country, "United States") && !is.null(facility_state) && nzchar(facility_state)) {
      paste0(facility_state, ", United States")
    } else {
      facility_country
    }
    total_funded_tons <- sum(alloc$funded_tons)
    total_funded_cost <- sum(alloc$funded_cost)
    coverage_pct <- if (gap_tons > 0) round(total_funded_tons / gap_tons * 100, 1) else 0

    tagList(
      tags$b("Gap: "), tags$span(comma(round(gap_tons)), " tCO2e (", input$pme_year, ")"),
      tags$span(" | ", tags$b("Budget: "), "$", comma(budget)), tags$br(),
      tags$b("Recommended: "), tags$span(comma(total_funded_tons), " tCO2e (", coverage_pct, "% of gap), $", comma(total_funded_cost)),
      tags$br(),
      tags$b("Facility: "),
      tags$span(if (has_facility) facility_label else "not specified -- proximity ignored"),
      if (has_facility) tags$span(" | ", tags$b("Proximity weight: "), input$pme_proximity_weight, "%"),
      if (isTRUE(attr(alloc, "tier_shortfall"))) {
        tags$div(
          style = "background:#FDEDEC; border:1px solid #E74C3C; border-radius:4px; padding:0.3rem 0.6rem; margin-top:0.3rem;",
          tags$b("Claim tier not met: "),
          "the ", scales::percent(attr(alloc, "tier_min_frac")), " coverage floor can't be reached within ",
          "budget, supply, and concentration caps -- showing the best budget-constrained mix instead."
        )
      }
    )
  })

  # ---- Facility / project map -- per request, a visual showing where the
  # company operates (large dark marker) alongside the states of the
  # projects this portfolio actually selected (colored by bucket, sized
  # by tons funded). State-level dots, not exact project coordinates --
  # this catalog doesn't have real lat/lon per project, and a state
  # centroid is the honest level of precision available given projects
  # are only located to the state, not a specific address. Shared
  # builder -- used identically on the Methodologies, Short-Term, and
  # Long-Term tabs, just fed a different allocation (pce_alloc() vs
  # pme_5yr_alloc()) so the map matches whichever portfolio is showing. ----
  build_facility_map_us <- function(alloc, facility_state, facility_county = NA_character_) {
    us_states_map <- map_data("state")

    # Per-county TOTAL funded tons (summed across all buckets) -- drives
    # the BUBBLE size only now (not a choropleth fill -- reverted per
    # explicit request: bubbles alone, no county coloring by tons).
    county_totals <- alloc %>%
      filter(funded_tons > 0, !is.na(state)) %>%
      group_by(state, county) %>%
      summarise(funded_tons = sum(funded_tons), .groups = "drop")

    county_points <- county_totals %>% left_join(us_county_lookup, by = c("state", "county"))

    # Plain county + state boundaries -- thin county lines (fine detail),
    # thick state borders on top (the visual hierarchy), no fill value
    # tied to either layer.
    p <- ggplot() +
      geom_polygon(
        data = us_county_map_data, aes(x = long, y = lat, group = group),
        fill = "#E5E7E9", color = "#95A5A6", linewidth = 0.15
      ) +
      geom_polygon(
        data = us_states_map, aes(x = long, y = lat, group = group),
        fill = NA, color = "#5D6D7E", linewidth = 0.7
      ) +
      coord_fixed(1.3) +
      theme_void(base_size = 13)

    if (nrow(county_points) > 0) {
      p <- p + geom_point(
        data = county_points, aes(x = lon, y = lat, size = funded_tons),
        shape = 21, fill = "#D7301F", color = "white", stroke = 0.6, alpha = 0.9
      ) +
        scale_size_continuous(name = "Tons funded", range = c(2, 14))
    }

    if (nzchar(facility_state)) {
      # State-wide semi-transparent RED FILL (not a single dot) = the
      # broad "where the company operates" indicator -- covers the whole
      # state so it reads as an AREA the company operates in, not one
      # point on it. County star (below) still marks the PRECISE location
      # used for actual proximity scoring -- the two markers stay
      # visually distinct: a translucent area vs. a sharp point.
      facility_state_poly <- us_states_map %>% filter(region == tolower(facility_state))
      has_county <- !is.na(facility_county) && nzchar(facility_county)
      county_pt <- if (has_county) {
        us_county_lookup %>% filter(state == facility_state, county == facility_county)
      } else {
        tibble()
      }

      if (nrow(facility_state_poly) > 0) {
        p <- p + geom_polygon(
          data = facility_state_poly, aes(x = long, y = lat, group = group),
          fill = "#E74C3C", alpha = 0.25, color = NA
        )
      }

      if (nrow(county_pt) > 0) {
        # Dark star = county-level marker -- the PRECISE location, only
        # shown when a county is actually set. Visually distinct shape
        # AND color from both the red state fill and the funded-county
        # bubbles, so all three read as different kinds of thing at a
        # glance.
        p <- p + geom_point(
          data = county_pt, aes(x = lon, y = lat),
          shape = 8, size = 6, stroke = 1.8, color = "#1B2631"
        ) +
          geom_text(
            data = county_pt, aes(x = lon, y = lat), label = paste0("  ", facility_county, " County"),
            hjust = 0, vjust = 1.6, size = 3.6, fontface = "bold", color = "#1B2631"
          )
      } else {
        # No county set -- at least label the state itself, since there's
        # no star/text to identify it otherwise.
        state_pt <- us_state_centroids %>% filter(state == facility_state)
        if (nrow(state_pt) > 0) {
          p <- p + geom_text(
            data = state_pt, aes(x = lon, y = lat), label = facility_state,
            size = 4.2, fontface = "bold", color = "#C0392B"
          )
        }
      }
    }

    facility_note <- if (!nzchar(facility_state)) {
      "Set a facility location in the sidebar to see it marked."
    } else if (!is.na(facility_county) && nzchar(facility_county)) {
      "Red shading = state (broad). \u2605 dark star = county (precise, used for proximity)."
    } else {
      "Red shading = state. Add a county in the sidebar for a more precise marker."
    }

    p + labs(
      subtitle = paste0(
        "Bubble = a county with funded projects, sized by tons funded there. Click below the map to see its projects. ", facility_note
      )
    ) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "right")
  }

  # World map (Phase 1) -- used whenever the facility or any funded
  # project is outside the US. Country-level precision only (no
  # sub-national polygons in this phase -- see has_subnational_data()'s
  # own comment for why). Degrades to a text-only summary if
  # rnaturalearth/sf aren't installed (has_world_map_pkgs, checked at
  # startup), rather than erroring.
  build_facility_map_world <- function(alloc, facility_country, facility_state = NA_character_) {
    if (!has_world_map_pkgs || is.null(world_countries_sf)) {
      return(NULL)  # caller falls back to the text-only summary table
    }

    country_totals <- alloc %>%
      filter(funded_tons > 0, !is.na(country)) %>%
      group_by(country) %>%
      summarise(funded_tons = sum(funded_tons), .groups = "drop") %>%
      left_join(country_centroids, by = "country")

    p <- ggplot() +
      geom_sf(data = world_countries_sf, fill = "#E5E7E9", color = "#95A5A6", linewidth = 0.15) +
      coord_sf(crs = sf::st_crs(4326)) +
      theme_void(base_size = 13)

    facility_country_safe <- if (is.null(facility_country)) "" else facility_country
    if (nzchar(facility_country_safe) && facility_country_safe %in% names(country_name_to_ne_admin)) {
      ne_name <- country_name_to_ne_admin[[facility_country]]
      admin_col <- if ("admin" %in% names(world_countries_sf)) "admin" else "name_long"
      facility_poly <- world_countries_sf[world_countries_sf[[admin_col]] == ne_name, ]
      if (nrow(facility_poly) > 0) {
        p <- p + geom_sf(data = facility_poly, fill = "#E74C3C", alpha = 0.25, color = NA)
      }
    }

    if (nrow(country_totals) > 0) {
      p <- p + geom_point(
        data = country_totals, aes(x = lon, y = lat, size = funded_tons),
        shape = 21, fill = "#D7301F", color = "white", stroke = 0.6, alpha = 0.9
      ) +
        scale_size_continuous(name = "Tons funded", range = c(3, 16)) +
        geom_text(
          data = country_totals, aes(x = lon, y = lat, label = country),
          size = 3, vjust = -1.4, fontface = "bold", color = "#7B241C"
        )
    }

    facility_note <- if (!nzchar(facility_country_safe)) {
      "Set a facility location in the sidebar to see it marked."
    } else {
      "Red shading = facility's country. Bubbles = countries with funded projects (country-level precision -- Phase 1)."
    }

    p + labs(subtitle = paste0("Bubble = a country with funded projects, sized by tons funded there. ", facility_note)) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "right")
  }

  # Same data, same logic as build_facility_map_world() above -- just
  # cropped to a macro-region's bounding box via coord_sf's xlim/ylim.
  # No separate map dataset or code path per region; this IS the world
  # map, viewed closer. Bounding boxes are rough visual framing, not
  # precise geographic boundaries.
  region_bounding_boxes <- list(
    "Europe"        = list(xlim = c(-25, 45),   ylim = c(34, 72)),
    "North America" = list(xlim = c(-170, -50), ylim = c(10, 75)),
    "Asia"          = list(xlim = c(60, 145),   ylim = c(-10, 55)),
    "Australia"     = list(xlim = c(110, 155),  ylim = c(-45, -10)),
    "Latin America" = list(xlim = c(-90, -30),  ylim = c(-58, 15)),
    "Middle East"   = list(xlim = c(30, 60),    ylim = c(10, 40)),
    "Africa"        = list(xlim = c(-20, 55),   ylim = c(-38, 38))
  )

  build_facility_map_region_zoom <- function(alloc, facility_country, region) {
    if (!has_world_map_pkgs || is.null(world_countries_sf)) return(NULL)
    bbox <- region_bounding_boxes[[region]]
    if (is.null(bbox)) return(NULL)

    # Actually crop the polygon geometry itself, not just the view.
    # coord_sf(xlim=, ylim=, expand=FALSE) alone was NOT reliably
    # restricting what renders -- confirmed directly: the "zoomed"
    # LATAM map was still showing the whole world's country outlines
    # even with that in place. st_crop() physically subsets the
    # polygons to the bounding box before plotting, so there's nothing
    # outside the target region for the panel to show, regardless of
    # any coord_sf display quirk behind the original symptom.
    # Wrapped in tryCatch -- Natural Earth's medium-scale country
    # polygons occasionally have self-intersecting/invalid geometry
    # that st_crop() can choke on; falls back to the uncropped
    # polygons (coord_sf's xlim/ylim below still applies as a second
    # layer of restriction either way) rather than erroring the map.
    crop_box <- sf::st_bbox(
      c(xmin = bbox$xlim[1], xmax = bbox$xlim[2], ymin = bbox$ylim[1], ymax = bbox$ylim[2]),
      crs = sf::st_crs(4326)
    )
    world_cropped <- tryCatch(
      suppressWarnings(sf::st_crop(sf::st_make_valid(world_countries_sf), crop_box)),
      error = function(e) world_countries_sf
    )

    country_totals <- alloc %>%
      filter(funded_tons > 0, !is.na(country)) %>%
      group_by(country) %>%
      summarise(funded_tons = sum(funded_tons), .groups = "drop") %>%
      left_join(country_centroids, by = "country") %>%
      # Filtered to the region's bounding box too -- keeps the point
      # layer itself free of far-away dots, independent of the polygon
      # crop above.
      filter(lon >= bbox$xlim[1], lon <= bbox$xlim[2], lat >= bbox$ylim[1], lat <= bbox$ylim[2])

    p <- ggplot() +
      geom_sf(data = world_cropped, fill = "#E5E7E9", color = "#95A5A6", linewidth = 0.15) +
      coord_sf(crs = sf::st_crs(4326), xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
      theme_void(base_size = 13)

    facility_country_safe <- if (is.null(facility_country)) "" else facility_country
    if (nzchar(facility_country_safe) && facility_country_safe %in% names(country_name_to_ne_admin)) {
      ne_name <- country_name_to_ne_admin[[facility_country_safe]]
      admin_col <- if ("admin" %in% names(world_cropped)) "admin" else "name_long"
      facility_poly <- world_cropped[world_cropped[[admin_col]] == ne_name, ]
      if (nrow(facility_poly) > 0) {
        p <- p + geom_sf(data = facility_poly, fill = "#E74C3C", alpha = 0.25, color = NA)
      }
    }

    if (nrow(country_totals) > 0) {
      p <- p + geom_point(
        data = country_totals, aes(x = lon, y = lat, size = funded_tons),
        shape = 21, fill = "#D7301F", color = "white", stroke = 0.6, alpha = 0.9
      ) +
        scale_size_continuous(name = "Tons funded", range = c(3, 16)) +
        geom_text(
          data = country_totals, aes(x = lon, y = lat, label = country),
          size = 3, vjust = -1.4, fontface = "bold", color = "#7B241C"
        )
    }

    n_outside <- alloc %>% filter(funded_tons > 0, !is.na(country)) %>% distinct(country) %>% nrow() - nrow(country_totals)
    outside_note <- if (n_outside > 0) paste0(" ", n_outside, " funded countr", if (n_outside == 1) "y is" else "ies are", " outside this crop -- see the world map for those.") else ""

    p + labs(subtitle = paste0("Same data as the world map, cropped to ", region, ". Red shading = selected country.", outside_note)) +
      theme(plot.subtitle = element_text(color = "grey40", size = 11), legend.position = "right")
  }

  # Kept as a thin wrapper -- EU Portfolio Mix already calls this name.
  build_facility_map_eu_zoom <- function(alloc, facility_country) {
    build_facility_map_region_zoom(alloc, facility_country, "Europe")
  }

  # ---- Interactive globe PREVIEW (plotly, draggable -- not auto-
  # rotating) ----
  # A user-draggable globe, not a continuously auto-rotating one --
  # auto-rotation makes bubbles impossible to click and labels
  # impossible to read while moving, which defeats the point of a tool
  # meant to be worked with. Plotly's orthographic projection lets the
  # person spin it themselves to look wherever they want, with real
  # hover tooltips (something the static ggplot2 maps can't do at all).
  # Degrades to NULL (caller shows a plain message) if plotly isn't
  # installed -- same soft-dependency pattern as rnaturalearth/sf.
  build_globe_preview <- function(alloc, facility_country) {
    if (!has_plotly) return(NULL)

    funded <- alloc %>% filter(funded_tons > 0, !is.na(country))
    if (nrow(funded) == 0) return(NULL)

    country_totals <- funded %>%
      group_by(country) %>%
      summarise(funded_tons = sum(funded_tons), funded_cost = sum(funded_cost),
                n_projects = n(), .groups = "drop") %>%
      left_join(country_centroids, by = "country")

    # Project names per country, for the hover list -- capped at 6
    # names with a "+N more" tail so a country with 30 tiny projects
    # doesn't produce an unreadable wall of text in the tooltip.
    project_names_by_country <- funded %>%
      arrange(country, desc(funded_tons)) %>%
      group_by(country) %>%
      summarise(
        project_list = {
          names <- project_type
          shown <- names[seq_len(min(6, length(names)))]
          extra <- length(names) - length(shown)
          paste0(paste(shown, collapse = "<br>&nbsp;&nbsp;\u2022 "),
                 if (extra > 0) paste0("<br>&nbsp;&nbsp;+", extra, " more") else "")
        },
        .groups = "drop"
      )

    country_totals <- country_totals %>% left_join(project_names_by_country, by = "country")

    hover_text <- paste0(
      "<b>", country_totals$country, "</b><br>",
      "Tons funded: ", comma(country_totals$funded_tons), "<br>",
      "Cost: $", comma(country_totals$funded_cost), "<br>",
      "Projects (", country_totals$n_projects, "):<br>&nbsp;&nbsp;\u2022 ", country_totals$project_list
    )

    # sqrt scaling so one huge country's bubble doesn't visually swamp
    # every smaller one on a globe view -- same reasoning as the flat
    # maps' scale_size_continuous(), just done manually since plotly
    # marker sizes are raw pixels, not a ggplot scale.
    marker_sizes <- pmax(8, pmin(40, sqrt(country_totals$funded_tons) / 4))

    fc_lookup <- if (!is.null(facility_country) && nzchar(facility_country)) {
      country_centroids %>% filter(country == facility_country)
    } else {
      country_centroids[0, ]
    }

    fig <- plotly::plot_ly(
      country_totals, type = "scattergeo", mode = "markers",
      lon = ~lon, lat = ~lat,
      marker = list(size = marker_sizes, color = "#D7301F", opacity = 0.85,
                    line = list(color = "white", width = 1)),
      text = hover_text, hoverinfo = "text", name = "Funded projects"
    )

    if (nrow(fc_lookup) > 0) {
      fig <- fig %>% plotly::add_trace(
        data = fc_lookup, type = "scattergeo", mode = "markers",
        lon = ~lon, lat = ~lat,
        marker = list(size = 14, color = "#F1C40F", symbol = "star", line = list(color = "#B7950B", width = 1)),
        text = paste0("Selected: ", fc_lookup$country), hoverinfo = "text", name = "Selected country"
      )
    }

    fig <- fig %>% plotly::layout(
      showlegend = FALSE,
      geo = list(
        projection = list(type = "orthographic"),
        showland = TRUE, landcolor = "#E5E7E9",
        showcountries = TRUE, countrycolor = "#95A5A6",
        showocean = TRUE, oceancolor = "#F4F6F7",
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )

    # Auto-rotation, pausing on hover -- genuine custom JavaScript, not
    # standard Plotly/R usage, since Plotly has no built-in "auto-
    # rotate" setting. A setInterval() nudges the globe's rotation
    # angle via Plotly.relayout() roughly 16x/second (a slow, ~70-
    # second full rotation -- fast enough to read as motion, slow
    # enough not to be dizzying); plotly_hover/plotly_unhover listeners
    # stop and restart that timer, so hovering a bubble to read its
    # tooltip doesn't fight against the globe moving out from under
    # the cursor. This is a known, documented technique for animating
    # plotly orthographic globes -- NOT something executed or verified
    # here, since there's no live browser to test it against; if it
    # doesn't behave as expected, that's exactly the kind of thing to
    # report back with specifics (what happened instead) rather than
    # assume is unfixable.
    tryCatch({
      fig <- htmlwidgets::onRender(fig, "
        function(el, x) {
          var rotation = 0;
          var interval = null;
          function startRotation() {
            if (interval) return;
            interval = setInterval(function() {
              rotation = (rotation + 0.3) % 360;
              Plotly.relayout(el, {'geo.projection.rotation.lon': rotation});
            }, 60);
          }
          function stopRotation() {
            if (interval) { clearInterval(interval); interval = null; }
          }
          startRotation();
          el.on('plotly_hover', function(data) { stopRotation(); });
          el.on('plotly_unhover', function(data) { startRotation(); });
        }
      ")
    }, error = function(e) {
      # If the JS injection fails for any reason, fig (still defined
      # above) is returned as-is below -- draggable but not
      # auto-rotating, same as the previous working version, rather
      # than the whole globe breaking.
    })

    fig
  }

  # Dispatch: the well-tested detailed US map when everything (facility +
  # every funded project) is US-based -- zero behavior change from
  # before Phase 1. The new world map otherwise. This is the ONLY place
  # that needs to know both maps exist; every caller just calls
  # build_facility_map() the same way regardless of geography.
  build_facility_map <- function(alloc, facility_country, facility_state = NA_character_, facility_county = NA_character_) {
    facility_is_us_or_unset <- is.null(facility_country) || !nzchar(facility_country) || identical(facility_country, "United States")
    projects_all_us_or_unset <- all(is.na(alloc$country) | alloc$country == "United States")

    if (facility_is_us_or_unset && projects_all_us_or_unset) {
      build_facility_map_us(alloc, if (is.null(facility_state)) "" else facility_state, facility_county)
    } else {
      world_map <- build_facility_map_world(alloc, facility_country, facility_state)
      if (is.null(world_map)) {
        # rnaturalearth/sf not installed -- degrade to a blank themed
        # plot with a clear message rather than a broken/missing output.
        ggplot() +
          annotate("text", x = 0, y = 0, label = paste0(
            "World map unavailable (rnaturalearth/sf not installed on this server).\n",
            "Install with: install.packages(c('rnaturalearth', 'rnaturalearthdata', 'sf'))"
          ), size = 4) +
          theme_void()
      } else {
        world_map
      }
    }
  }

  # Finds the county whose CENTROID is nearest a clicked map coordinate
  # -- an approximation (a true point-in-polygon test would be more
  # precise right at a county's edge), but robust and dependency-light,
  # and accurate enough for a click-to-inspect feature at this map scale.
  nearest_county <- function(click_lon, click_lat) {
    if (is.null(click_lon) || is.null(click_lat)) return(NULL)
    d2 <- (us_county_lookup$lon - click_lon)^2 + (us_county_lookup$lat - click_lat)^2
    us_county_lookup[which.min(d2), ]
  }

  output$pce_map <- renderPlot({
    build_facility_map(pce_alloc(), input$intake_facility_country, input$intake_facility_state, input$intake_facility_county)
  })

  # Per request: BOTH maps shown together, not one dispatched over the
  # other -- the US map is the precise view of where the facility itself
  # operates (state/county), while the world map is the big picture of
  # where every funded project actually is, since projects can now be
  # anywhere across the 19 supported countries. Two separate outputs per
  # tab, called directly rather than through the old single-map
  # dispatcher.
  output$pme_map_us <- renderPlot({
    build_facility_map_us(pce_alloc(), input$intake_facility_state, input$intake_facility_county)
  })
  output$pme_map_world <- renderPlot({
    world_map <- build_facility_map_world(pce_alloc(), input$intake_facility_country, input$intake_facility_state)
    if (is.null(world_map)) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste0(
          "World map unavailable (rnaturalearth/sf not installed on this server).\n",
          "Install with: install.packages(c('rnaturalearth', 'rnaturalearthdata', 'sf'))"
        ), size = 4) +
        theme_void()
    } else {
      world_map
    }
  })

  # Same pair, Long-Term tab -- fed the 5-year allocation instead, so it
  # shows where the AGGREGATED 5-year portfolio's projects are, not just
  # one year's.
  output$pme_5yr_map_us <- renderPlot({
    build_facility_map_us(pme_5yr_alloc(), input$intake_facility_state, input$intake_facility_county)
  })
  output$pme_5yr_map_world <- renderPlot({
    world_map <- build_facility_map_world(pme_5yr_alloc(), input$intake_facility_country, input$intake_facility_state)
    if (is.null(world_map)) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste0(
          "World map unavailable (rnaturalearth/sf not installed on this server).\n",
          "Install with: install.packages(c('rnaturalearth', 'rnaturalearthdata', 'sf'))"
        ), size = 4) +
        theme_void()
    } else {
      world_map
    }
  })

  # ---- Click-to-inspect: which county was clicked, and what's funded
  # there. Native Shiny plotOutput click events (NOT plotly) -- deliberate,
  # given this app's own history of real plotly conversion crashes earlier
  # this session, especially on complex polygon geometry like this map.
  # click$x/click$y arrive in DATA coordinates (the plot's own aes(x=long,
  # y=lat)), so no pixel-to-coordinate conversion is needed.
  build_county_detail_ui <- function(click, alloc, facility_country = NA_character_) {
    # Same "which map is actually showing" check the dispatcher itself
    # uses -- county-level click detection only makes sense against the
    # US map; on the world map a click's lat/lon would otherwise match
    # to "nearest US county by raw distance," which is a real but
    # meaningless (and misleading) answer for a click on another
    # continent entirely.
    facility_is_us_or_unset <- is.null(facility_country) || is.na(facility_country) || !nzchar(facility_country) || identical(facility_country, "United States")
    projects_all_us_or_unset <- all(is.na(alloc$country) | alloc$country == "United States")
    if (!facility_is_us_or_unset || !projects_all_us_or_unset) {
      return(tags$div(
        style = "color:#7F8C8D; font-size:12.5px; padding:0.5rem 0;",
        em("Country-level detail is shown directly on the map above (bubble size = tons funded) -- click-to-inspect detail is only available for the US county map.")
      ))
    }

    sel <- nearest_county(click$x, click$y)
    if (is.null(sel)) {
      return(tags$div(style = "color:#7F8C8D; font-size:12.5px; padding:0.5rem 0;",
                       em("Click a county on the map above to see which projects are funded there.")))
    }
    projects <- alloc %>% filter(state == sel$state, county == sel$county, funded_tons > 0)
    if (nrow(projects) == 0) {
      return(tags$div(
        style = "background:#F4F6F7; border-radius:6px; padding:0.6rem 1rem; font-size:13px;",
        tags$b(sel$county, " County, ", sel$state), " -- no funded projects here in this recommendation."
      ))
    }
    tagList(
      tags$div(
        style = "background:#EBF5FB; border-radius:6px; padding:0.6rem 1rem; margin-bottom:8px; font-size:13px;",
        tags$b(sel$county, " County, ", sel$state, ": "),
        comma(sum(projects$funded_tons)), " t funded across ", nrow(projects), " project(s)"
      ),
      tags$ul(
        style = "font-size:12.5px; padding-left:1.2rem;",
        lapply(seq_len(nrow(projects)), function(i) {
          tags$li(tags$b(projects$project_type[i]), " -- ", comma(projects$funded_tons[i]), " t, $",
                   comma(projects$funded_cost[i]), " (", projects$methodology_name[i], ")")
        })
      )
    )
  }

  output$pce_map_detail <- renderUI({ build_county_detail_ui(input$pce_map_click, pce_alloc(), input$intake_facility_country) })
  output$pme_map_detail <- renderUI({ build_county_detail_ui(input$pme_map_click, pce_alloc(), input$intake_facility_country) })
  output$pme_5yr_map_detail <- renderUI({ build_county_detail_ui(input$pme_5yr_map_click, pme_5yr_alloc(), input$intake_facility_country) })

  output$pce_tons_plot <- renderPlot({
    alloc <- pce_alloc()
    req(sum(alloc$funded_tons) > 0)
    df <- alloc %>% filter(funded_tons > 0) %>% arrange(desc(funded_tons))
    df <- df %>% mutate(project_type = factor(project_type, levels = rev(project_type)))

    # Color by the full 5-bucket key (same palette as Portfolio Mix Engine's
    # pme_colors), NOT just the 3-category mechanism -- coloring by
    # mechanism alone made avoidance and removal within the same mechanism
    # (e.g. two different "Technology-based" projects) look identical,
    # losing exactly the distinction the 5-bucket system exists to show,
    # and visually disagreeing with this same portfolio's rollup on the
    # Mix Engine tab.
    bucket_display <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )
    df <- df %>% mutate(bucket = bucket_display[key])

    ggplot(df, aes(x = project_type, y = funded_tons, fill = bucket)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = comma(funded_tons)), hjust = -0.15, size = 3.3) +
      coord_flip() +
      scale_fill_manual(values = pme_colors, name = NULL) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
      labs(subtitle = "Recommended tons by project (each bar is one located instance), within budget/supply/concentration caps", x = NULL, y = "Tons (tCO2e)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # User-adjustable tons per methodology, keyed by project_type -- lets
  # someone nudge the optimizer's recommendation up or down before
  # finalizing, per the explicit ask for this capability. Empty until a
  # cell is actually edited; falls back to the optimizer's own
  # funded_tons everywhere an override hasn't been set.
  pce_overrides <- reactiveVal(list())

  observeEvent(input$pce_reset_overrides_btn, {
    pce_overrides(list())
  })

  observeEvent(input$pce_table_cell_edit, {
    info  <- input$pce_table_cell_edit
    alloc <- pce_alloc() %>% arrange(desc(funded_tons))
    req(info$row <= nrow(alloc))
    proj    <- alloc$project_type[info$row]
    new_val <- suppressWarnings(as.numeric(info$value))
    if (is.na(new_val) || new_val < 0) new_val <- 0
    ov <- pce_overrides()
    ov[[proj]] <- new_val
    pce_overrides(ov)
  })

  # Adjusted allocation -- the optimizer's recommendation with any manual
  # overrides applied on top. Cost recalculates from the ADJUSTED tons
  # (not the original solve), so the summary always reflects what's
  # actually on the table right now.
  pce_adjusted <- reactive({
    alloc <- pce_alloc()
    ov <- pce_overrides()
    alloc$your_tons <- alloc$funded_tons
    for (proj in names(ov)) {
      idx <- which(alloc$project_type == proj)
      if (length(idx) == 1) alloc$your_tons[idx] <- ov[[proj]]
    }
    alloc$your_cost <- round(alloc$your_tons * alloc$buyer_price)
    alloc
  })

  output$pce_table <- renderDT({
    alloc <- pce_adjusted()
    df <- alloc %>%
      select(project_type, methodology_name, methodology_code, mechanism, action, geography, state, county,
             applicable_scope, proximity_tier, buyer_price, supply_tons, funded_tons, your_tons, your_cost) %>%
      arrange(desc(funded_tons))
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 12, dom = "tp", scrollX = TRUE),
      colnames = c("Project Name", "Methodology", "Code", "Mechanism", "Action", "Region", "State", "County",
                   "Applicable Scope", "Proximity", "Price ($/t)", "Supply (t)", "Recommended (t)", "Your Tons", "Your Cost ($)"),
      # Only "Your Tons" (column index 13, 0-indexed since rownames=FALSE)
      # is editable -- everything else stays a read-only reference.
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14)))
    )
  })

  output$pce_adjusted_summary <- renderUI({
    alloc     <- pce_adjusted()
    gap_tons  <- attr(pce_alloc(), "gap_tons")
    budget    <- attr(pce_alloc(), "budget")
    total_your_tons <- sum(alloc$your_tons)
    total_your_cost <- sum(alloc$your_cost)
    coverage_pct <- if (gap_tons > 0) round(total_your_tons / gap_tons * 100, 1) else 0
    over_budget  <- total_your_cost > budget

    tags$div(
      style = paste0(
        "background:", if (over_budget) "#FDEDEC" else "#F4F6F7", "; border-radius:6px; ",
        "padding:0.6rem 1rem; margin-top:8px; font-size:13px;"
      ),
      tags$b("Your adjusted mix: "), comma(total_your_tons), " t (", coverage_pct, "% of gap), $", comma(total_your_cost),
      if (over_budget) {
        tags$span(style = "color:#C0392B; font-weight:600;", " -- exceeds your $", comma(budget), " budget")
      } else {
        tags$span(" (budget: $", comma(budget), ")")
      }
    )
  })

  # ---- Methodology-level rollup -- the middle tier between individual
  # projects and buckets. Projects (specific located instances) roll up
  # into their methodology; methodologies roll up into their bucket
  # (already handled by pme_mix()/build_mix_rollup()). Shows which
  # methodologies actually got funded, and how many of their project
  # instances (e.g. "1 of 2") contributed. ----
  output$pce_methodology_table <- renderDT({
    alloc <- pce_adjusted()
    df <- alloc %>%
      group_by(methodology_code, methodology_name, key) %>%
      summarise(
        n_projects_total  = n(),
        n_projects_funded = sum(your_tons > 0),
        your_tons         = sum(your_tons),
        your_cost         = sum(your_cost),
        .groups = "drop"
      ) %>%
      mutate(
        bucket = c(nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
                   tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
                   comm_avoid = "Community-based avoidance")[key],
        projects_used = paste0(n_projects_funded, " of ", n_projects_total)
      ) %>%
      arrange(desc(your_tons)) %>%
      select(methodology_name, methodology_code, bucket, projects_used, your_tons, your_cost)

    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 12, dom = "tp"),
      colnames = c("Methodology", "Code", "Bucket", "Projects Used", "Tons (summed across instances)", "Cost ($)")
    )
  })

  output$pce_dev_priority_plot <- renderPlot({
    alloc <- pce_alloc()
    req(sum(alloc$ideal_tons) > 0)
    df <- alloc %>% filter(ideal_tons > 0) %>% arrange(desc(profit_ideal))
    df <- df %>% mutate(project_type = factor(project_type, levels = rev(project_type)))

    bucket_display <- c(
      nat_avoid = "Nature-based avoidance", nat_removal = "Nature-based removal",
      tech_avoid = "Technology-based avoidance", tech_removal = "Technology-based removal",
      comm_avoid = "Community-based avoidance"
    )
    df <- df %>% mutate(bucket = bucket_display[key])

    ggplot(df, aes(x = project_type, y = profit_ideal, fill = bucket)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = paste0("$", comma(profit_ideal))), hjust = -0.15, size = 3.3) +
      coord_flip() +
      scale_fill_manual(values = pme_colors, name = NULL) +
      scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.2))) +
      labs(
        subtitle = "Total profit potential if the WHOLE gap were served (ignoring budget) -- what's worth originating first",
        x = NULL, y = "Profit potential ($)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$pce_dev_priority_table <- renderDT({
    alloc <- pce_alloc()
    df <- alloc %>%
      select(project_type, mechanism, action, margin_per_ton, ideal_tons, profit_ideal) %>%
      arrange(desc(profit_ideal))
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 12, dom = "tp"),
      colnames = c("Project Type", "Mechanism", "Action", "Margin ($/ton)",
                   "Demand if Fully Served (tons)", "Total Profit Potential ($)")
    )
  })
}

# ======================================================
# PART 3 -- RUN APP
# ======================================================

shinyApp(ui = ui, server = server)
