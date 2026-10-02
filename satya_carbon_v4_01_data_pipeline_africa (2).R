# ======================================================
# SATYA CARBON -- AFRICA PIPELINE (South Africa)
# DATA PIPELINE: load real CDP-disclosed company emissions for South
# Africa, clean them, compute sector aggregations, export tables for
# the dashboard.
#
# GENUINELY DIFFERENT from the Asia pipeline, not just a copy with
# names changed -- confirmed by direct inspection of the source file:
#  - SINGLE YEAR ONLY (2018). This is a CDP disclosure snapshot, not a
#    multi-year panel. There is no trend to fit and NO forecast model
#    is built here -- attempting one would mean inventing a trajectory
#    from a single data point, which is not a real projection.
#  - TWO geographic scopes per company: "South Africa" (local
#    operations) and "Global" (worldwide total). Scope 1 and Scope 2
#    both have this split; Scope 3 is GLOBAL ONLY in this source (no
#    South-Africa-specific Scope 3 figure exists to extract).
#  - Sparse coverage: 109 companies in the raw file, but many rows
#    only have CDP letter-grade scores (2014-2018), not actual
#    emissions numbers -- only 64 companies have any usable Scope 1
#    figure (SA-specific or Global).
#
# SCOPE 1/2 VALUE CHOICE: South-Africa-specific figure preferred when
# available (59 of 64 companies have it), falling back to the Global
# figure only for the 5 companies that report Global-only. This is
# the locally-relevant number for a South-Africa-focused emissions
# gap/offsetting decision, consistent with what the app's Africa
# section is actually about. Scope 3 uses Global (the only option in
# this source) -- labeled as such downstream, not implied to be
# South-Africa-specific when it isn't.
#
# SANITY-CHECKED, not assumed clean: the largest values (Eskom ~205M
# tCO2e Scope 1, Sasol ~57M tCO2e) were checked against what's
# publicly known about these companies -- Eskom is South Africa's
# national utility, one of the world's largest coal-fired generators,
# and Sasol is one of the world's largest single-site CO2 emitters
# via coal-to-liquids. Both numbers are plausible, unlike the India
# pipeline's outliers, which is why NO sanity ceiling is applied here
# -- one wasn't needed for this file, confirmed by inspection rather
# than assumed either way.
# ======================================================

setwd("D:/carbon final")
rm(list = ls())

library(tidyverse)
library(readxl)

africa_path <- "CDP_South_Africa_2018_Company_Data.xlsx"

# ======================================================
# PART 1 -- LOAD AND CLEAN
# ======================================================

cat("=== PART 1: LOADING SOUTH AFRICA DATA ===\n")

sa_raw <- read_excel(africa_path, sheet = 1)
cat("Raw rows:", nrow(sa_raw), "\n")

sa_clean <- sa_raw %>%
  rename(
    company_name = Company, sector = Sector,
    s1_sa = `Scope 1 South Africa (tCO2e)`, s1_global = `Scope 1 Global (tCO2e)`,
    s2_sa = `Scope 2 South Africa (tCO2e)`, s2_global = `Scope 2 Global (tCO2e)`,
    s3_global = `Scope 3 Global (tCO2e)`
  ) %>%
  mutate(
    # Forced to numeric explicitly -- confirmed by direct cell-type
    # inspection that 5 companies (BHP Billiton, Capital & Counties
    # Properties, Compagnie Financiere Richemont SA, Hammerson, Intu
    # Properties plc) have their *_global figures stored as
    # TEXT-formatted numbers in the source Excel file (the cell
    # literally contains the string "10430000", not the number).
    # readxl types a column as character as soon as any cell in it is
    # text, which silently broke coalesce() below (mixing <double> and
    # <character>) -- pandas hid this same issue during the original
    # Python-side data exploration by coercing more permissively.
    # as.numeric() here converts these number-as-text cells correctly;
    # it does not lose or alter any genuine value.
    s1_sa = as.numeric(s1_sa), s1_global = as.numeric(s1_global),
    s2_sa = as.numeric(s2_sa), s2_global = as.numeric(s2_global),
    s3_global = as.numeric(s3_global),
    country = "South Africa",
    year = 2018L,
    # South-Africa-specific preferred; Global as fallback only when SA
    # figure itself is missing -- NOT summed together (that would
    # double-count the SA operations already included in Global).
    scope1 = coalesce(s1_sa, s1_global),
    scope2 = coalesce(s2_sa, s2_global),
    scope3 = s3_global,  # no SA-specific Scope 3 exists in this source
    company_id = paste0("ZA-", row_number())
  ) %>%
  filter(!is.na(scope1) | !is.na(scope2) | !is.na(scope3)) %>%
  select(country, company_id, company_name, sector, year, scope1, scope2, scope3)

cat("Companies with at least one usable scope value:", nrow(sa_clean), "\n")
cat("By sector:\n")
print(sa_clean %>% count(sector, sort = TRUE))

# ======================================================
# PART 2 -- SECTOR BUCKETING (>=5 companies to get its own bucket --
# a lower bar than Asia's >=10, since this dataset is much smaller;
# chosen from this data's own actual sector sizes, not copied from
# Asia's threshold without checking) ----
# ======================================================

sector_counts <- sa_clean %>% distinct(sector, company_id) %>% count(sector)
top_sectors <- sector_counts %>% filter(n >= 5) %>% pull(sector)

africa_panel_filtered <- sa_clean %>%
  mutate(
    sector_bucket = if_else(sector %in% top_sectors, sector, "Other"),
    sector_bucket = factor(sector_bucket),
    sector_bucket = relevel(sector_bucket, ref = "Other"),
    country = factor(country)
  )

cat("Final panel rows:", nrow(africa_panel_filtered), "\n")

# ======================================================
# PART 3 -- AGGREGATIONS
# ======================================================

cat("=== PART 3: BUILDING AGGREGATIONS ===\n")

# Single year, so this is a one-row-per-country total, not a time
# series -- kept as its own table for structural consistency with the
# Asia pipeline's output shape, not because a trend exists here.
hist_by_country_africa <- africa_panel_filtered %>%
  group_by(country, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), .groups = "drop")

hist_by_sector_africa <- africa_panel_filtered %>%
  group_by(sector, year) %>%
  summarise(emissions = sum(scope1, na.rm = TRUE), n_companies = n_distinct(company_id), .groups = "drop") %>%
  filter(n_companies >= 3)

company_lookup_africa <- africa_panel_filtered %>%
  distinct(company_id, company_name, country, sector, sector_bucket) %>%
  arrange(company_name)

settings_africa <- list(
  country_list = sort(unique(as.character(africa_panel_filtered$country))),
  sector_list = sort(unique(as.character(africa_panel_filtered$sector))),
  sector_bucket_list = sort(unique(as.character(africa_panel_filtered$sector_bucket))),
  data_year = 2018L,
  has_forecast = FALSE  # explicit flag -- single-year source, no fitted model exists; the app reads this rather than assuming
)

# ======================================================
# PART 4 -- EXPORT
# ======================================================

cat("=== PART 4: EXPORTING ===\n")

out_dir <- "shiny_data_africa"
if (!dir.exists(out_dir)) dir.create(out_dir)

saveRDS(africa_panel_filtered,   file.path(out_dir, "africa_panel_filtered.rds"))
saveRDS(hist_by_country_africa,  file.path(out_dir, "hist_by_country_africa.rds"))
saveRDS(hist_by_sector_africa,   file.path(out_dir, "hist_by_sector_africa.rds"))
saveRDS(company_lookup_africa,   file.path(out_dir, "company_lookup_africa.rds"))
saveRDS(settings_africa,         file.path(out_dir, "settings_africa.rds"))

cat("Done. Files written to", out_dir, "\n")
cat("NOTE: no future_pred_africa.rds -- single-year source, no forecast model fitted.\n")
