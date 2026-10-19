# validate_contract.R -- single source of truth for the data contract between
# the pipelines and the Shiny app (review: "no schema validation").
#
#   source("validate_contract.R")
#   validate_contract("shiny_data")            # one folder (call at the END of each pipeline)
#   validate_contract_all(".")                 # every folder that exists (called at app startup)
#
# Fails loudly (stop()) when a REQUIRED table is missing or lacks a required
# column / settings field. Optional tables only produce a warning.
# Column lists were taken from what the pipelines actually write, and only
# include columns the app depends on -- extra columns are always fine.

.contract <- list(
  shiny_data = list(
    ghgp_panel_filtered = c("facility_id", "year", "emissions"),
    future_pred         = c("facility_id", "year", "p50"),
    target_pred         = character(0),
    target_lookup       = character(0),
    hist_by_sector      = character(0),
    fore_by_sector      = character(0),
    target_by_sector    = character(0),
    facility_lookup     = c("facility_id", "facility_name", "company", "emissions_2023"),
    settings            = character(0)
  ),
  shiny_data_eu = list(
    eu_panel_filtered   = c("facility_id", "facility_name", "country", "activity_code", "year", "emissions"),
    facility_lookup_eu  = c("facility_id", "facility_name", "country", "sector_bucket", "emissions_latest"),
    future_pred_eu      = character(0),
    hist_by_country_eu  = character(0),
    hist_by_sector_eu   = character(0),
    hist_by_sector_bucket_eu = character(0),
    fore_by_sector_bucket_eu = character(0),
    settings_eu         = character(0)
  ),
  shiny_data_asia = list(
    asia_panel_filtered = c("company_id", "company_name", "country", "sector", "sector_bucket", "year"),
    future_pred_asia    = c("company_id", "year", "p50"),
    company_lookup_asia = c("company_id", "company_name", "country", "sector", "sector_bucket"),
    hist_by_country_asia = character(0), hist_by_sector_asia = character(0),
    settings_asia       = character(0)
  ),
  shiny_data_africa = list(
    africa_panel_filtered = c("company_id", "company_name", "country", "sector", "sector_bucket", "year"),
    company_lookup_africa = c("company_id", "company_name", "country", "sector", "sector_bucket"),
    hist_by_country_africa = character(0), hist_by_sector_africa = character(0),
    settings_africa     = character(0)
  ),
  shiny_data_australia = list(
    australia_panel_filtered = c("company_id", "company_name", "country", "sector", "sector_bucket", "year"),
    future_pred_australia    = c("company_id", "year", "p50"),
    company_lookup_australia = c("company_id", "company_name", "country", "sector", "sector_bucket"),
    hist_by_country_australia = character(0), hist_by_sector_australia = character(0),
    settings_australia = character(0)
  ),
  shiny_data_latam = list(
    chile_panel_filtered = c("company_id", "company_name", "country", "sector", "sector_bucket", "year", "region"),
    future_pred_chile    = c("company_id", "year", "p50"),
    company_lookup_chile = c("company_id", "company_name", "country", "sector", "sector_bucket", "region"),
    hist_by_country_chile = character(0), hist_by_sector_chile = character(0),
    settings_chile       = character(0)
  )
)

# Required fields inside each settings.rds (a list)
.contract_settings <- list(
  settings_eu        = c("sector_bucket_list"),
  settings_asia      = c("country_list", "sector_list", "sector_bucket_list"),
  settings_africa    = c("country_list", "sector_list", "sector_bucket_list"),
  settings_australia = c("country_list", "sector_list", "sector_bucket_list"),
  settings_chile     = c("country_list", "sector_list", "sector_bucket_list")
)

# Optional US tables: absence only warns (features degrade, app still runs)
.contract_optional <- c(
  "hist_by_sector_state", "fore_by_sector_state", "backtest_by_facility", "backtest_by_sector",
  "sbti_target_pred", "sbti_company_lookup", "sbti_sector_benchmark", "sbti_sector_targets",
  "sbti_big_sector_summary", "sector_venn_membership", "ghgrp_industry_reference",
  "ghgrp_subpart_presence", "ghgrp_sector_counts", "ghgrp_actual_goals", "ghgrp_non_sbti_goals"
)

validate_contract <- function(dir, stop_on_error = TRUE) {
  nm <- basename(normalizePath(dir, mustWork = FALSE))
  spec <- .contract[[nm]]
  if (is.null(spec)) stop("validate_contract: unknown data folder '", nm, "'")
  problems <- character(0)
  for (tbl in names(spec)) {
    f <- file.path(dir, paste0(tbl, ".rds"))
    if (!file.exists(f)) { problems <- c(problems, paste0(nm, "/", tbl, ".rds is MISSING")); next }
    x <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(x)) { problems <- c(problems, paste0(nm, "/", tbl, ".rds cannot be read")); next }
    need <- spec[[tbl]]
    if (length(need) > 0) {
      miss <- setdiff(need, names(x))
      if (length(miss) > 0)
        problems <- c(problems, paste0(nm, "/", tbl, ".rds lacks column(s): ", paste(miss, collapse = ", ")))
    }
    if (tbl %in% names(.contract_settings)) {
      miss <- setdiff(.contract_settings[[tbl]], names(x))
      if (length(miss) > 0)
        problems <- c(problems, paste0(nm, "/", tbl, ".rds lacks field(s): ", paste(miss, collapse = ", ")))
    }
  }
  if (nm == "shiny_data") {
    absent <- .contract_optional[!file.exists(file.path(dir, paste0(.contract_optional, ".rds")))]
    if (length(absent) > 0)
      warning("validate_contract: optional tables absent (features degrade): ", paste(absent, collapse = ", "), call. = FALSE)
  }
  if (length(problems) > 0 && stop_on_error)
    stop("DATA CONTRACT VIOLATION in '", nm, "':\n  - ", paste(problems, collapse = "\n  - "),
         "\nRe-run the matching pipeline or restore the correct data folder.", call. = FALSE)
  invisible(problems)
}

# App startup: shiny_data is mandatory; every other region folder is checked
# only if it exists (a missing region folder just means that region is off).
validate_contract_all <- function(root = ".") {
  validate_contract(file.path(root, "shiny_data"))
  for (d in setdiff(names(.contract), "shiny_data"))
    if (dir.exists(file.path(root, d))) validate_contract(file.path(root, d))
  invisible(TRUE)
}
