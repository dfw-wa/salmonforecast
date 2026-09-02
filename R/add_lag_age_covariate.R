#' @name add_lag_age_covariate
#' @title Add a lagged younger-age-class covariate
#' @description For datasets containing returns of the same population at multiple ages,
#'   this adds a covariate column giving the observed abundance of the next-younger age
#'   class in the previous year (e.g., the age-3 return in 2025 as a covariate for
#'   forecasting the age-4 return in 2026). This uses only already-observed values (never
#'   a forecast), so it can be computed once as a simple preprocessing/join step before any
#'   modeling occurs.
#'
#'   To match the package's usual convention of log-transforming and standardizing
#'   abundance-derived covariates (see `make_dat()`), the covariate is by default
#'   log-transformed and then standardized (mean 0, sd 1) *within each (population, age)
#'   group* -- i.e. scaled the same way it will actually be seen by the model fit to that
#'   group, rather than across the whole multi-population/multi-age dataset.
#'
#' @param dat A data frame containing (at minimum) `population`, `age`, `year`, and
#'   `abundance` columns (column names configurable via the arguments below).
#' @param population_var Name of the population identifier column.
#' @param age_var Name of the (numeric) age column. Ages must be numeric/orderable so that
#'   "age - 1" identifies the next-younger age class.
#' @param year_var Name of the year column.
#' @param abundance_var Name of the observed abundance column.
#' @param lag_name Name to give the new lagged-covariate column. If `NULL` (default), a
#'   name is chosen automatically based on `log_transform` (`"lag1_log_age_minus1"` if
#'   `TRUE`, otherwise `"lag1_age_minus1"`).
#' @param log_transform Logical; if `TRUE` (default), the lagged abundance is log-transformed
#'   before attaching (and before standardizing), matching how other abundance-derived
#'   covariates (e.g. `lag1_log_Jack`) are handled in `make_dat()`.
#' @param standardize Logical; if `TRUE` (default), the (optionally logged) lagged covariate
#'   is standardized to mean 0 / sd 1 separately within each `(population, age)` group, so
#'   that each age-specific model sees the covariate scaled the same way `make_dat()` scales
#'   its other covariates.
#'
#' @return `dat` with one additional column (named by `lag_name`) giving the (optionally
#'   log-transformed and standardized) observed abundance of age-1 (within the same
#'   population) in year-1. Rows for the youngest age class present (or any row lacking a
#'   same-population age-1/year-1 observation) will have `NA` for this column.
#'
#' @importFrom dplyr select all_of distinct transmute left_join group_by mutate across ungroup
#' @importFrom rlang .data :=
#' @export
add_lag_age_covariate <- function(dat,
                                  population_var = "population",
                                  age_var = "age",
                                  year_var = "year",
                                  abundance_var = "abundance",
                                  lag_name = NULL,
                                  log_transform = TRUE,
                                  standardize = TRUE) {

  required <- c(population_var, age_var, year_var, abundance_var)
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols) > 0) {
    stop("add_lag_age_covariate requires columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(dat[[age_var]])) {
    stop("`", age_var, "` must be numeric so that the next-younger age class (age - 1) can be identified.")
  }

  if (is.null(lag_name)) {
    lag_name <- if (log_transform) "lag1_log_age_minus1" else "lag1_age_minus1"
  }

  if (lag_name %in% names(dat)) {
    stop("`", lag_name, "` already exists in `dat`; choose a different `lag_name` or drop the existing column.")
  }

  # Build a lookup keyed on (population, age + 1, year + 1) so that joining on
  # (population, age, year) attaches the *younger* age class's abundance from the
  # *previous* year onto each row.
  lookup <- dat %>%
    dplyr::select(dplyr::all_of(c(population_var, age_var, year_var, abundance_var))) %>%
    dplyr::distinct() %>%
    dplyr::transmute(
      !!population_var := .data[[population_var]],
      !!age_var := .data[[age_var]] + 1,
      !!year_var := .data[[year_var]] + 1,
      !!lag_name := if (log_transform) log(.data[[abundance_var]]) else .data[[abundance_var]]
    )

  out <- dat %>%
    dplyr::left_join(lookup, by = c(population_var, age_var, year_var))

  if (standardize) {
    # Scale within each (population, age) group so an older age's model sees this
    # covariate standardized against only its own series, the same way it will actually
    # be modeled once the data are split by group (e.g. in do_forecast_multi()).
    out <- out %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(population_var, age_var)))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(lag_name), ~ as.numeric(scale(.x)))) %>%
      dplyr::ungroup()
  }

  out
}
