#' @name do_forecast_multi
#' @title Run do_forecast() across multiple populations and/or ages
#' @description A wrapper around `do_forecast()` that splits an input dataset by
#'   `population` and/or `age` column(s) and runs the full covariate-selection /
#'   one-step-ahead / rolling-performance / ensembling pipeline independently for each
#'   group. Optionally adds a lagged younger-age-class covariate (the observed abundance of
#'   age - 1 in the previous year, useful for forecasting older age classes), restricts
#'   which age classes are actually forecast, and computes a summed-across-group
#'   retrospective performance evaluation (e.g., total run size summed across ages, or a
#'   combined total summed across populations).
#'
#' @param dat A data frame as required by `do_forecast()`, optionally with additional
#'   `population` and/or `age` columns identifying the group each row belongs to. `age` must
#'   be numeric (e.g., 3, 4, 5) so that the next-younger age class can be identified for the
#'   lagged-covariate feature.
#' @param group_vars Character vector of grouping columns to split on. Defaults to whichever
#'   of `c("population", "age")` are present in `dat`. If neither is present, `dat` is run
#'   as a single group (equivalent to calling `do_forecast()` directly). If `dat` has a
#'   `population` and/or `age` column with more than one distinct value that is *not*
#'   included in `group_vars`, it is added automatically (with a warning) -- rows from
#'   distinct populations/ages are never silently combined into a single modeled series.
#'   Use `aggregate_by` to control which of these dimensions get summed together afterward
#'   for the retrospective performance evaluation.
#' @param forecast_ages Optional vector of age values to actually forecast (others are
#'   dropped entirely before modeling). Defaults to all ages present in `dat`. Use this to,
#'   e.g., forecast a youngest age class purely so it is available as a lagged covariate,
#'   without necessarily including it in the summed total (see `total_ages`).
#' @param total_ages Optional vector of age values to include when computing the
#'   summed-across-ages retrospective performance total. Defaults to `forecast_ages`. Must
#'   be a subset of `forecast_ages`. Use this to exclude a youngest age class from the total
#'   while still forecasting it (e.g., to use it as a lagged covariate for older ages).
#' @param add_lag_age_covariate Logical; if `TRUE` (default) and both `population` and
#'   `age` columns are present (or a single, implicit population), a lagged younger-age
#'   covariate is precomputed via `add_lag_age_covariate()` and automatically appended to
#'   the `covariates` argument passed to `do_forecast()` for any age group that has a
#'   younger age class available in the data.
#' @param lag_age_covariate_name Column name to use for the lagged younger-age covariate.
#'   If `NULL` (default), a name is chosen automatically based on
#'   `log_lag_age_covariate` (`"lag1_log_age_minus1"` if `TRUE`, otherwise
#'   `"lag1_age_minus1"`), matching `add_lag_age_covariate()`'s own default naming.
#' @param log_lag_age_covariate Logical; if `TRUE` (default), the lagged younger-age
#'   covariate is log-transformed before use, matching how the abundance response itself is
#'   modeled on the log scale (via `auto.arima(lambda = 0, ...)`) and how other
#'   abundance-derived covariates (e.g. `lag1_log_Jack`) are log-transformed in
#'   `make_dat()`.
#' @param standardize_lag_age_covariate Logical; if `TRUE` (default), the (optionally
#'   logged) lagged covariate is standardized to mean 0 / sd 1 within each `(population,
#'   age)` group, matching the scaling `make_dat()` applies to its other covariates.
#' @param aggregate_by Character vector, a subset of `group_vars`, specifying which
#'   dimension(s) to sum across for the retrospective performance evaluation (e.g., `"age"`
#'   to evaluate a summed total run per population, `"population"` to evaluate a summed
#'   total across populations per age, or both for a grand total). Set to `character(0)` to
#'   skip aggregation. Defaults to all of `group_vars`.
#' @param total_model Which per-group ensemble model to use when building the summed total
#'   series; passed through to `aggregate_group_performance()`. Defaults to `"best"`
#'   (auto-select the top-performing ensemble per group); alternatively supply a fixed
#'   ensemble name (e.g., `"MAPE_weighted"`), a vector of several such names, or `"all"` to
#'   compute the aggregated total/performance for `"best"` plus every ensemble-weighting
#'   method common to all groups, so you can compare summed-total retrospective performance
#'   across weighting schemes (see `aggregate_group_performance()` for the resulting output
#'   shape when more than one `total_model` is used).
#' @param parallel_groups Logical; whether to run the per-group `do_forecast()` calls in
#'   parallel (across groups) rather than sequentially. If `TRUE`, consider setting the
#'   `n_cores` argument passed through `...` to `1` to avoid nested parallelism.
#' @param n_cores_groups Number of parallel workers to use across groups when
#'   `parallel_groups = TRUE`.
#' @param ... Additional arguments passed through to `do_forecast()` for every group (e.g.,
#'   `covariates`, `TY_ensemble`, `slide`, `k`, `min_vars`, `max_vars`, `n_cores`, etc.).
#'
#' @return A list with elements:
#'   - `by_group`: a named list of the full `do_forecast()` output for each group.
#'   - `group_keys`: a data frame giving the grouping-variable values for each element of
#'     `by_group` (same row order).
#'   - `combined_forecasts`: a single tidy data frame of per-year ensemble forecasts across
#'     all groups, tagged with the grouping columns, for easy `dplyr` filtering/plotting.
#'   - `combined_performance`: a single tidy data frame of each group's own (un-aggregated)
#'     retrospective forecast skill, tagged with the grouping columns.
#'   - `aggregated_performance`: the output of `aggregate_group_performance()` (`NULL` if
#'     `aggregate_by` is empty), giving the summed-across-group retrospective performance.
#'
#' @importFrom dplyr ungroup select all_of distinct arrange across everything filter bind_cols bind_rows mutate n_distinct
#' @export
do_forecast_multi <- function(dat,
                              group_vars = intersect(c("population", "age"), names(dat)),
                              forecast_ages = NULL,
                              total_ages = forecast_ages,
                              add_lag_age_covariate = TRUE,
                              lag_age_covariate_name = NULL,
                              log_lag_age_covariate = TRUE,
                              standardize_lag_age_covariate = TRUE,
                              aggregate_by = group_vars,
                              total_model = "best",
                              parallel_groups = FALSE,
                              n_cores_groups = 1,
                              ...) {

  extra_args <- list(...)
  dat <- dplyr::ungroup(dat)

  if (length(group_vars) == 0) {
    message("No 'population' or 'age' columns found (or group_vars supplied); running a single do_forecast() call.")
    result <- do.call(do_forecast, c(list(dat = dat), extra_args))
    return(list(
      by_group = list(all = result),
      group_keys = data.frame(group = "all"),
      combined_forecasts = NULL,
      combined_performance = NULL,
      aggregated_performance = NULL
    ))
  }

  # Safety net: never let splitting silently omit a real identifying dimension. If `dat`
  # contains a `population` and/or `age` column with more than one distinct value, but the
  # caller's `group_vars` doesn't include it, rows from multiple independent time series
  # would get silently combined into a single series per (remaining) group -- corrupting
  # model fitting (duplicate years, mismatched observations) instead of raising a clear
  # error. Auto-include it (with a warning) so every group modeled is always a single,
  # genuine time series; use `aggregate_by` (which defaults to `group_vars`, so this
  # addition flows through automatically) to control which dimension(s) get summed for
  # retrospective performance -- e.g. `group_vars = "age"` plus multiple populations in
  # `dat` will still model each (population, age) combination separately, and then sum
  # across populations per age for the retrospective total.
  for (v in c("population", "age")) {
    if (v %in% names(dat) && !(v %in% group_vars) && dplyr::n_distinct(dat[[v]]) > 1) {
      warning("`dat` contains multiple distinct '", v, "' values but '", v, "' was not in ",
              "`group_vars`; adding it automatically so each population/age combination is ",
              "modeled as its own series (use `aggregate_by` to control which dimension(s) ",
              "are summed for retrospective performance).", call. = FALSE)
      group_vars <- c(group_vars, v)
    }
  }

  has_age <- "age" %in% group_vars
  has_population <- "population" %in% group_vars

  # ---- 1. Precompute lagged younger-age covariate (pure preprocessing) ----
  # IMPORTANT: this must happen *before* restricting `dat` to `forecast_ages` below, so
  # that a younger age class kept only as a covariate source (e.g. age 2 used to forecast
  # age 3, but not itself forecast) is still available to compute the lag from -- even
  # though it will be dropped from the set of ages actually modeled in step 2.
  lag_added <- FALSE
  temp_population_col <- FALSE
  if (is.null(lag_age_covariate_name)) {
    lag_age_covariate_name <- if (log_lag_age_covariate) "lag1_log_age_minus1" else "lag1_age_minus1"
  }
  if (add_lag_age_covariate && has_age) {
    if (!has_population) {
      # single implicit population: add a constant placeholder so the join logic works,
      # then drop it again before returning/using dat downstream.
      dat$.tmp_population <- "all"
      population_col <- ".tmp_population"
      temp_population_col <- TRUE
    } else {
      population_col <- "population"
    }

    dat <- add_lag_age_covariate(dat,
                                 population_var = population_col,
                                 age_var = "age",
                                 lag_name = lag_age_covariate_name,
                                 log_transform = log_lag_age_covariate,
                                 standardize = standardize_lag_age_covariate)
    lag_added <- TRUE

    if (temp_population_col) {
      dat$.tmp_population <- NULL
    }
  }

  # ---- 2. Restrict to the age classes we actually want to forecast ----
  # (done *after* the lag covariate is computed -- see note above)
  if (has_age) {
    all_ages <- sort(unique(dat$age))
    if (is.null(forecast_ages)) forecast_ages <- all_ages
    if (is.null(total_ages)) total_ages <- forecast_ages
    if (!all(total_ages %in% forecast_ages)) {
      stop("`total_ages` must be a subset of `forecast_ages` (an age can't be included in ",
           "the total unless it is also forecast).")
    }
    dat <- dat %>% dplyr::filter(age %in% forecast_ages)
  }

  # ---- 3. Enumerate groups ----
  group_keys <- dat %>%
    dplyr::select(dplyr::all_of(group_vars)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::across(dplyr::everything()))

  group_names <- apply(group_keys, 1, function(r) paste(r, collapse = "."))

  run_one_group <- function(i) {
    key_row <- group_keys[i, , drop = FALSE]
    sub_dat <- dat
    for (v in group_vars) {
      sub_dat <- sub_dat[sub_dat[[v]] == key_row[[v]][1], , drop = FALSE]
    }

    group_extra_args <- extra_args
    if (lag_added) {
      has_younger <- lag_age_covariate_name %in% names(sub_dat) &&
        any(!is.na(sub_dat[[lag_age_covariate_name]]))
      if (has_younger && !is.null(group_extra_args$covariates) &&
          !(lag_age_covariate_name %in% group_extra_args$covariates)) {
        group_extra_args$covariates <- c(group_extra_args$covariates, lag_age_covariate_name)
      }
    }

    do.call(do_forecast, c(list(dat = sub_dat), group_extra_args))
  }

  if (parallel_groups) {
    cl <- parallel::makeCluster(n_cores_groups)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    i <- NULL # avoid R CMD check NOTE for foreach's local binding
    results_list <- foreach::foreach(i = seq_len(nrow(group_keys)),
                                     .packages = c("salmonforecast", "dplyr", "forecast")) %dopar% {
      run_one_group(i)
    }
  } else {
    results_list <- lapply(seq_len(nrow(group_keys)), run_one_group)
  }
  names(results_list) <- group_names

  # ---- 4. Combine per-group forecasts/performance into tidy tibbles ----
  combined_forecasts <- dplyr::bind_rows(lapply(seq_along(results_list), function(i) {
    key <- group_keys[i, , drop = FALSE]
    ens <- results_list[[i]]$ens$ensembles
    dplyr::bind_cols(key[rep(1, nrow(ens)), , drop = FALSE], ens)
  }))

  combined_performance <- dplyr::bind_rows(lapply(seq_along(results_list), function(i) {
    key <- group_keys[i, , drop = FALSE]
    fs <- results_list[[i]]$ens$forecast_skill
    dplyr::bind_cols(key[rep(1, nrow(fs)), , drop = FALSE], fs)
  }))

  # ---- 5. Summed-across-group retrospective performance ----
  aggregated_performance <- NULL
  if (length(aggregate_by) > 0) {
    aggregated_performance <- aggregate_group_performance(
      results_list = results_list,
      group_keys = group_keys,
      group_vars = group_vars,
      aggregate_by = aggregate_by,
      total_ages = if (has_age) total_ages else NULL,
      total_model = total_model
    )
  }

  list(
    by_group = results_list,
    group_keys = group_keys,
    combined_forecasts = combined_forecasts,
    combined_performance = combined_performance,
    aggregated_performance = aggregated_performance
  )
}
