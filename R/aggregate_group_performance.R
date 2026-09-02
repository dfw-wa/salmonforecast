#' @name aggregate_group_performance
#' @title Aggregate forecasts across a grouping dimension and evaluate retrospective performance
#' @description Given a set of per-group `do_forecast()` outputs (e.g., one per age class or
#'   one per population), this sums a chosen forecast series (predicted and observed
#'   abundance) across one or more grouping dimensions (e.g., summing individual ages up to
#'   a total run, or summing individual populations up to an aggregate), and then computes
#'   retrospective performance metrics (via `calculate_performance_metrics()`) on that summed
#'   series. This supports both the "sum ages to a total" use case and the "sum populations"
#'   use case (or both at once) with the same code path.
#'
#' @param results_list A named list of `do_forecast()` outputs, one element per group
#'   (as produced by `do_forecast_multi()`).
#' @param group_keys A data frame with one row per element of `results_list` (in the same
#'   order), giving the grouping-variable values (e.g., `population`, `age`) for that group.
#' @param group_vars Character vector naming the grouping columns present in `group_keys`
#'   (e.g., `c("population", "age")`).
#' @param aggregate_by Character vector, a subset of `group_vars`, naming the dimension(s)
#'   to sum across (e.g., `"age"` to sum ages into a total per population, `"population"` to
#'   sum populations into a total per age, or `c("age","population")` for a grand total).
#' @param total_ages Optional vector of age values to include when summing. Ages not in
#'   this vector are dropped before summation (only relevant when `"age" %in% group_vars`).
#'   Use this to exclude, e.g., the youngest age class from the summed total while still
#'   forecasting it (so it can be used as a lagged covariate for older ages).
#' @param total_model Which per-group ensemble model's forecast series to sum. Either
#'   `"best"` (default) to auto-select, for each group, the ensemble-weighted model with the
#'   lowest retrospective MAPE, or a specific ensemble model name (e.g., `"MAPE_weighted"`)
#'   to use the same ensemble type across all groups (recommended if you want the summed
#'   total to be based on a consistent method across groups).
#'
#' @return A list with elements:
#'   - `series`: the summed year-by-year predicted/observed abundance, retaining any
#'     grouping columns not summed over.
#'   - `performance`: performance metrics (from `calculate_performance_metrics()`) computed
#'     on `series`, one row per remaining group combination (or a single row if fully summed).
#'   - `aggregated_over`, `total_ages`, `total_model`: echoed inputs, for reference.
#'
#' @importFrom dplyr filter select distinct bind_cols bind_rows across all_of group_by summarise ungroup n group_modify arrange slice pull
#' @export
aggregate_group_performance <- function(results_list,
                                        group_keys,
                                        group_vars,
                                        aggregate_by = group_vars,
                                        total_ages = NULL,
                                        total_model = "best") {

  if (!all(aggregate_by %in% group_vars)) {
    stop("`aggregate_by` must be a subset of `group_vars`.")
  }
  if (length(results_list) != nrow(group_keys)) {
    stop("`results_list` and `group_keys` must have the same number of groups.")
  }

  extract_group_series <- function(result, total_model) {
    fs <- result$ens$forecast_skill %>% dplyr::filter(grepl("weight", model))
    if (nrow(fs) == 0) {
      stop("No ensemble-weighted models found in a group's forecast_skill; cannot extract a total series.")
    }

    chosen_model <- if (identical(total_model, "best")) {
      fs %>% dplyr::arrange(MAPE) %>% dplyr::slice(1) %>% dplyr::pull(model)
    } else {
      if (!total_model %in% fs$model) {
        stop("`total_model` = '", total_model, "' not found among ensemble models: ",
             paste(fs$model, collapse = ", "))
      }
      total_model
    }

    pred <- result$ens$ensembles %>%
      dplyr::filter(model == chosen_model) %>%
      dplyr::select(year, predicted_abundance) %>%
      dplyr::distinct()

    obs <- result$rp$all_mods %>%
      dplyr::select(year, abundance) %>%
      dplyr::distinct()

    dplyr::left_join(pred, obs, by = "year")
  }

  series_list <- lapply(seq_along(results_list), function(i) {
    key <- group_keys[i, , drop = FALSE]
    s <- extract_group_series(results_list[[i]], total_model = total_model)
    dplyr::bind_cols(key[rep(1, nrow(s)), , drop = FALSE], s)
  })
  all_series <- dplyr::bind_rows(series_list)

  # Drop ages excluded from the total (e.g., a youngest age kept only to serve as a
  # lagged covariate for older ages, but not itself wanted in the summed total).
  if ("age" %in% group_vars && !is.null(total_ages)) {
    all_series <- all_series %>% dplyr::filter(age %in% total_ages)
  }

  keep_vars <- setdiff(group_vars, aggregate_by)

  summed <- all_series %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keep_vars, "year")))) %>%
    dplyr::summarise(
      predicted_abundance = sum(predicted_abundance, na.rm = FALSE),
      abundance = sum(abundance, na.rm = FALSE),
      n_groups_summed = dplyr::n(),
      .groups = "drop"
    )

  perf <- if (length(keep_vars) == 0) {
    calculate_performance_metrics(summed$predicted_abundance, summed$abundance)
  } else {
    summed %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keep_vars))) %>%
      dplyr::group_modify(~ calculate_performance_metrics(.x$predicted_abundance, .x$abundance)) %>%
      dplyr::ungroup()
  }

  list(
    series = summed,
    performance = perf,
    aggregated_over = aggregate_by,
    total_ages = total_ages,
    total_model = total_model
  )
}
