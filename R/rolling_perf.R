#' @name rolling_perf
#' @title Calculate rolling performance of models
#' @description This function calculates the rolling performance of models on time series data.
#'
#' This function calculates the rolling performance of models on time series data.
#'
#' @param one_aheads A data frame with one-step ahead model predictions.
#' @param series A data frame with time series data that includes 'year' and 'abundance' columns.
#' @param roll_years The number of years for rolling calculation (default is 15).
#' @param screen_metric the metric used to rank and screen top models. options are MAPE or aicc
#' @param mod_include The number of top-performing models to include (default is 25).
#' @param TY_ensemble The number of years for the ensemble analysis (default: 16).
#' @param model_list A data frame with information about each model, including the 'model' column.
#' @param alpha the annual rate of decay per year used in weighting performance in metric  calculation for model selection
#' @return A list with three elements:
#'   - 'all_mods': A data frame with all models' performance metrics.
#'   - 'top_mods': A data frame with the top-performing models' performance metrics.
#'   - 'performance': A summary of the performance metrics for the top models.
#'
#' @importFrom dplyr %>% select filter arrange group_by mutate ungroup summarize desc mutate row_number between lag
#' @import zoo
#' @import lubridate
#' @import tidyr
#' @import tibble
#' @import readr
#' @import stats
#'
#' @export
rolling_perf <- function(one_aheads, series, roll_years, screen_metric, mod_include, TY_ensemble, model_list,alpha=0) {

  out <- one_aheads %>%
    left_join(series %>% dplyr::select(year, abundance)) %>%
    mutate(
      error = abundance - predicted_abundance,
      APE = abs(error / abundance)
    ) %>%
    arrange(model, year) %>%
    group_by(model) %>%
    mutate(
      # MAPE = dplyr::lag(100 * zoo::rollmean(APE, k = roll_years, fill = NA, align = "right")),
      MAPE = dplyr::lag(100 * zoo::rollapply(data=APE, width = roll_years,\(x) weighted.mean(x,if(alpha==0)rep(1,roll_years)else (alpha*(1-alpha)^(roll_years:1))) , fill = NA, align = "right"))


    ) %>%
    group_by(year) %>%
    mutate(
      rank = rank({{screen_metric}}),
      model = as.character(model)
    ) %>% dplyr::ungroup()

  tops <- out %>%
    dplyr::filter(., dplyr::between(year, max(year) - TY_ensemble + 1, max(year)), rank <= mod_include) %>%
    dplyr::left_join(model_list, by = "model") %>%
    dplyr::arrange(dplyr::desc(year), rank)


  # Use the new function for performance metrics calculation
  perf <- tops %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank == 1) %>%
    dplyr::summarize(
      performance_metrics = calculate_performance_metrics(predicted_abundance, abundance)
    ) %>%
    unpack(cols=performance_metrics)

  return(list(
    all_mods = out %>% dplyr::ungroup(),
    top_mods = tops %>% dplyr::ungroup(),
    performance = perf
  ))
}
