#' @name arima_forecast
#' @title ARIMA Forecast Function
#'
#' @description A comprehensive R package for time series analysis, forecasting, and regression modeling.
#'
#' @param tdat The time series data.
#' @param xreg The exogenous variables used in the model.
#' @param xreg_pred The exogenous variables for prediction.
#' @param last_train_yr The last year of training data.
#' @param first_forecast_period The first forecast period.
#' @param write_model_summaries A logical indicating whether to write model summaries.
#' @param train_test A vector or column of the time series data indicating training/test periods.
#' @param freq  Frequency of the time series
#' @param abundance A vector or column of the time series data indicating abundance values.
#'
#' @return A description of the return value.
#' @export
#' @importFrom dplyr if_any filter ungroup select mutate bind_cols pull all_of %>%
#' @importFrom foreach %dopar%
#' @importFrom forecast auto.arima forecast
#' @importFrom tidyr pivot_wider as_tibble
#' @import lubridate
#' @import parallel
#' @import doParallel

arima_forecast <- function(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period, write_model_summaries, train_test, abundance,freq=1,seasonal=FALSE) {
  train_test <- tdat$train_test
  abundance <- tdat$abundance
  value=NULL
  #Point_Forecast<-NULL
  result <- tryCatch(
    {
      if (ncol(xreg) > 0) {
        m1 <- tdat %>%
          dplyr::filter(train_test == 0) %>%
          dplyr::ungroup() %>%
          dplyr::select(abundance) %>%
          unlist() %>%
          ts(frequency = freq) %>%
          forecast::auto.arima(lambda = 0, seasonal = seasonal, xreg = xreg)

        pred <- c(
          m1$fitted,
          forecast::forecast(m1, lambda = 0, xreg = xreg_pred)$mean
        )
        CI <- forecast::forecast(m1, lambda = 0, level = c(50, 95), xreg = xreg_pred) %>%
          tidyr::as_tibble() %>%
          dplyr::select(!`Point Forecast`) %>%
          dplyr::mutate(year = last_train_yr + 1, period = ifelse(first_forecast_period == 2, 2, c(1:freq)))
      } else {
        m1 <- tdat %>%
          dplyr::filter(train_test == 0) %>%
          dplyr::ungroup() %>%
          dplyr::select(abundance) %>%
          unlist() %>%
          ts(frequency = freq) %>%
          forecast::auto.arima(lambda = 0, seasonal = seasonal, xreg = NULL)

        pred <- c(
          m1$fitted,
          forecast::forecast(m1, lambda = 0, h = 1, xreg = NULL)$mean
        )
        CI <- forecast::forecast(m1, lambda = 0, h = 1, level = c(50, 95), xreg = NULL) %>%
          tidyr::as_tibble() %>%
          dplyr::select(!`Point Forecast`) %>%
          dplyr::mutate(year = last_train_yr + 1, period = 1)
      }


      # Calculate performance metrics using the new function
      performance_metrics <- calculate_performance_metrics(pred, tdat$abundance)

      # Append performance metrics to the data frame
      tdat <- tdat %>%
        dplyr::bind_cols(performance_metrics)

      return(list(pred = pred, CI = CI, arma = paste(m1$arma, collapse = ""), aicc = m1$aicc,eq=equatiomatic::extract_eq(m1,use_coefs=TRUE),
                  mod=m1))
      if (write_model_summaries == TRUE) {
        sink("summary.txt", append = TRUE)
        print(summary(m1))
        sink()
      }
    }, error = function(e) {
      message("An error occurred while fitting or forecasting the model.")
      message("Here is the original error message:")
      message(e)
      return(list(
        pred = rep(NA, tdat %>% dplyr::filter(train_test == 0) %>% nrow() + 1),
        CI = rep(NA, 4) %>%
          tidyr::as_tibble() %>%
          dplyr::bind_cols(names = c("Lo 50", "Hi 50", "Lo 95", "Hi 95")) %>%
          dplyr::mutate(value = as.numeric(value)) %>%
          tidyr::pivot_wider(names_from = names, values_from = value) %>%
          dplyr::mutate(year = last_train_yr + 1, period = 1),
        arma = paste(m1$arma, collapse = ""),
        aicc = m1$aicc,
        eq=equatiomatic::extract_eq(m1,use_coefs=TRUE),
        mod=m1
      ))
    }
  )
  return(result)
}
