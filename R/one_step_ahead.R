#' @name one_step_ahead
#' @title Function to evaluate the performance of ARIMA model (produces season total forecasts only)
#' @description This function evaluates the performance of an ARIMA model for season total forecasts using provided covariates and other parameters.
#'
#' @param series A data frame containing the time series data.
#' @param leave_yrs Number of years to leave out during the evaluation.
#' @param TY_ensemble A parameter for ensemble evaluation.
#' @param covariates A list of covariates to include in the model.
#' @param first_forecast_period The first period for forecasting.
#' @param ts_freq frequency of time series being forecasts
#' @param seasonal whether the arima models should include seasonal  components
#' @param plot_results A logical value to indicate whether to plot results.
#' @param write_model_summaries A logical value to indicate whether to write model summaries.
#' @param forecast_period_start_m The starting month for the forecast period (inclusive).
#' @param n_cores
#' @param include_mod
#' @param forecast_period_start_d The starting day for the forecast period (inclusive).
#'
#' @return A data frame containing forecasts and other information.
#'
#' @export
#'
#' @import doParallel
#' @import foreach
#' @importFrom dplyr select filter mutate ungroup pull bind_cols left_join rename bind_rows %>%
#' @importFrom utils write.table tail
#' @importFrom doRNG %dorng%
#' @import forecast
one_step_ahead <- function(series,
                           leave_yrs,
                           TY_ensemble,
                           covariates,
                           first_forecast_period,
                           ts_freq=1,
                           seasonal=FALSE,
                           plot_results,
                           write_model_summaries,
                           forecast_period_start_m, # inclusive
                           forecast_period_start_d, # inclusive
                           n_cores,
                           include_mod=FALSE
) {


  start <- Sys.time()

  if (write_model_summaries == TRUE) {
    write.table(NULL, "summary.txt")
  }


  filtered_series<-series%>%
    ungroup()%>%
    dplyr::select(year,species,period,abundance,all_of(unique(unlist(covariates))))%>%
    filter(
      if_all(
        all_of(unique(unlist(covariates))),
        ~ !is.na(.x)
      )
    )


  #cl <- makeCluster(parallel::detectCores() - 3)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  set.seed(123)
  out<-foreach::foreach(i = 1:leave_yrs, .combine = 'rbind',
                   .packages = c("forecast", "dplyr", "tibble", "SalmonForecasting")) %dorng% {



    forecasts_out<-NULL
    i <- i  # Define 'i' within the foreach loop

    for (c in 1:length(covariates)) {
      last_train_yr <- max(filtered_series$year) - (leave_yrs - i + 1)
      tdat <- filtered_series %>%
        dplyr::filter(year <= (last_train_yr + 1)) %>%
        dplyr::mutate(train_test = ifelse(year > last_train_yr & period >= first_forecast_period, 1, 0)
        )

      xreg <- tdat %>%
        dplyr::filter(train_test == 0) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(covariates[[c]])) %>%
        as.matrix()

      xreg_pred <- tdat %>%
        dplyr::filter(train_test == 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::all_of(covariates[[c]])) %>%
        as.matrix()

      temp <- NULL

      # Use tryCatch to handle potential errors during ARIMA model fitting
      tryCatch(
        {
          temp <- SalmonForecasting:::arima_forecast(tdat, xreg, xreg_pred, last_train_yr, first_forecast_period,freq=ts_freq,seasonal=seasonal)
          pred <- temp$pred %>% tail(1)
          CI <- temp$CI

          tdat <- tdat %>%
            dplyr::filter(train_test == 1) %>%
            dplyr::select(c("year", "period")) %>%
            dplyr::bind_cols(data.frame(
              predicted_abundance = pred,
              arma = temp$arma,
              aicc = temp$aicc))

          if(include_mod){
            tdat<-tdat %>%dplyr::bind_cols(tibble::tibble(
              mod=list(mod=temp$mod),
              eq=temp$eq))
          }


          tdat<-tdat %>%
            dplyr::left_join(CI, by = c("year", "period")) %>%
            dplyr::mutate(model = as.character(c))


          if (c == 1) {
            forecasts_out <- tdat
          } else {
            forecasts_out <- forecasts_out %>% dplyr::bind_rows(tdat)
          }


          # Calculate performance metrics using the new function
          performance_metrics <- SalmonForecasting:::calculate_performance_metrics(pred, tdat$abundance)

          # Append performance metrics to the data frame
          tdat <- tdat %>%
            dplyr::bind_cols(performance_metrics)


        }, error = function(e) {
          # Handle errors (e.g., model not suitable)
          print(paste("Error in ARIMA model for covariate", c, "- Skipping:", e$message))
        })
    }
    return(forecasts_out)
  }

  stopCluster(cl)

  print((Sys.time() - start))
  return(out)
}
