#' @name ensemble
#' @title Ensemble Forecasting Function
#' @description A function that generates ensemble forecasts based on input forecasts and series data.
#'
#' @param forecasts A data frame containing model forecasts.
#' @param series A data frame containing observed time series data.
#' @param TY_ensemble The number of years to consider for ensemble generation.
#' @param k A parameter for weighting in the ensemble generation.
#' @param slide The length of the sliding window for calculating ensemble weights
#' @param stretch Boolean whether to stretch rather than slide
#' @param num_models The number of top models to consider in each iteration.
#' @param do_stacking whether to include stacking model weights
#' @param stack_metric The metric to use for stacking weights.
#'
#' @return A list containing final model weights, forecast skill evaluation, ensembles, and updated forecasts.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr pluck
#' @importFrom scales percent
#' @importFrom dplyr summarise filter left_join select mutate group_by arrange bind_rows ungroup pull
#' @importFrom MCMCpack rdirichlet
#' @export
ensemble <- function(forecasts, series, TY_ensemble, k, min_ens_yrs=5, slide, num_models, stack_metric, stretch = FALSE,do_stacking=TRUE,alpha=0,num_stack_it=500) {

  yrrange <- forecasts %>%
    dplyr::summarise(minyr = min(year), maxyr = max(year)) %>%
    unlist()

  maxdata_year <- forecasts %>%
    dplyr::filter(!is.na(abundance)) %>%
    dplyr::summarise(max(year)) %>%
    unlist() %>%
    purrr::pluck()

  ensembles <- NULL
  for (i in (yrrange[2] - TY_ensemble):maxdata_year) {




    if (stretch) {
      # Determine the min_ens_yrs based on the current year (i) and TY_ensemble
      ens_yrs  <- i - (yrrange[2] - TY_ensemble)+ min_ens_yrs


      # Generate the sequence of years
      years <- seq(to = i, length.out = ens_yrs)
    } else {
      # Check if stretched is less than 3
      if (min_ens_yrs < 3) {
        stop("Error: 'min_ens_yrs' should not be less than 3.")}
      years <- seq(to = i, length.out = slide)
    }

max_year<-i
    tdat <- forecasts %>%
      dplyr::filter(
        model %in% c(forecasts %>%
                       dplyr::filter(year == i + 1, rank <= num_models) %>%
                       dplyr::pull(model))
      ) %>%
      dplyr::filter(year %in% years) %>%
      dplyr::left_join(series, by = "year") %>%
      dplyr::select(year, model, predicted_abundance, abundance = abundance.x,aicc) %>%
      dplyr::ungroup() |>
      dplyr::mutate(error = abundance - predicted_abundance,
                    year_dif=max_year-year+1,
                    exp_smooth_weight=if(alpha!=0){alpha*(1-alpha)^year_dif}else{1}) %>%
      dplyr::filter(!is.na(error)) %>%
      dplyr::arrange(year) |>
      dplyr::group_by(model) %>%
      dplyr::summarize(RMSE = sqrt(weighted.mean(error^2,exp_smooth_weight)),
                       MAPE = weighted.mean(abs(error / abundance),exp_smooth_weight),
                       ,
                       MSA = 100 * (exp(weighted.mean(abs(log(abundance / predicted_abundance)),exp_smooth_weight)) - 1),
                       aicc=tail(aicc,1)
      ) %>%
      dplyr::arrange(MSA) %>%
      dplyr::mutate(MSA_weight = (1 / MSA)^k / sum((1 / MSA)^k),
                    RMSE_weight = (1 / RMSE)^k / sum((1 / RMSE)^k),
                    MAPE_weight = (1 / MAPE)^k / sum((1 / MAPE)^k),
                    AICc_weight = exp(-.5*(aicc-min(aicc)))/
                      sum(exp(-.5*(aicc-min(aicc)))),
                    equal_weight = 1/dplyr::n(),
      )



    if(do_stacking){

      stackdat <- forecasts %>% dplyr::ungroup() |>
      dplyr::filter(
        model %in% c(forecasts %>%
                       dplyr::filter(year == i + 1, rank <= num_models) %>%
                       dplyr::pull(model))
      ) %>%
      dplyr::filter(year %in% years) %>%
      tidyr::pivot_wider(names_from = model, values_from = predicted_abundance, id_cols = year) %>%
      dplyr::left_join(series %>% dplyr::select(year, abundance)) %>%
        dplyr::ungroup()



    stack_weights <- find_stack_weights(tau = 1,
                                        n = num_stack_it,
                                        metric = stack_metric,
                                        initial_weights = rep(1 / (ncol(stackdat)-2), (ncol(stackdat)-2)),
                                        preds = stackdat %>%
                                          dplyr::filter(!is.na(abundance)) %>%
                                          dplyr::select(!abundance & !year) %>%
                                          as.matrix(),
                                        obs = stackdat %>%
                                          dplyr::filter(!is.na(abundance)) %>%
                                          dplyr::select(abundance & !year) %>%
                                          as.matrix()
    )


    stacking_weights <- data.frame("Stacking_weight" = as.vector(round(unlist(stack_weights[[1]]), 4)))
    stacking_weights$model <- colnames(stackdat)[!colnames(stackdat) %in% c("year", "abundance")]
    tdat <- tdat %>%
      dplyr::left_join(stacking_weights)
    }else{
         tdat <-tdat |> dplyr::mutate(Stacking_weight=1)
    }


    tdat2 <- forecasts %>%
      dplyr::filter(
        model %in% c(forecasts %>%
                       dplyr::filter(year == i + 1, rank <= num_models) %>%
                       dplyr::pull(model))
      ) %>%
      dplyr::filter(year == max(years) + 1) %>%
      tidyr::pivot_longer(names_to = "Parameter",
                          cols = c("predicted_abundance", "Lo 95", "Hi 95", "Lo 50", "Hi 50"),
                          values_to = "value") %>%
      dplyr::left_join(tdat %>% dplyr::select(model, MSA_weight:Stacking_weight)) %>%
      dplyr::mutate(MSA_weighted = value * MSA_weight,
                    RMSE_weighted = value * RMSE_weight,
                    MAPE_weighted = value * MAPE_weight,
                    AICc_weighted = value * AICc_weight,
                    Equal_weighted = value * equal_weight,
                    Stack_weighted = value * Stacking_weight
      ) %>%
      dplyr::group_by(year, Parameter) %>%
      dplyr::summarise(MSA_weighted = sum(MSA_weighted),
                       RMSE_weighted = sum(RMSE_weighted),
                       MAPE_weighted = sum(MAPE_weighted),
                       AICc_weighted = sum(AICc_weighted),
                       Equal_weighted = sum(Equal_weighted),
                       Stack_weighted = sum(Stack_weighted)

      ) %>%
      tidyr::pivot_longer(names_to = "model",
                          cols = c("MSA_weighted", "RMSE_weighted", "MAPE_weighted", "AICc_weighted", "Equal_weighted", "Stack_weighted"),
                          values_to = "value") %>%
      tidyr::pivot_wider(id_cols = c("year", "model"), names_from = Parameter, values_from = value)

    if(!do_stacking){
      tdat2<-tdat2[tdat2$model!="Stack_weighted",]
    }

    # # Add "Best individual" rows with correct performance metrics
    # tdat2 <- dplyr::bind_rows(
    #   tdat2,
    #   forecasts %>%
    #     dplyr::filter(
    #       model %in% c(forecasts %>%
    #                      dplyr::filter(year == i + 1, rank <= num_models) %>%
    #                      dplyr::pull(model))
    #     ) %>%
    #     dplyr::filter(year == max(years) + 1) %>%
    #     dplyr::mutate(model = "Best individual") %>%
        # dplyr::group_by(year, model) %>%
        # dplyr::summarise(
        #   MSA_weighted = mean(MSA_weight, na.rm = TRUE),
        #   RMSE_weighted = mean(RMSE_weight, na.rm = TRUE),
        #   MAPE_weighted = mean(MAPE_weight, na.rm = TRUE),
        #   Stack_weighted = mean(Stacking_weight, na.rm = TRUE)
        # )
    # )

    ensembles <- dplyr::bind_rows(ensembles, tdat2)
  }


  # function to evaluate forecast skill ()
  evaluate_forecasts2 <- function(forecasts, observations) {
    forecast_skill <- forecasts %>%
      # left_join(observations,by=c("Year","runsize_obs"))%>%
      dplyr::select(year, model, abundance, predicted_abundance) %>%
      dplyr::arrange(year) |>
      dplyr::group_by(model) %>%
      dplyr::mutate(error = predicted_abundance - abundance,,
                    naive_error=abundance-dplyr::lag(abundance)
                    ) %>%
      dplyr::filter(!is.na(error)) %>%
      dplyr::group_by(model) %>%
      dplyr::summarise(
        MAPE = mean(abs(error / abundance)) * 100,
        RMSE = sqrt(mean(error^2)),
        MSA = 100 * (exp(mean(abs(log(abundance / predicted_abundance)))) - 1),
        MASE=mean(abs(error),na.rm=T)/mean(abs(naive_error),na.rm=T)

      ) %>%
      dplyr::arrange(MAPE)
    return(forecast_skill)
  }

  forecast_skill <- evaluate_forecasts2(
    forecasts = dplyr::bind_rows(forecasts , ensembles %>%
                                   dplyr::left_join(series)) %>%
      dplyr::filter(year > (yrrange[2] - TY_ensemble)),
    observations = series
  )

  forecasts2 <- dplyr::bind_rows(forecasts, ensembles %>% dplyr::left_join(series)) %>%
    dplyr::filter(year > (yrrange[2] - TY_ensemble)) %>%
    dplyr::mutate(error = predicted_abundance - abundance,
                  pct_error = scales::percent(error / abundance)
    ) %>%
    dplyr::left_join(tdat %>% dplyr::select(model, Stacking_weight)) %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(
      performance_metrics = calculate_performance_metrics(predicted_abundance, abundance),
      Stacking_weight = mean(Stacking_weight, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>% unpack(cols=performance_metrics)

  results <- list(
    final_model_weights = tdat,
    forecast_skill = forecast_skill,
    ensembles = ensembles,
    forecasts = forecasts2
  )
  return(results)
}
