#' Perform Forecasting with Ensemble Models
#'
#' This function conducts time series forecasting using ensemble models.
#' @name do_forecast
#' @title Perform Forecasting with Ensemble Models
#' @param dat data frame with columns "year", "species", "period", "abundance", and the covariates included in the covariates argument
#' @param covariates A vector specifying the covariates to be considered in the forecasting models.
#' @param TY_ensemble number of years ti evaluate performance based on plus 1.
#' @param slide The length of the sliding window for calculating ensemble weights
#' @param first_forecast_period The starting period for making forecasts.
#' @param plot_results Logical, indicating whether to plot forecast results.
#' @param write_model_summaries Logical, indicating whether to write model summaries.
#' @param forecast_period_start_m Starting month of the forecast period (inclusive).
#' @param forecast_period_start_d Starting day of the forecast period (inclusive).
#' @param stack_metric The metric used for stacking models in the ensemble.
#' @param num_stack_it max number of iterations to run stacking algorithm
#' @param k Number of top models to include in the ensemble.
#' @param min_vars Minimum number of covariates in a model.
#' @param max_vars Maximum number of covariates in a model.
#' @param forecast_type Type of forecasting approach ("preseason" or other).
#' @param screen_metric the metric used to rank and screen top models. options are MAPE or aicc
#' @param num_models Number of top models to consider in ensemble creation.
#' @param alpha the annual rate of decay per year used in weighting performance in metric  calculation for model selection and ensemble-weights
#' @param ts_freq frequency for time series (generally 1)
#' @param seasonal whether the arima models should include seasonal  components
#' @param n_cores number of cores to use in parallel computing
#' @param supplied_forecasts a dataframe of forecasts to be used *instead* of fitting specified models
#' @param update_last_year refit only the models with the full dataset used to predict the upcoming year.
#'
#'
#' @return A list containing tables and plots summarizing the forecasting results and a list containing various outputs, including selected covariates, forecast results, rolling performance, ensemble models, and plots/tables.
#'
#' @importFrom dplyr mutate group_by %>%
#' @import ggplot2
#' @export
#'

#function to do forecast and return plots

do_forecast<-function(
    dat,
    covariates=c(#new
      "lag1_log_JackOPI"
      ,"lag1_log_SmAdj"
      ,"lag1_NPGO"
      ,"lag1_PDO"
      ,"WSST_A"

      #OCN
      ,"PDO.MJJ"
      ,"MEI.OND"
      ,"UWI.JAS"
      ,"SST.AMJ"
      ,"SSH.AMJ"
      ,"UWI.SON"),


    TY_ensemble=16,
    slide=15,
    first_forecast_period = 1,
    plot_results = FALSE,
    write_model_summaries = TRUE,
    forecast_period_start_m =  1, #inclusive
    forecast_period_start_d =  1, #inclusive
    do_stacking=FALSE,
    stack_metric = "MAPE",
    k=1,
    min_vars=0,
    max_vars=1,
    forecast_type="preseason",
    screen_metric=aicc,
    num_models=25,
    n_cores=2,
    ts_freq=1,
    seasonal=FALSE,
    exp_smooth_alpha=0,
    include_mod=FALSE,
    num_stack_it=500,
    supplied_forecasts=NULL,
    update_last_year=FALSE


){

  leave_yrs<-TY_ensemble+slide


  #find all subsets
  best_covariates<-all_subsets(series=dat,covariates=covariates,min=min_vars,max=max_vars,type=forecast_type,fit=FALSE)

  #generate one step ahead forecasts
  if(is.null(supplied_forecasts)|update_last_year){

    if(update_last_year){
      leave_yrs<-2
    }

  results<-one_step_ahead(series=dat,
                          leave_yrs=leave_yrs,
                          TY_ensemble=TY_ensemble,
                          covariates=
                            best_covariates[[1]],
                          first_forecast_period = first_forecast_period,
                          plot_results = plot_results,
                          write_model_summaries = write_model_summaries,
                          forecast_period_start_m =  forecast_period_start_m, #inclusive
                          forecast_period_start_d =  forecast_period_start_d, #inclusive
                          n_cores=n_cores,
                          ts_freq=ts_freq,
                          seasonal=seasonal,
                          include_mod=include_mod
  )

  if(update_last_year){
    results<-dplyr::bind_rows(supplied_forecasts,results)
  }

  }else{
    results<-supplied_forecasts
  }


  ## not sure if we need this? if not, get rid of it, if so, let's wrap it into one of the existing function (e.g., one_step_ahead)
  model_list<-lapply(best_covariates[[1]],
                     function(x) paste(x,collapse = " + "))%>%
    unlist()%>%
    dplyr::as_tibble()%>%
    tibble::rownames_to_column()%>%
    dplyr::rename(model=rowname,model_name=value)

  # rolling performance / ensemble
  rp<-rolling_perf(one_aheads=results,
                   series=dat,
                   roll_years = TY_ensemble-1,
                   screen_metric={{screen_metric}},
                   mod_include = num_models,
                   TY_ensemble = TY_ensemble,
                   model_list = model_list,
                   alpha=exp_smooth_alpha)


  ens<-ensemble(forecasts=(rp$all_mods %<>% dplyr::group_by(year) %>% dplyr::mutate(rank=rank({{screen_metric}})) %>% dplyr::ungroup()) |> dplyr::select(year:aicc,`Lo 50`:rank),
                series=dat,
                TY_ensemble=TY_ensemble,
                k=k,
                slide=slide,
                num_models=num_models,
                stack_metric="MAPE",
                do_stacking=do_stacking,
                alpha=exp_smooth_alpha,
                num_stack_it=num_stack_it)
  # plot and table
  plots_and_tables<- plot_table(
    rp=rp,
    ens=ens,
    dat=dat,
    stack_metric=stack_metric,
    rolling_year_window=TY_ensemble-1
  )
  #return = plots_and_tables
  return(list(
    best_covariates = best_covariates,
    results = results,
    model_list=model_list,
    rp = rp,
    ens=ens,
    plots_and_tables = plots_and_tables
  ))

}

