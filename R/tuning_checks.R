#' @name check_nyrs
#' @title evaluate performance of using different numbers of years when calculating performance metrics for model selection and weighting
#'
#' @param One_ahead_out
#' @param dat
#' @param num_models
#' @param TY_ensemble
#' @param model_list
#' @param alpha
#' @param model_to_plot
#' @inherit do_forecast
#'
#' @return
#' @export
#'
#' @examples
check_nyrs<-function(One_ahead_out,dat,num_models,TY_ensemble,model_list,alpha,model_to_plot="MAPE_weighted"){


out<-tibble::tibble(

)

for ( i in 1:(TY_ensemble-1)){


  rp<-rolling_perf(One_ahead_out,dat,i,num_models,TY_ensemble,model_list,alpha=alpha)
  # rp$performance



  # num_models<-10
  ens<-ensemble(forecasts=rp$all_mods %<>% group_by(year) %>% mutate(rank=rank(MAPE)) %>% ungroup,
                                   series=dat,
                                   TY_ensemble=TY_ensemble,
                                   k=1,
                                   slide=i,
                                   num_models=num_models,
                                   stack_metric=NULL,
                                   stretch=FALSE,
                                   do_stacking = FALSE,
                                   alpha=alpha)
  # ens$forecast_skill %>%filter(grepl("weight",model))

  for_skill<-
    rp$performance %>% dplyr::mutate(model="Best_individual") %>% bind_rows(
      ens$forecast_skill %>%dplyr::filter(grepl("weight",model))
    ) %>%
    dplyr::arrange(MAPE) |>
    dplyr::mutate(rolling_window=i,alpha=alpha)

  out<-out |> dplyr::bind_rows(for_skill |>
                                 dplyr::left_join(ens$ensembles |> dplyr::ungroup() |> dplyr::filter(year==max(year,na.rm=T))))

}


plot_perf<-out |> dplyr::filter(model==model_to_plot) |> ggplot2::ggplot(ggplot2::aes(x=rolling_window,y=MAPE))+ggplot2::geom_col() +ggplot2::xlab("N years included")

plot_pred<-out |> filter(model==model_to_plot)  |>  ggplot2::ggplot(ggplot2::aes(x=rolling_window,y=predicted_abundance))+ggplot2::geom_col() + ggplot2::xlab("N years included")

list(out=out,
     plot_perf=plot_perf,
     plot_pred=plot_pred)

}



#' @name check_alpha
#' @title evaluate performance of using different rates of decay (alpha) in weighting when calculating performance metrics for model selection and weighting
#'
#' @param One_ahead_out
#' @param dat
#' @param num_models
#' @param TY_ensemble
#' @param model_list
#' @param roll_years length of sliding window for performance eval
#' @param screen_metric
#' @param alpha
#' @param model_to_plot
#'
#' @inherit do_forecast
#'
#' @return
#' @export
#'
#' @examples
check_alpha<-function(One_ahead_out,dat,roll_years,screen_metric,num_models,TY_ensemble,model_list,model_to_plot="MAPE_weighted"){


  out<-tibble::tibble(

  )

  for ( i in seq(0,.95,by=0.05)){


    rp<-rolling_perf(One_ahead_out,dat,roll_years,screen_metric,num_models,TY_ensemble,model_list,alpha=i)
    # rp$performance



    # num_models<-10
    ens<-ensemble(forecasts=rp$all_mods %<>% group_by(year) %>% mutate(rank=rank(MAPE)) %>% ungroup,
                  series=dat,
                  TY_ensemble=TY_ensemble,
                  k=1,
                  slide=roll_years,
                  num_models=num_models,
                  stack_metric=NULL,
                  stretch=FALSE,
                  do_stacking = FALSE,
                  alpha=i)
    # ens$forecast_skill %>%filter(grepl("weight",model))

    for_skill<-
      rp$performance %>% dplyr::mutate(model="Best_individual") %>% bind_rows(
        ens$forecast_skill %>%dplyr::filter(grepl("weight",model))
      ) %>%
      dplyr::arrange(MAPE) |>
      dplyr::mutate(alpha=i)

    out<-out |> dplyr::bind_rows(for_skill |>
                                   dplyr::left_join(ens$ensembles |> dplyr::ungroup() |> dplyr::filter(year==max(year,na.rm=T))))

  }


  plot_perf<-out |> dplyr::filter(model==model_to_plot) |> ggplot2::ggplot(ggplot2::aes(x=alpha,y=MAPE))+ggplot2::geom_col() +ggplot2::xlab("Exponential decay rate")

  plot_pred<-out |> filter(model==model_to_plot)  |>  ggplot2::ggplot(ggplot2::aes(x=alpha,y=predicted_abundance))+ggplot2::geom_col() + ggplot2::xlab("Exponential decay rate")

  list(out=out,
       plot_perf=plot_perf,
       plot_pred=plot_pred)

}
