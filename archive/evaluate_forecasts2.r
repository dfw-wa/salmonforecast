#function to evaluate forecast skill ()
evaluate_forecasts2<-function(forecasts,observations){
  forecast_skill<-forecasts%>%
    # left_join(observations,by=c("Year","runsize_obs"))%>%
    dplyr::select(year,model,abundance,predicted_abundance)%>%
    dplyr::arrange(year) |>
    group_by(model) %>%
    mutate(error=predicted_abundance-abundance,
           naive_error=abundance-dplyr::lag(abundance))%>%
    filter(!is.na(error))%>%
    summarise(MAPE = mean(abs(error/abundance))*100,
              RMSE = sqrt(mean(error^2)),
              MSA = 100*(exp(mean(abs(log(abundance/predicted_abundance))))-1),
              MASE=mean(abs(error),na.rm=T)/mean(abs(naive_error),na.rm=T)
    )%>%
    arrange(MAPE)
  return(forecast_skill)
}
