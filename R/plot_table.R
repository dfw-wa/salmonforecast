#' Plot and Generate Tables for Forecasting Results
#' @name plot_table
#' @title Plot and Generate Tables for Forecasting Results
#' This function creates tables and plots to summarize the forecasting results.
#'
#' @param rp Rolling performance data.
#' @param ens Ensemble model data.
#' @param dat the input data with columns "year" and "abundance"
#' @param benchmark info on a  model to be compared to
#'
#' @return A list containing tables and plots summarizing the forecasting results.
#'
#' @importFrom ggplot2 geom_ribbon geom_line geom_point scale_x_continuous scale_color_manual theme
#' @importFrom kableExtra kbl kable_classic save_kable
#' @importFrom dplyr bind_rows filter mutate select left_join arrange
#' @export


# function to make plots and tables
plot_table<-function(
    rp,
    ens,
    dat,
    stack_metric,
    rolling_year_window,
    benchmark=NULL,
    output_path = "outputs"

  ){

  for_skill<-
    rp$performance %>% dplyr::mutate(model="Best_individual") %>% dplyr::bind_rows(
      ens$forecast_skill %>%dplyr::filter(grepl("weight",model))
    )  %>%
    {if(!is.null(benchmark)){
      dplyr::bind_rows(.,benchmark$perf) %>%
        dplyr::arrange(MAPE)}else{
      .
    }
    }





  Table2<-for_skill%>% mutate(model=sub("_"," ",model),
                              )%>% rename(Model=model)%>%
    kableExtra::kbl(caption = paste0("Table 2. One step ahead individual and ensemble model performance"),digits =2)%>%
    kableExtra::kable_classic(full_width = F, html_font = "Cambria")




  Table3<-
    # results$forecasts%>%
    # filter(year==max(year))%>%
    rp$top_mods %>% filter(rank==1) %>% arrange(year) %>%
    dplyr::select(year,model_name,predicted_abundance,`Lo 50`,`Hi 50`,`Lo 95`,`Hi 95`) %>% #,Stacking_weight)%>%
    rename(Year=year,Model=model_name,`Predicted abundance` =predicted_abundance) %>%
    kbl(caption = paste0("Table 3. Individual models which performed the best in",stack_metric," based on a",rolling_year_window, "year retrsopective analysis, and their forecasts."),digits =2)%>%
    kable_classic(full_width = F, html_font = "Cambria")



  best_model<-for_skill$model[1]
if(!grepl("weight",best_model)){
  best_model<-for_skill$model[2]
}

  results_best<-rp$top_mods %>% filter(rank==1) %>% mutate(model="Best_individual") %>%
    dplyr::select(year:aicc,`Lo 50`:rank) |>
    bind_rows(
    ens$ensembles %>% left_join(dat %>% dplyr::select(year,abundance)) %>% mutate(model_name=model)
  ) %>% filter(model==best_model) %>%
    dplyr::select(year,model_name,abundance,
                  predicted_abundance,"Lo 50",
                  "Hi 50","Lo 95","Hi 95") %>%
    mutate(error=predicted_abundance-abundance,
           pct_errpr=error/abundance)




  Table4<-results_best%>%
    rename(Year=year,Model=model_name,Abundance=abundance,`Predicted abundance`=predicted_abundance,`% error`=pct_errpr) %>%
    # dplyr::select(!c("train_test","Stacking_weight","model_name") & !c(covariates[covariates%in%names(results_best)]))%>%
    kbl(caption = paste0("Table 4. One-year-ahead forecasts based on the ",best_model," model."),digits =2)%>%
    kable_classic(full_width = F, html_font = "Cambria")




  Figure1<-ggplot(results_best,aes(x=year,y=predicted_abundance))+
    geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`),color=NA,alpha=0.5,fill = "cadetblue")+
    geom_ribbon(aes(ymin=`Lo 50`,ymax=`Hi 50`),color=NA,alpha=0.5,fill = "cadetblue")+
    geom_line()+
    geom_point(aes(x=year,y=abundance))+
    ylim(0,NA)+
    scale_x_continuous(breaks=unique(results_best$year))+ylab("Abundance")+xlab("")



  Figure2<-ggplot(

    rp$top_mods %>% filter(rank==1) %>% mutate(model="Best_individual") |>  dplyr::select(year:aicc,`Lo 50`:rank)%>% bind_rows(
    ens$ensembles%>% left_join(dat %>% dplyr::select(year,abundance)) %>% mutate(model_name=model)) %>%
      filter(model==best_model)


    ,aes(x=year,y=predicted_abundance,col=model))+
  geom_line()

  if(!is.null(benchmark)){
    Figure2<- Figure2+
  geom_line(data=benchmark$forecasts,mapping=aes(x=year,y=predicted_abundance),size=1.25,col="grey")
  }

  Figure2<- Figure2+
  geom_point(aes(x=year,y=abundance),color="black")+
  ylim(0,NA)+
  scale_x_continuous(breaks=unique(results_best$year))+
  theme(legend.position = "none")+
  ylab("Abundance")+xlab("")+
  theme(legend.key.size=unit(.15,'cm'),legend.position = "top")


  Figure3<-ggplot((results_best ) %>% mutate(Model=sub("_"," ",model_name)) %>%  mutate(pct_error=log10(((predicted_abundance-abundance)/abundance)+1)) ,
                  aes(x=year,y=pct_error,color=Model))+geom_hline(yintercept=0)+geom_line(lwd=1.25)+scale_color_manual(values=c("darkblue","darkred"))+xlab("")+ylab(expression(paste(log[10], "(% error +1)")))

  # kableExtra::save_kable(Table2, file.path(output_path, "Table2.html"))
  # kableExtra::save_kable(Table3, file.path(output_path, "Table3.html"))
  # kableExtra::save_kable(Table4, file.path(output_path, "Table4.html"))
  #
  # ggsave(file.path(output_path, "Figure1.png"), Figure1, width = 8, height = 6)
  # ggsave(file.path(output_path, "Figure2.png"), Figure2, width = 8, height = 6)
  # ggsave(file.path(output_path, "Figure3.png"), Figure3, width = 8, height = 6)

return(
  list(
    Table2=Table2,
    Table3=Table3,
    Table4=Table4,
    Figure1=Figure1,
    Figure2=Figure2,
    Figure3=Figure3
  )
)

}
