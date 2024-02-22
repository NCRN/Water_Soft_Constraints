SoftConstraintsFunc <- function(Water_Data, site, water_char, distribution, Model_Year, To_Plot, ticker, total_runs){ # DM added ticker and total runs
  # This function inputs the EDD dataset and desired monitoring site, characteristic, 
  # and distribution of the data. It outputs charts and a table of soft constraints.
  
  # Extract data
  Demo_Data<-Water_Data %>% 
    filter(MonitoringLocationIdentifier==site,
           CharacteristicName ==water_char) %>% 
    select(MonitoringLocationIdentifier,ActivityStartDate,CharacteristicName,ResultMeasureValue  ) %>% 
    mutate(ResultMeasureValue=as.numeric(ResultMeasureValue))
  
  
  Demo_Data<-Demo_Data %>%
    mutate(Year=year(ActivityStartDate),
           Month=month(ActivityStartDate),
           DecDate=decimal_date(ActivityStartDate),
           YrFraction=decimal_date(ActivityStartDate)-Year,
           Date_cos=cos(2*pi*YrFraction),
           Date_sin=sin(2*pi*YrFraction))
  
  
  DO_Model<-brm(ResultMeasureValue~s(DecDate, k=4) + s(Date_cos,Date_sin, k=4),
                data = Demo_Data,
                family=switch(distribution,
                              "gamma" = Gamma(link="log"),
                              "gaussian" = gaussian()),
                chains = 4,
                cores=4,
                control = list(adapt_delta = 0.99, max_treedepth=15),
                warmup=500,
                iter=2000,
  )
  
  
  Prediction_Data<-
    data.frame(ActivityStartDate=seq(ymd(paste(min(Model_Year),"-01-01")), 
                                     ymd(paste(max(Model_Year),"-12-31")), by="days")) %>%
    mutate( Year=year(ActivityStartDate),
            Month=month(ActivityStartDate),
            DecDate=decimal_date(ActivityStartDate),
            YrFraction=decimal_date(ActivityStartDate)-Year,
            Date_cos=cos(2*pi*YrFraction),
            Date_sin=sin(2*pi*YrFraction))
  
  
  Predictions<-predicted_draws(DO_Model, Prediction_Data) %>%
    summarize(Median=quantile(.prediction, probs=.5), Lower=quantile(.prediction, probs=.1),
              Upper=quantile(.prediction, probs=.9, .by=ActivityStartingDate)) %>% ungroup()
  
  
  
  # Make and Print the plots
  if (To_Plot==1){
    # Plot time series
    A <- ggplot(Demo_Data, aes(x=ActivityStartDate, y=ResultMeasureValue)) +
      geom_point() +
      ggtitle(paste(site,water_char))  # Plot time series
    
    # Plot the model
    B <- Demo_Data%>% filter(!is.na(ResultMeasureValue)) %>% 
      add_predicted_draws(DO_Model ) %>%
      ggplot(aes(x=ActivityStartDate, y=ResultMeasureValue)) +
      stat_lineribbon(aes(y=.prediction), .width=c(.9),
                      alpha=.8, color="black")+
      geom_point(data=Demo_Data, color="blue")+
      scale_fill_brewer(palette="Greys")+
      theme_bw()
    
    # Plot predictions
    C <- Predictions %>% ggplot(aes(x=ActivityStartDate))+
      geom_ribbon(aes(ymin=Lower, ymax=Upper), fill="grey")+
      geom_line(aes(y=Median))+
      theme_bw() +
      ggtitle(paste(distribution, Model_Year))
    
    
    windows(6.5,9)
    print(
      ggarrange(A,B,C, ncol=1, nrow=3)
    )
  }
  
  Soft_Limits<-Predictions %>%
    summarise(Lower=median(Lower), Upper=median(Upper), .by=c(Month, Year))  
  
  Soft_Limits<- Soft_Limits %>% 
    mutate(MonitoringLocationIdentifier=site, CharacteristicName=water_char)
  
  # Print ticker (DM)
  print(paste(ticker,"of",total_runs,"complete"))
  
  # Generate table of outputs
  Soft_Limits
  return(Soft_Limits)
}