library(tidyverse)
library(lubridate)
library(brms)
library(tidyverse)
library(ggpubr)
library(tidybayes)

#### Read in data and filter only the needed columnst and rows. Eliminate rows with NA for result values

Water_Data<-read_csv("C:/Data/NCRN_Water_WQX/20240129_wqp_wqx_bss_wq_npsncrn.csv") %>% 
  select(ProjectIdentifier, MonitoringLocationIdentifier, ActivityStartDate,
         CharacteristicName, ResultMeasureValue) %>% 
  filter(ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring", 
         CharacteristicName %in% c("Temperature, water", "Barometric pressure", "Conductivity", 
           "Specific conductance", "Turbidity", "Salinity", "pH",
           "Dissolved oxygen (DO)", "Dissolved oxygen saturation",
           "Wetted Width", "Base flow discharge", "RBP Stream Velocity",
           "Cross-Section Depth", "Acid Neutralizing Capacity (ANC)",
           "Total Nitrogen, mixed forms", "Total Phosphorus, mixed forms",
           "Solids, Dissolved (TDS)", "Temperature, air"), 
         !is.na(ResultMeasureValue)
  )


## Remove the filter below to do more than one stream
Water_Data<-Water_Data %>% filter(MonitoringLocationIdentifier=="NCRN_CATO_BGHC")



#### Check how much data is in each Stream X Characteristic Combo. Reomve from analysis if <20
Data_Count<-Water_Data %>% 
  summarise(Records=n(), YearMin=min(year(ActivityStartDate)),YearMax=max(year(ActivityStartDate)), .by=c(MonitoringLocationIdentifier,CharacteristicName)) %>% 
  filter(Records>19)
#this removes about 1/8 of the stream x characteristic combos

Data_Count <-Data_Count %>% 
  mutate(distribution= ifelse(CharacteristicName %in% c(
  "Acid Neutralizing Capacity (ANC)", "Temperature, air", "Wetted Width", 
  "Cross-Section Depth", "RBP Stream Velocity", "Temperature, water",
  "pH", "Dissolved oxygen saturation", "Dissolved oxygen (DO)","Salinity",
  "Total Phosphorus, mixed forms"), "gaussian", "gamma"))
  

#salinity  and Total Phosphorus, mixed forms were made gaussian due to zero values - revisit!!!

Water_Data<-Water_Data %>% semi_join(Data_Count)

calcLimits<-function(Water_Data,MonitoringLocationIdentifier, CharacteristicName,distribution, YearMin,YearMax ){

  SoftConstraintsFunc(Water_Data=Water_Data, site=MonitoringLocationIdentifier, 
                      water_char=CharacteristicName, distribution = distribution,
                      Model_Year=YearMin:YearMax, To_Plot=F)
  
}

Limit_List<-pmap(.l=list(Water_Data=list(Water_Data ), 
          MonitoringLocationIdentifier= Data_Count$MonitoringLocationIdentifier,
          CharacteristicName=Data_Count$CharacteristicName,
          distribution=Data_Count$distribution,
          YearMin=Data_Count$YearMin,
          YearMax=Data_Count$YearMax),
     .f=calcLimits)


Output_Limits<-bind_rows(Limit_List)

write_csv(Output_Limits, "Soft_Constraints.csv")

   