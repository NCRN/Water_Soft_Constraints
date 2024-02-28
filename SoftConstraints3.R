library(tidyverse)
library(lubridate)
library(brms)
library(tidyverse)
library(ggpubr)
library(tidybayes)

# Source function
source("SoftConstraintsFunc3.R")

#### Read in data and filter only the needed columnst and rows. Eliminate rows with NA for result values

Water_Data_all<-read_csv("Data/20240129_wqp_wqx_bss_wq_npsncrn.csv") %>% 
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

# Do each site individually (so don't have to run ~24 hour script all at once)
site_names <- unique(Water_Data_all$MonitoringLocationIdentifier)
for (i in 1:length(site_names)){
  
  ## Remove the filter below to do more than one stream
  Water_Data<-Water_Data_all %>% filter(MonitoringLocationIdentifier==site_names[i])
  
  
  #### Check how much data is in each Stream X Characteristic Combo. Reomve from analysis if <20
  Data_Count<-Water_Data %>% 
    summarise(Records=n(), YearMin=min(year(ActivityStartDate)),YearMax=max(year(ActivityStartDate)), .by=c(MonitoringLocationIdentifier,CharacteristicName),zeroes = sum(as.numeric(ResultMeasureValue)<=0,na.rm=T)) %>% 
    filter(Records>19)
  #this removes about 1/8 of the stream x characteristic combos
  
  Data_Count <-Data_Count %>% 
    mutate(distribution= ifelse(CharacteristicName %in% c(
      "Acid Neutralizing Capacity (ANC)", "Temperature, air", "Wetted Width", 
      "Cross-Section Depth", "RBP Stream Velocity", "Temperature, water",
      "pH", "Dissolved oxygen saturation", "Dissolved oxygen (DO)",
      "Barometric pressure") | zeroes>0, "gaussian", "gamma"))
  
  
  # Some salinity  and Total Phosphorus, mixed forms were made gaussian due to zero values.
  # The above lines detect zero or negative values, and then assign a gaussian distribution
  # for sites they occur at.
  
  Water_Data<-Water_Data %>% semi_join(Data_Count)
  
  # Create ticker (experimental)
  Data_Count$ticker <- 1:nrow(Data_Count)
  total_runs <- nrow(Data_Count)
  start_time <- Sys.time()
  print("Started:"); print(start_time)
  
  calcLimits<-function(Water_Data,MonitoringLocationIdentifier, CharacteristicName,distribution, YearMin,YearMax, ticker, total_runs){
    
    SoftConstraintsFunc(Water_Data=Water_Data, site=MonitoringLocationIdentifier, 
                        water_char=CharacteristicName, distribution = distribution,
                        Model_Year=YearMin:YearMax, ticker=ticker, total_runs=total_runs,To_Plot=F) # DM added ticker and total runs
    
  }
  
  Limit_List<-pmap(.l=list(Water_Data=list(Water_Data ), 
                           MonitoringLocationIdentifier= Data_Count$MonitoringLocationIdentifier,
                           CharacteristicName=Data_Count$CharacteristicName,
                           distribution=Data_Count$distribution,
                           YearMin=Data_Count$YearMin,
                           YearMax=Data_Count$YearMax,
                           ticker=Data_Count$ticker, total_runs=total_runs), # DM added ticker and total runs
                   .f=calcLimits)
  
  
  Output_Limits<-bind_rows(Limit_List)
  
  write_csv(Output_Limits, paste("Soft constraints files/Soft_Constraints_", site_names[i], ".csv"))
  
  end_time <- Sys.time()
  print(site_names[i])
  print("Started:"); print(start_time)
  print("Finished:"); end_time
  print("Time spent:"); end_time-start_time
  
}

# After all the sites are finished, you can just combine the individual Soft_Constraints csv's
# into one long one.
csv_data_all <- data.frame()
setwd("Soft constraints files/")
files <- dir()
for (i in 1:length(files)){
  csv_data <- read.csv(files[i])
  csv_data_all <- rbind(csv_data_all, csv_data)
}
head(csv_data_all)
write.csv(csv_data_all, "SoftConstraints_from_models.csv")
