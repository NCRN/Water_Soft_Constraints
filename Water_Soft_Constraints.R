# Water_Soft_Constraints.R
# This script reads in the NCRN water EDD dataset, and outputs a table of soft
# constraints for quality control checks.
# 12/19/2023

#### Load Libraries ############################################################
library(tidyverse)
library(brms)
library(tidybayes)
library(tibble)
library(tidyr)
library(dplyr)
library(rlang)
library(ggplot2)
library(lubridate)
library(ggpubr)


#### Read in Data ##############################################################
# data should be in a /Data directory
# need hard constraints and the full data set. 

# Read data
fileName <- "20231128_wqp_wqx_bss_wq_npsncrn.csv" 
Water_Data <- read_csv(paste("Data/", fileName, sep=""))

# Read Lookup and constraints
lookup <- read.csv("Data/Lookup.csv")
softConstraints <- read.csv("Data/SoftConstraints 2.csv")
hardConstraints <- read.csv("Data/HardConstraints.csv")

# Select WQ data by project identifier
Water_Data <- Water_Data[Water_Data$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]


#### Create Vectors that store which sites and characteristics we want #########
# All of them
Site_Names <- unique(Water_Data$MonitoringLocationIdentifier) %>% sort()
Model_Year <- 2005:2023
Char_Names <- c("Temperature, water", "Barometric pressure", "Conductivity", 
                "Specific conductance", "Turbidity", "Salinity", "pH",
                "Dissolved oxygen (DO)", "Dissolved oxygen saturation",
                "Wetted Width", "Base flow discharge", "RBP Stream Velocity",
                "Cross-Section Depth", "Acid Neutralizing Capacity (ANC)",
                "Total Nitrogen, mixed forms", "Total Phosphorus, mixed forms",
                "Solids, Dissolved (TDS)", "Temperature, air")

# # Choose one of each for testing
# Site_Names <- "NCRN_GWMP_MIRU"
# Char_Names <- "Specific conductance"
# Model_Year <- 2023

# Do you want to make plots?
To_Plot = 0 # 1=yes, 0=no

# What is the minimum number of records we want to make a model?
min_records <- 3

# Start ticker
ticker <- 1

# Loop through site-characteristic-year combos
for (i in 1:length(Site_Names)){
  for (j in 1:length(Char_Names)){
    for (k in 1:length(Model_Year)){
      
      # Check if there's enough data to be worth it
      n_records <- nrow(Water_Data[Water_Data$MonitoringLocationIdentifier==Site_Names[i] &
                                Water_Data$CharacteristicName==Char_Names[j] &
                                as.numeric(year(Water_Data$ActivityStartDate))==Model_Year[k],])
      
      if (n_records >= min_records){ # Arbitrary number, below which we would not want to make a model
  
        #### Filter data and format to what we want ##############################
        # Select the desired distribution of the data
        if (Char_Names[j] %in% c(
          "Acid Neutralizing Capacity (ANC)", "Temperature, air", "Wetted Width", 
          "Cross-Section Depth", "RBP Stream Velocity", "Temperature, water",
          "pH", "Dissolved oxygen saturation", "Dissolved oxygen (DO)"
        )){
          distribution <- "gaussian"
          
        } else if (Char_Names[j] %in% c(
          "Base flow discharge", "Specific conductance", "Salinity", "Conductivity", 
          "Solids, Dissolved (TDS)", "Total Nitrogen, mixed forms", "Nitrogen, ammonia (NH3) as NH3",
          "Total Phosphorus, mixed forms", "Phosphorus, orthophosphate as PO4",
          "Chlorine", "Turbidity"
        )){
          distribution <- "gamma"
          
        } else {distribution=="gaussian"} # In case something new pops up
        
        
        #### Define functions ####################################################
        
        ##### Analysis function #####
        
        
        ##### Constraint Extraction function #####
        source("SoftConstraintsFunc.R")
        
        
        #### Run Functions #######################################################
        Soft_Limits <- SoftConstraintsFunc(Water_Data, Site_Names[i], Char_Names[j], distribution, Model_Year[k], To_Plot)
        
        ###### Figure out which data sets have enough data #####
        
        ##### run analysis #####
        
        ##### extract contstraints #####
        
        ##### Do whatever fiddly stuff is needed for sites / characteristics with low data #####
        
        
        #### Export Results ######################################################
        
        ##### get data into correct format #####
        # Get soft characteristic column names
        soft_names <- lookup$SoftConstraints_name[lookup$CharacteristicName==Char_Names[j]]
        
        # Fill matching cells in softCharacteristics
        softConstraints[softConstraints$Year==Model_Year[k] &
                          softConstraints$Location_ID==Site_Names[i], 
                        soft_names] <- Soft_Limits[,2:3]
      }
      
      # Print progress
      print(paste("Finished", ticker, "out of", (length(Site_Names)*length(Char_Names)*length(Model_Year))))
      ticker <- ticker + 1
    }
  }
}

###### export #####
write.csv(softConstraints,"SoftConstraints-Filled.csv",row.names = F,quote=F)
