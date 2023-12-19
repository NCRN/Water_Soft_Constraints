#### Load Libraries ####

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


#### Read in Data ####

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


#### Create Vectors that store which sites and characteristics we want ####

Site_Names <- NA
Char_Names <- NA

#### Filter data and format to what we want ####



#### Define functions ####

##### Analysis function #####


##### Constraint Extraction function #####
source("SoftConstraintsFunc.R")


#### Run Functions ####

###### Figure out which data sets have enough data #####

##### run analysis #####

##### extract contstraints #####

##### Do whatever fiddly stuff is needed for sites / characteristics with low data #####




#### Export Results ####

##### get data into correct format #####
# Get soft characteristic column names
soft_names <- lookup$SoftConstraints_name[lookup$CharacteristicName==Char_Names]

# Fill matching cells in softCharacteristics
softConstraints[softConstraints$Year==2023 &
                  softConstraints$Location_ID==Site_Names, 
                soft_names] <- Soft_Limits[,2:3]

###### export #####
write.csv(softConstraints,"SoftConstraints-Filled.csv",row.names = F,quote=F)

