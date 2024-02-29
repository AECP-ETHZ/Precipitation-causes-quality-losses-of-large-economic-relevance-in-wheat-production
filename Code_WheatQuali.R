# =================================================================================================
# =================================================================================================
# -------------------------------------------------------------------------------------------------
# Publication: Precipitation causes quality losses of large economic relevance in wheat production
# Authors: Janic Bucheli, Margot Visse-Mansiaux, Juan Herrera, Lilia Levy Häner, Jesse Tack 
#          and Robert Finger
# Published in Q-Open in 2024.
# -------------------------------------------------------------------------------------------------
# =================================================================================================
# =================================================================================================

# Download required packages
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(fixest)
library(reshape2)
library(lmtest)
library(sandwich)
library(DescTools)

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 1) Read and prepare the varietal trial data
# -------------------------------------------------------------------------------------------------
# =================================================================================================

setwd("H:/Working Papers/Wheat Quality Paper/Raw_Data")
cols_interest <- c("site", "siteId","weather_station","variety","plotArea_m2","plot","sitePostalCode","sowingDate", "harvestDate", "year", "yield_dtPerHa","timeToFallDry_sec","heading_days","varietyAlternativeName")

# -----------------------------
# 2008 to 2012
# -----------------------------

filenames0812 <- list.files(path="2008-2012", full.names = T)

temp1 <- as.data.frame(read_excel(filenames0812[1]))
temp1 <- temp1[,cols_interest]
temp1$heading_days <- as.numeric(temp1$heading_days)
temp1$sowingDate <- as.numeric(temp1$sowingDate)
temp1$harvestDate <- as.numeric(temp1$harvestDate)

for(f in 2:length(filenames0812)){
  temp <- as.data.frame(read_excel(filenames0812[f]))
  
  # if condition because there are different column names in raw data files
  if ("variety" %in% colnames(temp)){
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate) 
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
  } else{
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate) 
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
    colnames(temp)[which(colnames(temp) == "variety...11")] <- "variety"
  }
  
  temp <-  temp[,cols_interest]
  
  temp1 <- bind_rows(temp1,temp)
  rm(temp)
}

temp_0812 <- temp1
rm(temp1,f,filenames0812)

# -----------------------------
# 2013 to 2015
# -----------------------------

filenames1315 <- list.files(path="2013-2015", full.names = T)

temp1 <- as.data.frame(read_excel(filenames1315[1]))
temp1 <- temp1[,cols_interest]
temp1$heading_days <- as.numeric(temp1$heading_days)
temp1$sowingDate <- as.numeric(temp1$sowingDate)
temp1$harvestDate <- as.numeric(temp1$harvestDate)
temp1$testWeightMittel_kg <- as.numeric(temp1$testWeightMittel_kg)

for(f in 2:length(filenames1315)){
  temp <- as.data.frame(read_excel(filenames1315[f]))
  
  if ("variety" %in% colnames(temp)){
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate)
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
  } else{
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate) 
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
    colnames(temp)[which(colnames(temp) == "variety...11")] <- "variety"
  }
  
  temp <-  temp[,cols_interest]
  
  temp1 <- bind_rows(temp1,temp)
  rm(temp)
}

temp_1315 <- temp1
rm(temp1,f, filenames1315)

# -----------------------------
# 2016 to 2019
# -----------------------------

filenames1619 <- list.files(path="2016-2019", full.names = T)

temp1 <- as.data.frame(read_excel(filenames1619[1]))
temp1 <- temp1[,cols_interest]
temp1$heading_days <- as.numeric(temp1$heading_days)
temp1$sowingDate <- as.numeric(temp1$sowingDate)
temp1$harvestDate <- as.numeric(temp1$harvestDate)
temp1$plotArea_m2 <- as.numeric(temp1$plotArea_m2)

for(f in 2:length(filenames1619)){
  temp <- as.data.frame(read_excel(filenames1619[f]))
  
  if ("variety" %in% colnames(temp)){
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate) 
    temp$plotArea_m2 <- as.numeric(temp$plotArea_m2)
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
  } else{
    temp$heading_days <- as.numeric(temp$heading_days)
    temp$yield_dtPerHa <- as.numeric(temp$yield_dtPerHa)
    temp$proteinYield_dtPerHa <- as.numeric(temp$proteinYield_dtPerHa)
    temp$humidityPercentageInGrain <- as.numeric(temp$humidityPercentageInGrain)
    temp$timeToFallDry_sec <- as.numeric(temp$timeToFallDry_sec)
    temp$sowingDate <- as.numeric(temp$sowingDate)
    temp$harvestDate <- as.numeric(temp$harvestDate) 
    temp$plotArea_m2 <- as.numeric(temp$plotArea_m2)
    temp$testWeightMittel_kg <- as.numeric(temp$testWeightMittel_kg)
    colnames(temp)[which(colnames(temp) == "variety...11")] <- "variety"
  }
  
  temp <-  temp[,cols_interest]
  
  temp1 <- bind_rows(temp1,temp)
  rm(temp)
}

temp_1619 <- temp1
rm(temp1,f,filenames1619)

# -----------------------------
# Bring all data together
# -----------------------------

data_final <- bind_rows(temp_0812, temp_1315, temp_1619)
rm(temp_0812, temp_1315, temp_1619, filenames0812, filenames1315, filenames1619, cols_interest)

# Consistent labelling of variety names
data_final[data_final == "CH CLARO"] <- "CH_CLARO"
data_final[data_final == "CH CAMEDO"] <- "CH_CAMEDO"
data_final[data_final == "CH NARA"] <- "CH_NARA"
data_final[data_final == "BOCKRIS_(2)"] <- "BOCKRIS"
data_final[data_final == "ARNOLD_(BLE)"] <- "ARNOLD"
data_final[data_final == "ARNOLD (BLE)"] <- "ARNOLD"
data_final[data_final == "GENIUS (2)"] <- "GENIUS"
data_final[data_final == "GENIUS_(2)"] <- "GENIUS"
data_final[which(data_final$variety == "DILLAGO"),"variety"] <- as.character("DILAGO")
data_final[which(data_final$variety == "DILAGGO"),"variety"] <- as.character("DILAGO")

# Replace NA with the alternative variety name
data_final[which(data_final$variety == "NA"),"variety"] <- data_final[which(data_final$variety == "NA"),"varietyAlternativeName"]

# Drop observations for which the variety is unknown (these varieties are not introduced onto the market)
# We need to know the variety to allocate it to the quality class
data_final <- data_final[which(data_final$variety != "NA"),]

# -----------------------------
# Add sowing and havrest dates
# -----------------------------

data_sowing <- read.csv("Meta/Sowing_dates.csv", sep=";")
colnames(data_sowing) <- c("site", seq(2008,2020,1))

for(i in 1:nrow(data_final)){
  data_final[i,"sowingDate"] <- as.character(data_sowing[which(data_sowing$site == data_final[i,"site"]),which(colnames(data_sowing) == as.character(data_final[i,"year"]))])
}
data_final$sowingDate <- as.Date(data_final$sowingDate, format="%Y-%m-%d")
rm(data_sowing,i)

data_harvest <- read.csv("Meta/Harvest_dates.csv", sep=";")
colnames(data_harvest) <- c("site", seq(2008,2020,1))

for(i in 1:nrow(data_final)){
  data_final[i,"harvestDate"] <- as.character(data_harvest[which(data_harvest$site == data_final[i,"site"]),which(colnames(data_harvest) == as.character(data_final[i,"year"]))])
}
data_final$harvestDate <- as.Date(data_final$harvestDate, format="%Y-%m-%d")
rm(data_harvest,i)

# -----------------------------
# Downgrading
# -----------------------------

downgrading_threshold <- 220

data_final$downgrading <- NA

# 1/0 for occurrence of declassification
for(i in 1:nrow(data_final)){
  
  if(is.na(data_final[i,"timeToFallDry_sec"])){
    data_final[i,"downgrading"] <- NA
  } else if(data_final[i,"timeToFallDry_sec"] <= downgrading_threshold){
    data_final[i,"downgrading"] <- 1
  } else (data_final[i,"downgrading"] <- 0)
  
}
rm(i, downgrading_threshold)

# -----------------------------
# Match variety with price class
# -----------------------------

# We derive the classifications from officially published lists of recommended varieties
data_classification <- read.csv("Meta/classification.csv", sep=";")
colnames(data_classification) <- c("variety", seq(2022,2008,-1))
data_classification[data_classification == ""] <- NA

# Use only rows with at least one record, i.e. no NA in each year
# This removes varieties that were not introduced to the list of recommended varieties
# In this list, there are 15 years, i.e. 15 NAs  per row show that the variety is not in the list of recommended varieties
# Note that the quality class (economic class) may change when the variety shows lower relative performance
data_downgrading <- data_classification[which(rowSums(is.na(data_classification[,-1])) != 15),]

# Check whether there are spelling errors
# These are presumably the varieties that were not introduced to the list of recommended varieties
sort(unique(data_final$variety)[which(unique(data_final$variety) %in% data_classification$variety != T)])

# We match by variety and year
data_final$class <- NA

for (i in 1:nrow(data_final)){
  
  if (data_final[i,"variety"] %in% data_downgrading$variety && !is.na(data_downgrading[which(data_downgrading$variety == data_final[i,"variety"]), which(colnames(data_downgrading) == as.character(data_final[i,"year"]))])){
    data_final[i,"class"] <- data_downgrading[which(data_downgrading$variety == data_final[i,"variety"]), which(colnames(data_downgrading) == as.character(data_final[i,"year"]))]
  } else if (data_final[i,"variety"] %in% data_downgrading$variety && is.na(data_downgrading[which(data_downgrading$variety == data_final[i,"variety"]), which(colnames(data_downgrading) == as.character(data_final[i,"year"]))])){
    # Varieties are not immediately in list. Pick the classification from the year of market introduction
    temp1 <- data_downgrading[which(data_downgrading$variety == data_final[i,"variety"]), -1]
    data_final[i,"class"] <- temp1[!is.na(temp1)][length(temp1[!is.na(temp1)])]
  } else {data_final[i,"class"] <- NA}
}
rm(i,temp1)

# This vector shows the varieties that are not in the list of recommended varieties
missing_varieties <- sort(unique(data_final[is.na(data_final$class),"variety"]))

# We drop observations for the class "Bio" because this class was introduced after 2019 and observations that are not in the list of recommended varieties
# We continue with the dataset agronomic_data that contains all the relevant information to answer our research questions
'Allow here yields with NA -> only interested in modelling weather effects on probability of a downgrading'
downgrading_data <- data_final[which(!data_final$class %in% c("Bio", "Futterweizen") & !is.na(data_final$class) & !is.na(data_final$sowingDate) & !is.na(data_final$harvestDate) & !is.na(data_final$downgrading)),]
rm(data_final, missing_varieties, data_classification, data_downgrading)

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 2) Read and prepare meteorological data
# -------------------------------------------------------------------------------------------------
# =================================================================================================

# The maximum number of days prior to harvest for our weather variables of interest
number_days_prior_harvest <- 60

data_harvest <- read.csv("Meta/Harvest_dates.csv", sep=";")
colnames(data_harvest) <- c("site", seq(2008,2020,1))

data_sowing <- read.csv("Meta/Sowing_dates.csv", sep=";")
colnames(data_sowing) <- c("site", seq(2007,2019,1))

# -----------------------------
# 2.1 Precipitation
# -----------------------------

load("Weather/list_weather_data2.RData")

precipitation_data <- list_weather_data[[1]]

# Precipitation amounts d days prior to harvest
# Precipitation array with the structure: [number of days prior to harvest, location, year]
array_cumulative_precipitation_main <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
dimnames(array_cumulative_precipitation_main)[[1]] <- seq(1,number_days_prior_harvest,1)
dimnames(array_cumulative_precipitation_main)[[2]] <- unique(downgrading_data$site)                          
dimnames(array_cumulative_precipitation_main)[[3]] <- seq(2008,2019,1)

for (d in 1:number_days_prior_harvest){
  for (i in 1:length(unique(downgrading_data$site))){
    for (t in 1: length(seq(2008,2019,1))){
      
      if(is.na(data_harvest[which(data_harvest$site == dimnames(array_cumulative_precipitation_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])){
        array_cumulative_precipitation_main[d,i,t] <- NA
      } else{
        temp1 <- precipitation_data[which(row.names(precipitation_data) == dimnames(array_cumulative_precipitation_main)[[2]][i]),(which(colnames(precipitation_data) == data_harvest[which(data_harvest$site == dimnames(array_cumulative_precipitation_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])-number_days_prior_harvest):which(colnames(precipitation_data) == data_harvest[which(data_harvest$site == dimnames(array_cumulative_precipitation_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])]
        array_cumulative_precipitation_main[d,i,t] <- sum(temp1[(length(temp1)-d):(length(temp1)-1)])                                    
        rm(temp1)}                                 
    }
  }
}

rm(d,i,t)

# Precipitation amounts before the d days prior to harvest and from the sowing date (control variable)
array_cumulative_precipitation_control <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
dimnames(array_cumulative_precipitation_control)[[1]] <- seq(1,number_days_prior_harvest,1)
dimnames(array_cumulative_precipitation_control)[[2]] <- unique(downgrading_data$site)                          
dimnames(array_cumulative_precipitation_control)[[3]] <- seq(2008,2019,1)

for (d in 1:number_days_prior_harvest){
  for (i in 1:length(unique(downgrading_data$site))){
    for (t in 1: length(seq(2008,2019,1))){
      
      # Start date is the sowing date
      temp_start <- as.character(data_sowing[which(data_sowing$site == dimnames(array_cumulative_precipitation_control)[[2]][i]),as.character(seq(2007,2018,1)[t])])
      temp_end   <- as.character(as.Date(data_harvest[which(data_harvest$site == dimnames(array_cumulative_precipitation_control)[[2]][i]),as.character(seq(2008,2019,1)[t])])-(d+1))
      
      if(is.na(temp_start)){
        array_cumulative_precipitation_control[d,i,t] <- NA
      } else if (is.na(temp_end)){
        array_cumulative_precipitation_control[d,i,t] <- NA
      } else{
        temp1 <- precipitation_data[which(row.names(precipitation_data) == dimnames(array_cumulative_precipitation_control)[[2]][i]),which(colnames(precipitation_data) == temp_start): which(colnames(precipitation_data) == temp_end)]
        array_cumulative_precipitation_control[d,i,t] <- sum(temp1)                                    
        rm(temp1)}
      rm(temp_start,temp_end)
    }
  }
}

rm(precipitation_data,d,i,t)

# -----------------------------
# 2.2 Temperature
# -----------------------------

Tmax_data <- list_weather_data[[2]]
Tmin_data <- list_weather_data[[3]]

# Baseline temperature for growing degree-days (GDD)
baseT <- 5

# We will loop over various critical temperature thresholds (from 18 to 32)
heat_threshold_range <- seq(25,32)

# Each element contains results for a specific heat threshold
list_GDD_array_cumulative_GDD_HDD_main <- list()
list_HDD_array_cumulative_GDD_HDD_main <- list()

list_GDD_array_cumulative_GDD_HDD_control <- list()
list_HDD_array_cumulative_GDD_HDD_control <- list()

for (h in 1:length(heat_threshold_range)){
  
  # Temperature threshold from which we measure heat stress
  heat_threshold <- heat_threshold_range[h]
  
  # Array for growing degree-days (GDD)
  GDD_array_cumulative_GDD_HDD_main <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
  dimnames(GDD_array_cumulative_GDD_HDD_main)[[1]] <- seq(1,number_days_prior_harvest,1)
  dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]] <- unique(downgrading_data$site)                          
  dimnames(GDD_array_cumulative_GDD_HDD_main)[[3]] <- seq(2008,2019,1)
  
  # Array for heat degree-days (HDD) with temperature exposure above the heat threshold
  HDD_array_cumulative_GDD_HDD_main <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
  dimnames(HDD_array_cumulative_GDD_HDD_main)[[1]] <- seq(1,number_days_prior_harvest,1)
  dimnames(HDD_array_cumulative_GDD_HDD_main)[[2]] <- unique(downgrading_data$site)                          
  dimnames(HDD_array_cumulative_GDD_HDD_main)[[3]] <- seq(2008,2019,1)
  
  for (d in 1:number_days_prior_harvest){
    for (i in 1:length(unique(downgrading_data$site))){
      for (t in 1: length(seq(2008,2019,1))){
        
        if(is.na(data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])){
          GDD_array_cumulative_GDD_HDD_main[d,i,t] <- NA
        } else{
          
          # Get the daily min and max temperature data
          temp_Tmax <- Tmax_data[which(row.names(Tmax_data) == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),(which(colnames(Tmax_data) == data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])-number_days_prior_harvest):which(colnames(Tmax_data) == data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])]
          temp_Tmin <- Tmin_data[which(row.names(Tmin_data) == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),(which(colnames(Tmin_data) == data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])-number_days_prior_harvest):which(colnames(Tmin_data) == data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_main)[[2]][i]),as.character(seq(2008,2019,1)[t])])]
          
          # Daily temperature load for 5-30?C
          temp_daily_load_GDD <- vector(length=d)
          
          # Daily temperature load for > 30?C
          temp_daily_load_HDD <- vector(length=d)
          
          # a) Tmax < baseT
          
          # b) Tmax < heat threshold 
          # b.1) Tmin > baseT
          # b.2) Tmin < baseT
          
          # c) Tmax > heat threshold
          # c.1) HDD + Tmin > baseT
          # c.2) HDD + Tmin < baseT
          
          for (l in 1:length(temp_daily_load_GDD)){
            
            # Get the values to calculate daily temperature temperature curves
            temp_Tmax_dayd <- temp_Tmax[length(temp_Tmax)-l] 
            temp_Tmin_dayd <- temp_Tmin[length(temp_Tmax)-l]
            temp_Tmin_nextday <-  temp_Tmin[length(temp_Tmax)-l+1]
            
            # ----------------------------------------------------------
            # HDD & GDD for first half day (from Tmin_dayd to Tmax_dayd)
            # ----------------------------------------------------------
            
            GDD_firsthalf <- NA
            HDD_firsthalf <- NA
            
            # a) Tmax < baseT
            if (temp_Tmax_dayd < baseT){
              GDD_firsthalf <- 0
              HDD_firsthalf <- 0
              
              # b) Tmax < heat threshold 
            } else if (temp_Tmax_dayd < heat_threshold){
              HDD_firsthalf <- 0
              
              # b.1) Tmin > baseT
              if (temp_Tmin_dayd > baseT){
                
                temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
                temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
                temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = (pi/2))$value - baseT * ((pi/2) - (-pi/2))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, temp_integrand_firsthalf)
                
              } else {
                # b.2) Tmin < baseT
                temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
                temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
                temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
                
                # Get lower bound of integral (crossing of baseT and function)
                lower_integral_bound <- asin((baseT - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
                # Get GDD from first halfday
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound, upper = (pi/2))$value - (baseT * ((pi/2) - lower_integral_bound))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, lower_integral_bound)   
                
              }
              
              # c) Tmax > heat threshold 
            } else {
              
              temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
              temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
              temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
              
              # HDD
              lower_integral_bound_HDD <- asin((heat_threshold - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
              HDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound_HDD, upper = (pi/2))$value - (heat_threshold * ((pi/2) - lower_integral_bound_HDD))
              
              # c.1) HDD + Tmin > baseT 
              if (temp_Tmin_dayd > baseT){
                
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = lower_integral_bound_HDD)$value - (baseT * (lower_integral_bound_HDD - (-pi/2)))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf,temp_integrand_firsthalf, lower_integral_bound_HDD)
              } else{
                
                # c.2) Tmin < baseT
                lower_integral_bound <- asin((baseT - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound, upper = lower_integral_bound_HDD)$value - (baseT * (lower_integral_bound_HDD - lower_integral_bound))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf,temp_integrand_firsthalf, lower_integral_bound_HDD, lower_integral_bound)
              }
              
            }
            
            # -------------------------------------------------------------
            # HDD & GDD for second half day (from Tmax_dayd to Tmin_nextday)
            # -------------------------------------------------------------
            
            GDD_secondhalf <- NA
            HDD_secondhalf <- NA
            
            # a) Tmax < baseT
            if (temp_Tmax_dayd < baseT){
              GDD_secondhalf <- 0
              HDD_secondhalf <- 0
              
              # b) Tmax < heat threshold
            } else if (temp_Tmax_dayd < heat_threshold){
              
              HDD_secondhalf <- 0
              
              # b.1) temp_Tmin_nextday > baseT
              if (temp_Tmin_nextday > baseT){
                
                temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
                temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
                temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (pi/2), upper = (3*pi/2))$value - baseT * ((3*pi/2) - (pi/2))
                rm(temp_meanT_secondhalf, temp_amplitude_secondhalf, temp_integrand_secondhalf)
                
                # b.2) temp_Tmin_nextday < baseT
              } else {
                
                temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
                temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
                temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
                
                temp1 <- asin((baseT - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
                
                # temp1 shows the first intersection of baseT and sine curve. We need the second one.
                # We get the second intersection by adding the absolute time difference between midday and first intersection to midday (pi/2) 
                upper_integral_bound <- (pi/2) + abs((pi/2) - temp1)
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = pi/2, upper = (upper_integral_bound))$value - baseT * ((upper_integral_bound) - (pi/2))
                rm (temp_integrand_secondhalf, upper_integral_bound, temp1, temp_amplitude_secondhalf, temp_meanT_secondhalf)
                
              }
              
              # c) Tmax > heat threshold 
            } else {
              temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
              temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
              temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
              
              # HDD
              temp1 <- asin((heat_threshold - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
              upper_integral_bound_HDD <- (pi/2) + abs((pi/2) - temp1)
              HDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (pi/2), upper = (upper_integral_bound_HDD))$value - (heat_threshold * (upper_integral_bound_HDD-(pi/2)))
              rm(temp1)
              
              # c.1) HDD + Tmin > baseT
              if (temp_Tmin_nextday > baseT){
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (upper_integral_bound_HDD), upper = (3*pi/2))$value - (baseT * ((3*pi/2) - (upper_integral_bound_HDD)))
                rm(temp_meanT_secondhalf, temp_amplitude_secondhalf,temp_integrand_secondhalf, upper_integral_bound_HDD)
                
                # c.2) HDD + Tmin < baseT   
              }else {
                temp1 <- asin((baseT - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
                upper_integral_bound <- (pi/2) + abs((pi/2) - temp1)
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (upper_integral_bound_HDD), upper = upper_integral_bound)$value - (baseT * (upper_integral_bound - upper_integral_bound_HDD))
                rm(temp1)
              }
            }
            
            temp_daily_load_GDD[l] <- GDD_firsthalf + GDD_secondhalf
            temp_daily_load_HDD[l] <- HDD_firsthalf + HDD_secondhalf
            rm(GDD_firsthalf, GDD_secondhalf,HDD_firsthalf, HDD_secondhalf, temp_Tmax_dayd, temp_Tmin_dayd, temp_Tmin_nextday)
            
          } 
          
          GDD_array_cumulative_GDD_HDD_main[d,i,t] <- sum(temp_daily_load_GDD)
          HDD_array_cumulative_GDD_HDD_main[d,i,t] <- sum(temp_daily_load_HDD)
          rm(temp_daily_load_GDD, temp_daily_load_HDD,temp_Tmax, temp_Tmin)
        } 
      }
    }
    
  }
  rm(d,i,t,l) 
  
  GDD_array_cumulative_GDD_HDD_control <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
  dimnames(GDD_array_cumulative_GDD_HDD_control)[[1]] <- seq(1,number_days_prior_harvest,1)
  dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]] <- unique(downgrading_data$site)                          
  dimnames(GDD_array_cumulative_GDD_HDD_control)[[3]] <- seq(2008,2019,1)
  
  # Array for heat degree-days (HDD) with temperature exposure above the heat threshold
  HDD_array_cumulative_GDD_HDD_control <- array(dim=c(number_days_prior_harvest,length(unique(downgrading_data$site)),length(seq(2008,2019,1))))
  dimnames(HDD_array_cumulative_GDD_HDD_control)[[1]] <- seq(1,number_days_prior_harvest,1)
  dimnames(HDD_array_cumulative_GDD_HDD_control)[[2]] <- unique(downgrading_data$site)                          
  dimnames(HDD_array_cumulative_GDD_HDD_control)[[3]] <- seq(2008,2019,1)
  
  for (d in 1:number_days_prior_harvest){
    for (i in 1:length(unique(downgrading_data$site))){
      for (t in 1: length(seq(2008,2019,1))){
        
        if(is.na(data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]][i]),as.character(seq(2008,2019,1)[t])])){
          GDD_array_cumulative_GDD_HDD_control[d,i,t] <- NA
        } else{
          
          # Get the daily min and max temperature data
          temp_start <- as.character(data_sowing[which(data_sowing$site == dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]][i]),as.character(seq(2007,2018,1)[t])])
          temp_end   <- as.character(as.Date(data_harvest[which(data_harvest$site == dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]][i]),as.character(seq(2008,2019,1)[t])])-(d))
          
          temp_Tmax <- Tmax_data[which(row.names(Tmax_data) == dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]][i]),which(colnames(Tmax_data) == temp_start): which(colnames(Tmax_data) == temp_end)]
          temp_Tmin <- Tmin_data[which(row.names(Tmin_data) == dimnames(GDD_array_cumulative_GDD_HDD_control)[[2]][i]),which(colnames(Tmin_data) == temp_start): which(colnames(Tmin_data) == temp_end)]
          
          # Daily temperature load for 5-30?C
          temp_daily_load_GDD <- vector(length=length(temp_Tmax)-1)
          
          # Daily temperature load for > 30?C
          temp_daily_load_HDD <- vector(length=length(temp_Tmax)-1)
          
          # a) Tmax < baseT
          
          # b) Tmax < heat threshold 
          # b.1) Tmin > baseT
          # b.2) Tmin < baseT
          
          # c) Tmax > heat threshold
          # c.1) HDD + Tmin > baseT
          # c.2) HDD + Tmin < baseT
          
          for (l in 1:length(temp_daily_load_GDD)){
            
            # Get the values to calculate daily temperature temperature curves
            temp_Tmax_dayd <- temp_Tmax[l] 
            temp_Tmin_dayd <- temp_Tmin[l]
            temp_Tmin_nextday <-  temp_Tmin[l+1]
            
            # ----------------------------------------------------------
            # HDD & GDD for first half day (from Tmin_dayd to Tmax_dayd)
            # ----------------------------------------------------------
            
            GDD_firsthalf <- NA
            HDD_firsthalf <- NA
            
            # a) Tmax < baseT
            if (temp_Tmax_dayd < baseT){
              GDD_firsthalf <- 0
              HDD_firsthalf <- 0
              
              # b) Tmax < heat threshold 
            } else if (temp_Tmax_dayd < heat_threshold){
              HDD_firsthalf <- 0
              
              # b.1) Tmin > baseT
              if (temp_Tmin_dayd > baseT){
                
                temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
                temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
                temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = (pi/2))$value - baseT * ((pi/2) - (-pi/2))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, temp_integrand_firsthalf)
                
              } else {
                # b.2) Tmin < baseT
                temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
                temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
                temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
                
                # Get lower bound of integral (crossing of baseT and function)
                lower_integral_bound <- asin((baseT - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
                # Get GDD from first halfday
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound, upper = (pi/2))$value - (baseT * ((pi/2) - lower_integral_bound))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, lower_integral_bound)   
                
              }
              
              # c) Tmax > heat threshold 
            } else {
              
              temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
              temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
              temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
              
              # HDD
              lower_integral_bound_HDD <- asin((heat_threshold - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
              HDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound_HDD, upper = (pi/2))$value - (heat_threshold * ((pi/2) - lower_integral_bound_HDD))
              
              # c.1) HDD + Tmin > baseT 
              if (temp_Tmin_dayd > baseT){
                
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = lower_integral_bound_HDD)$value - (baseT * (lower_integral_bound_HDD - (-pi/2)))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf,temp_integrand_firsthalf, lower_integral_bound_HDD)
              } else{
                
                # c.2) Tmin < baseT
                lower_integral_bound <- asin((baseT - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
                GDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = lower_integral_bound, upper = lower_integral_bound_HDD)$value - (baseT * (lower_integral_bound_HDD - lower_integral_bound))
                rm(temp_meanT_firsthalf, temp_amplitude_firsthalf,temp_integrand_firsthalf, lower_integral_bound_HDD, lower_integral_bound)
              }
              
            }
            
            # -------------------------------------------------------------
            # HDD & GDD for second half day (from Tmax_dayd to Tmin_nextday)
            # -------------------------------------------------------------
            
            GDD_secondhalf <- NA
            HDD_secondhalf <- NA
            
            # a) Tmax < baseT
            if (temp_Tmax_dayd < baseT){
              GDD_secondhalf <- 0
              HDD_secondhalf <- 0
              
              # b) Tmax < heat threshold
            } else if (temp_Tmax_dayd < heat_threshold){
              
              HDD_secondhalf <- 0
              
              # b.1) temp_Tmin_nextday > baseT
              if (temp_Tmin_nextday > baseT){
                
                temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
                temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
                temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (pi/2), upper = (3*pi/2))$value - baseT * ((3*pi/2) - (pi/2))
                rm(temp_meanT_secondhalf, temp_amplitude_secondhalf, temp_integrand_secondhalf)
                
                # b.2) temp_Tmin_nextday < baseT
              } else {
                
                temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
                temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
                temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
                
                temp1 <- asin((baseT - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
                
                # temp1 shows the first intersection of baseT and sine curve. We need the second one.
                # We get the second intersection by adding the absolute time difference between midday and first intersection to midday (pi/2) 
                upper_integral_bound <- (pi/2) + abs((pi/2) - temp1)
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = pi/2, upper = (upper_integral_bound))$value - baseT * ((upper_integral_bound) - (pi/2))
                rm (temp_integrand_secondhalf, upper_integral_bound, temp1, temp_amplitude_secondhalf, temp_meanT_secondhalf)
                
              }
              
              # c) Tmax > heat threshold 
            } else {
              temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
              temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
              temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
              
              # HDD
              temp1 <- asin((heat_threshold - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
              upper_integral_bound_HDD <- (pi/2) + abs((pi/2) - temp1)
              HDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (pi/2), upper = (upper_integral_bound_HDD))$value - (heat_threshold * (upper_integral_bound_HDD-(pi/2)))
              rm(temp1)
              
              # c.1) HDD + Tmin > baseT
              if (temp_Tmin_nextday > baseT){
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (upper_integral_bound_HDD), upper = (3*pi/2))$value - (baseT * ((3*pi/2) - (upper_integral_bound_HDD)))
                rm(temp_meanT_secondhalf, temp_amplitude_secondhalf,temp_integrand_secondhalf, upper_integral_bound_HDD)
                
                # c.2) HDD + Tmin < baseT   
              }else {
                temp1 <- asin((baseT - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
                upper_integral_bound <- (pi/2) + abs((pi/2) - temp1)
                
                GDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (upper_integral_bound_HDD), upper = upper_integral_bound)$value - (baseT * (upper_integral_bound - upper_integral_bound_HDD))
                rm(temp1)
              }
            }
            
            temp_daily_load_GDD[l] <- GDD_firsthalf + GDD_secondhalf
            temp_daily_load_HDD[l] <- HDD_firsthalf + HDD_secondhalf
            rm(GDD_firsthalf, GDD_secondhalf,HDD_firsthalf, HDD_secondhalf, temp_Tmax_dayd, temp_Tmin_dayd, temp_Tmin_nextday)
            
          } 
          
          GDD_array_cumulative_GDD_HDD_control[d,i,t] <- sum(temp_daily_load_GDD)
          HDD_array_cumulative_GDD_HDD_control[d,i,t] <- sum(temp_daily_load_HDD)
          rm(temp_daily_load_GDD, temp_daily_load_HDD,temp_Tmax, temp_Tmin, temp_start,temp_end)
          
        } # close else loop
      }
    }
    
  }
  
  list_GDD_array_cumulative_GDD_HDD_main[[h]] <- GDD_array_cumulative_GDD_HDD_main
  list_HDD_array_cumulative_GDD_HDD_main[[h]] <- HDD_array_cumulative_GDD_HDD_main
  
  list_GDD_array_cumulative_GDD_HDD_control[[h]] <- GDD_array_cumulative_GDD_HDD_control
  list_HDD_array_cumulative_GDD_HDD_control[[h]] <- HDD_array_cumulative_GDD_HDD_control
  
  rm(GDD_array_cumulative_GDD_HDD_main, HDD_array_cumulative_GDD_HDD_main,GDD_array_cumulative_GDD_HDD_control, HDD_array_cumulative_GDD_HDD_control)
  print(h / length(heat_threshold_range))
  
}

rm(h,i,l,d,t,Tmax_data, Tmin_data)
rm(data_harvest, data_sowing, heat_threshold)

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 3) Core model: Precipitation effects
# -------------------------------------------------------------------------------------------------
# =================================================================================================

# We use region x year clusters to account for spatial and temporal autocorrelation
west <- c("Changins","Moudon","Grangeneuve","Zollikofen","Courtemelon")
east <- c("Riedholz","Liebegg","Neuhausen","Lindau","Salenstein")

downgrading_data$cluster_region <- NA

for(i in 1:nrow(downgrading_data)){
  if (downgrading_data[i,"site"] %in% west){
    downgrading_data[i,"cluster_region"] <- "west"
  } else if (downgrading_data[i,"site"] %in% east){
    downgrading_data[i,"cluster_region"] <- "east"
  } else ( downgrading_data[i,"cluster_region"] <- NA)
}

downgrading_data$cluster_regionXyear <- paste(downgrading_data$cluster_region,"X",downgrading_data$year, sep="")
downgrading_data$cluster_siteXyear <- paste(downgrading_data$site,"X",downgrading_data$year, sep="")

# -----------------------------
# 3.1 Core model
# -----------------------------

# Precipitation, GDD, HDD in period 1 & 2. 
# Location and year FE

RSS_reg_core  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames( RSS_reg_core) <- heat_threshold_range

reg_input_core <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear","cluster_siteXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core$precip_main <- NA
    reg_input_core$precip_control <- NA
    reg_input_core$GDD_main <- NA
    reg_input_core$HDD_main <- NA
    reg_input_core$GDD_control <- NA
    reg_input_core$HDD_control <- NA
    
    for(i in 1:nrow(reg_input_core)){
      reg_input_core[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core[i,"year"])]
      reg_input_core[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_core[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_core[i,"year"])]
      
      reg_input_core[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
      reg_input_core[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
      
      reg_input_core[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
      reg_input_core[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
      
    }
    
    temp <- feols(downgrading ~ precip_main  + GDD_main + HDD_main  + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core)
    RSS_reg_core[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

which(RSS_reg_core == min(RSS_reg_core), arr.ind = TRUE)
rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
h = which(RSS_reg_core == min(RSS_reg_core), arr.ind = TRUE)[2]
d = which(RSS_reg_core == min(RSS_reg_core), arr.ind = TRUE)[1]
rm(RSS_reg_core)

# Second, re-estimation of model with largest goodness of fit

reg_input_core$precip_main <- NA
reg_input_core$precip_control <- NA
reg_input_core$GDD_main <- NA
reg_input_core$HDD_main <- NA
reg_input_core$GDD_control <- NA
reg_input_core$HDD_control <- NA

for (i in 1:nrow(reg_input_core)){
  reg_input_core[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core[i,"year"])]
  reg_input_core[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_core[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_core[i,"year"])]
  
  reg_input_core[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
  reg_input_core[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
  
  reg_input_core[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
  reg_input_core[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
}
core_model <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core)
sum(resid(core_model)^2)
AIC(core_model)
BIC(core_model)

core_model_cluster_locXyear <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_siteXyear, data = reg_input_core)
core_model_cluster_year <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ year, data = reg_input_core)
core_model_cluster_none <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, data = reg_input_core)
 

# -----------------------------
# 3.2 Additional core models
# -----------------------------

# 3.2.1 No temperature control
# 3.2.2 No precipitation in period 2
# 3.2.3 No control variables

# ----------------------------------
# 3.2.1 No precipitation in period 2
# ----------------------------------

RSS_reg_core_M2  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames(RSS_reg_core_M2) <- heat_threshold_range

reg_input_core_M2 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core_M2$precip_main <- NA
    reg_input_core_M2$GDD_main <- NA
    reg_input_core_M2$HDD_main <- NA
    reg_input_core_M2$GDD_control <- NA
    reg_input_core_M2$HDD_control <- NA
    
    for(i in 1:nrow(reg_input_core_M2)){
      reg_input_core_M2[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M2[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M2[i,"year"])]
      
      reg_input_core_M2[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
      reg_input_core_M2[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
      
      reg_input_core_M2[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
      reg_input_core_M2[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
    }
    
    temp <- feols(downgrading ~ precip_main + HDD_main + GDD_main + GDD_control + HDD_control | variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M2)
    RSS_reg_core_M2[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
h = which(RSS_reg_core_M2 == min(RSS_reg_core_M2), arr.ind = TRUE)[2]
d = which(RSS_reg_core_M2 == min(RSS_reg_core_M2), arr.ind = TRUE)[1]
rm(RSS_reg_core_M2)

# Second, re-estimation of model with largest goodness of fit

reg_input_core_M2$precip_main <- NA
reg_input_core_M2$GDD_main <- NA
reg_input_core_M2$HDD_main <- NA
reg_input_core_M2$GDD_control <- NA
reg_input_core_M2$HDD_control <- NA


for (i in 1:nrow(reg_input_core_M2)){
  reg_input_core_M2[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M2[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M2[i,"year"])]
  
  reg_input_core_M2[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
  reg_input_core_M2[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_core[i,"year"])] 
  
  reg_input_core_M2[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
  reg_input_core_M2[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_core[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_core[i,"year"])]
}

core_model_M2 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M2)
sum(resid(core_model_M2)^2)
AIC(core_model_M2)
BIC(core_model_M2)

# -----------------------------
# 3.2.2 No temperature control
# -----------------------------

RSS_reg_core_M3  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames( RSS_reg_core_M3) <- heat_threshold_range

reg_input_core_M3 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core_M3$precip_main <- NA
    reg_input_core_M3$precip_control <- NA
    
    for(i in 1:nrow(reg_input_core_M3)){
      reg_input_core_M3[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M3[i,"year"])]
      reg_input_core_M3[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_core_M3[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_core_M3[i,"year"])]
    }
    
    temp <- feols(downgrading ~ precip_main + precip_control | variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M3)
    RSS_reg_core_M3[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
d = which(RSS_reg_core_M3 == min(RSS_reg_core_M3), arr.ind = TRUE)[1]
rm(RSS_reg_core_M3)

# Second, re-estimation of model with largest goodness of fit

reg_input_core_M3$precip_main <- NA
reg_input_core_M3$precip_control <- NA


for (i in 1:nrow(reg_input_core_M3)){
  reg_input_core_M3[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M3[i,"year"])]
  reg_input_core_M3[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_core_M3[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_core_M3[i,"year"])]
  
}
core_model_M3 <- feols(downgrading ~ precip_main + precip_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M3)
sum(resid(core_model_M3)^2)
AIC(core_model_M3)
BIC(core_model_M3)


# ----------------------------------
# 3.2.3 No controls
# ----------------------------------

RSS_reg_core_M4  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames(RSS_reg_core_M4) <- heat_threshold_range

reg_input_core_M4 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core_M4$precip_main <- NA
    
    for(i in 1:nrow(reg_input_core_M4)){
      reg_input_core_M4[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M4[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M4[i,"year"])]
      
    }
    
    temp <- feols(downgrading ~ precip_main| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M4)
    RSS_reg_core_M4[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
d = which(RSS_reg_core_M4 == min(RSS_reg_core_M4), arr.ind = TRUE)[1]
rm(RSS_reg_core_M4)

# Second, re-estimation of model with largest goodness of fit

reg_input_core_M4$precip_main <- NA

for (i in 1:nrow(reg_input_core_M4)){
  reg_input_core_M4[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M4[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M4[i,"year"])]
}

core_model_M4 <- feols(downgrading ~ precip_main| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_core_M4)
sum(resid(core_model_M4)^2)
AIC(core_model_M4)
BIC(core_model_M4)

# ----------------------------------
# 3.2.4 No region fixed effects
# ----------------------------------

RSS_reg_core_M5  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames(RSS_reg_core_M5) <- heat_threshold_range

reg_input_core_M5 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core_M5$precip_main <- NA
    
    for(i in 1:nrow(reg_input_core_M5)){
      reg_input_core_M5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M5[i,"year"])]
      
    }
    
    temp <- feols(downgrading ~ precip_main|variety ,cluster = ~ cluster_regionXyear, data = reg_input_core_M5)
    RSS_reg_core_M5[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
d = which(RSS_reg_core_M5 == min(RSS_reg_core_M5), arr.ind = TRUE)[1]
rm(RSS_reg_core_M5)

# Second, re-estimation of model with largest goodness of fit

reg_input_core_M5$precip_main <- NA

for (i in 1:nrow(reg_input_core_M5)){
  reg_input_core_M5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M5[i,"year"])]
}

core_model_M5 <- feols(downgrading ~ precip_main| variety, cluster = ~ cluster_regionXyear, data = reg_input_core_M5)
sum(resid(core_model_M5)^2)
AIC(core_model_M5)
BIC(core_model_M5)

# ----------------------------------
# 3.2.5 No fixed effects
# ----------------------------------

RSS_reg_core_M6  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames(RSS_reg_core_M6) <- heat_threshold_range

reg_input_core_M6 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_core_M6$precip_main <- NA
    
    for(i in 1:nrow(reg_input_core_M6)){
      reg_input_core_M6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M6[i,"year"])]
      
    }
    
    temp <- lm(downgrading ~ precip_main, data = reg_input_core_M6)
    RSS_reg_core_M6[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
d = which(RSS_reg_core_M6 == min(RSS_reg_core_M6), arr.ind = TRUE)[1]
rm(RSS_reg_core_M6)

# Second, re-estimation of model with largest goodness of fit

reg_input_core_M6$precip_main <- NA

for (i in 1:nrow(reg_input_core_M6)){
  reg_input_core_M6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_core_M6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_core_M6[i,"year"])]
}

core_model_M6 <- lm(downgrading ~ precip_main, data = reg_input_core_M6)
coeftest(core_model_M6, vcov = vcovCL, cluster = ~ cluster_regionXyear)
summary(core_model_M6)
sum(resid(core_model_M6)^2)
AIC(core_model_M6)
BIC(core_model_M6)

# ----------------------------------
# 3.3 Coefficient plot
# ----------------------------------

coef_plot_core <- as.data.frame(matrix(NA, nrow=6, ncol=7))
colnames(coef_plot_core) <- c("Model","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

coef_plot_core[,1] <- as.factor(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"))

# Point estimates
coef_plot_core[1,2] <- core_model$coefficients[1]
coef_plot_core[2,2] <- core_model_M2$coefficients[1]
coef_plot_core[3,2] <- core_model_M3$coefficients[1]
coef_plot_core[4,2] <- core_model_M4$coefficients[1]
coef_plot_core[5,2] <- core_model_M5$coefficients[1]
coef_plot_core[6,2] <- core_model_M6$coefficients[2]

# SE
coef_plot_core[1,3] <- core_model$se[1]
coef_plot_core[2,3] <- core_model_M2$se[1]
coef_plot_core[3,3] <- core_model_M3$se[1]
coef_plot_core[4,3] <- core_model_M4$se[1]
coef_plot_core[5,3] <- core_model_M5$se[1]
coef_plot_core[6,3] <- coeftest(core_model_M6, vcov = vcovCL, cluster = ~ cluster_regionXyear)[2,2]

# 95% CI
coef_plot_core[,4] <- coef_plot_core[,2] - 1.96 * coef_plot_core[,3]
coef_plot_core[,5] <- coef_plot_core[,2] + 1.96 * coef_plot_core[,3]

# 99% CI
coef_plot_core[,6] <- coef_plot_core[,2] - 2.576 * coef_plot_core[,3]
coef_plot_core[,7] <- coef_plot_core[,2] + 2.576 * coef_plot_core[,3]

coef_plot_core[,2] <- coef_plot_core[,2] * 100 
coef_plot_core[,4:7] <- coef_plot_core[,4:7] * 100 

#plot

plot_coefficient <- ggplot(coef_plot_core)+ggtitle("")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="TT Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="TT Times New Roman", size=10, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="TT Times New Roman", size=11, colour = "grey25"),
        axis.text.x = element_text(family="TT Times New Roman", size=11),
        axis.text.y = element_text(family="TT Times New Roman", size=9),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
   xlab("")+ylab("increased risk of a downgrading\n\n[percentage points]")+ ylim(-0.02, 0.25)+

  geom_linerange(aes(x=Model, ymin= CI99_down, ymax=CI99_up ), lwd= 0.6, color ="#fc8d59")+
  geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up ), lwd= 0.8, color ="dodgerblue4")+
  geom_point(aes(x=Model,y=point), size= 1.3)

ggsave(plot=plot_coefficient,"coefficient_plot.png", width = 16, height = 10, unit="cm")

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 4) Robustness checks: Precipitation effects
# -------------------------------------------------------------------------------------------------
# =================================================================================================

# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.1 Robustness check 1: Omitted variable bias
# ---------------------------------------------------------
# ---------------------------------------------------------

# a) Time trend
# b) Hail events
# c) Freezing degree-days

reg_input_RC1 <- reg_input_core 

# ---------------------------------------------------------
# 4.1a Time trend
# ---------------------------------------------------------

reg_input_RC1 <- reg_input_core 

reg_input_RC1$squ_year <- (reg_input_RC1$year)^2

RC1_model_year_linTrend <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control + year|variety + site, cluster = ~ cluster_regionXyear, data = reg_input_RC1)
RC1_model_year_quadTrend <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control + year + squ_year|variety + site, cluster = ~ cluster_regionXyear, data = reg_input_RC1)
RC1_model_year_yearFE <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control |variety + site + year, cluster = ~ cluster_regionXyear, data = reg_input_RC1)

# ---------------------------------------------------------
# 4.1b Major hail events
# ---------------------------------------------------------

# Two major hail events (2009 in Grangeneuve and 2012 in Lindau)
# Only 32 observations with a hail event
reg_input_RC1$hail <- NA

for (i in 1:nrow(reg_input_RC1)){
  if (reg_input_RC1[i,"year"] == "2009" & reg_input_RC1[i,"site"] == "Grangeneuve"){
    reg_input_RC1[i,"hail"] <- 1
  } else if (reg_input_RC1[i,"year"] == "2012" & reg_input_RC1[i,"site"] == "Lindau"){
    reg_input_RC1[i,"hail"] <- 1
  } else ( reg_input_RC1[i,"hail"] <- 0)
}

RC1_model_year_hail <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control + as.factor(hail) |variety + site, cluster = ~ cluster_regionXyear, data = reg_input_RC1)

# ---------------------------------------------------------
# 4.1c Freezing degree-days
# ---------------------------------------------------------

# ---------------------------------------------------------
# Calculate freezing degree-days 
# ---------------------------------------------------------

load("Weather/list_weather_data2.RData")

data_harvest <- read.csv("Meta/Harvest_dates.csv", sep=";")
colnames(data_harvest) <- c("site", seq(2008,2020,1))

data_sowing <- read.csv("Meta/Sowing_dates.csv", sep=";")
colnames(data_sowing) <- c("site", seq(2007,2019,1))

Tmax_data <- list_weather_data[[2]]
Tmin_data <- list_weather_data[[3]]

FDD_df <- as.data.frame(matrix(NA, nrow=length(unique(downgrading_data$site)), ncol=length(seq(2008,2019,1))))
row.names(FDD_df) <-  unique(downgrading_data$site) 
colnames(FDD_df) <- seq(2008,2019,1)


  for (i in 1:length(unique(downgrading_data$site))){
    for (t in 1: length(seq(2008,2019,1))){
      
      if(is.na(data_harvest[which(data_harvest$site == row.names(FDD_df)[i]),as.character(seq(2008,2019,1)[t])])){
        FDD_df[i,t] <- NA
      } else{
        
        # Get the daily min and max temperature data
        temp_start <- as.character(data_sowing[which(data_sowing$site == row.names(FDD_df)[i]),as.character(seq(2007,2018,1)[t])])
        temp_end   <- as.character(as.Date(data_harvest[which(data_harvest$site == row.names(FDD_df)[i]),as.character(seq(2008,2019,1)[t])]))
        
        temp_Tmax <- Tmax_data[which(row.names(Tmax_data) == row.names(FDD_df)[i]),which(colnames(Tmax_data) == temp_start): which(colnames(Tmax_data) == temp_end)]
        temp_Tmin <- Tmin_data[which(row.names(Tmin_data) == row.names(FDD_df)[i]),which(colnames(Tmin_data) == temp_start): which(colnames(Tmin_data) == temp_end)]
        
        # Daily temperature load for freezing degree-days
        temp_daily_load_FDD <- vector(length=length(temp_Tmax)-1)
        
        
        # a) Tmim > 0
        
        # b) Tmax < 0
       
        # c) Tmin < 0 & Tmax > 0
        
        for (l in 1:length(temp_daily_load_FDD)){
          
          # Get the values to calculate daily temperature temperature curves
          temp_Tmax_dayd <- temp_Tmax[l] 
          temp_Tmin_dayd <- temp_Tmin[l]
          temp_Tmin_nextday <-  temp_Tmin[l+1]
          
          # ----------------------------------------------------------
          # FDD for first half day (from Tmin_dayd to Tmax_dayd)
          # ----------------------------------------------------------
          
          FDD_firsthalf <- NA
          
          # a) Tmin > 0
          if (temp_Tmin_dayd > 0){
            
            FDD_firsthalf <- 0
            
            # b) Tmax < 0
          } else if (temp_Tmax_dayd < 0){
          
            
              temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
              temp_amplitude_firsthalf <- (temp_Tmax_dayd - temp_Tmin_dayd) / 2
              temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
              FDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = (pi/2))$value
              rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, temp_integrand_firsthalf)
            
            
            # c) Tmin < & Tmax > 0
          } else {
            
            temp_meanT_firsthalf <- (temp_Tmax_dayd + temp_Tmin_dayd) / 2
            temp_amplitude_firsthalf <- abs(temp_Tmax_dayd - temp_Tmin_dayd) / 2
            temp_integrand_firsthalf <- function(x){temp_meanT_firsthalf + temp_amplitude_firsthalf * sin(x)}
            
            
            # FDD
            integral_bound_FDD <- asin((0 - temp_meanT_firsthalf) / temp_amplitude_firsthalf)
            FDD_firsthalf <- integrate(temp_integrand_firsthalf, lower = (-pi/2), upper = integral_bound_FDD)$value
            rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, temp_integrand_firsthalf)}
            
          
          # -------------------------------------------------------------
          # FDD for second half day (from Tmax_dayd to Tmin_nextday)
          # -------------------------------------------------------------
          
          FDD_secondhalf <- NA
          
          # a) Tmin > 0
          if (temp_Tmin_nextday > 0){
            FDD_secondhalf <- 0
            
            # b) Tmax < 0
          } else if (temp_Tmax_dayd < 0){
            
        
            temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
            temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
            temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
            FDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = (pi/2), upper = (3*pi/2))$value
            rm(temp_meanT_firsthalf, temp_amplitude_firsthalf, temp_integrand_firsthalf)
            
           # c) Tmax > 0
          } else{
            
            temp_meanT_secondhalf <- (temp_Tmax_dayd + temp_Tmin_nextday) / 2
            temp_amplitude_secondhalf <- (temp_Tmax_dayd - temp_Tmin_nextday) / 2
            temp_integrand_secondhalf <- function(x){temp_meanT_secondhalf + temp_amplitude_secondhalf * sin(x)}
            
            temp1 <- asin((0 - temp_meanT_secondhalf) / temp_amplitude_secondhalf)
            
            # temp1 shows the first intersection of baseT and sine curve. We need the second one.
            # We get the second intersection by adding the absolute time difference between midday and first intersection to midday (pi/2) 
            lower_integral_bound <- (pi/2) + abs((pi/2) - temp1)
            FDD_secondhalf <- integrate(temp_integrand_secondhalf, lower = lower_integral_bound, upper = (3 * pi / 2))$value
            
            temp_daily_load_FDD[l] <- FDD_firsthalf + FDD_secondhalf
            rm(FDD_firsthalf, FDD_secondhalf)
          }
        }
            
        FDD_df[i,t] <- sum(temp_daily_load_FDD)
      }
    }
  } 

# Build FDD into model

h = 3
d = 31

# Second, re-estimation of model with largest goodness of fit

reg_input_RC1$precip_main <- NA
reg_input_RC1$precip_control <- NA
reg_input_RC1$GDD_main <- NA
reg_input_RC1$HDD_main <- NA
reg_input_RC1$GDD_control <- NA
reg_input_RC1$HDD_control <- NA
reg_input_RC1$FDD_control <- NA

for (i in 1:nrow(reg_input_RC1)){
  reg_input_RC1[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC1[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC1[i,"year"])]
  reg_input_RC1[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC1[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC1[i,"year"])]
  
  reg_input_RC1[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC1[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC1[i,"year"])] 
  reg_input_RC1[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC1[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC1[i,"year"])] 
  
  reg_input_RC1[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC1[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC1[i,"year"])]
  reg_input_RC1[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC1[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC1[i,"year"])]

  reg_input_RC1[i,"FDD_control"] <- FDD_df[which(row.names(FDD_df) == reg_input_RC1[i,"site"]), which(colnames(FDD_df) ==  reg_input_RC1[i,"year"])]
}
RC1_model_FDD <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control + FDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC1)
sum(resid(core_model)^2)
AIC(core_model)
BIC(core_model)

# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.2 Robustness check 2: Nonlinear precipitation effects
# ---------------------------------------------------------
# ---------------------------------------------------------

# Piecewise linear splines for precipitation in period 1
# We set 1 knot (but consider knot placements and various quantiles)
# Thus, the search grid contains the length of period 1, heat threshold and knot

# Package to estimate linear splines
library(lspline)

# Quantiles
knot_quantiles <- seq(0.2,0.8,0.1)

# Array to save RSS
RSS_reg_RC2  <- array(NA, dim = c(length (knot_quantiles), number_days_prior_harvest, length(heat_threshold_range)))
dimnames(RSS_reg_RC2)[[1]] <- knot_quantiles
dimnames(RSS_reg_RC2)[[2]] <- 1:number_days_prior_harvest
dimnames(RSS_reg_RC2)[[3]] <- heat_threshold_range

for (k in 1:length(knot_quantiles)){

  reg_input_RC2 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

  for (h in 1:length(heat_threshold_range)){
    for (d in 1:number_days_prior_harvest){
  
    reg_input_RC2$precip_main <- NA
    reg_input_RC2$precip_control <- NA
    reg_input_RC2$GDD_main <- NA
    reg_input_RC2$HDD_main <- NA
    reg_input_RC2$GDD_control <- NA
    reg_input_RC2$HDD_control <- NA
    
    for(i in 1:nrow(reg_input_RC2)){
      reg_input_RC2[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC2[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC2[i,"year"])]
      reg_input_RC2[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC2[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC2[i,"year"])]
      
      reg_input_RC2[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC2[i,"year"])] 
      reg_input_RC2[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC2[i,"year"])] 
      
      reg_input_RC2[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC2[i,"year"])]
      reg_input_RC2[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC2[i,"year"])]
      
    }
    
    # Get knot locations and spline variables
    knot <- quantile(reg_input_RC2$precip_main,knot_quantiles[k], type=1)
    temp_new_ts_precip_ls <- as.data.frame(lspline(reg_input_RC2$precip_main,knots= knot))
    colnames(temp_new_ts_precip_ls) <- c("Var1","Var2")
    
    temp_reg_input_RC2 <- cbind(reg_input_RC2,temp_new_ts_precip_ls)
    rm(temp_new_ts_precip_ls)
    
    # Spline regression
    
    temp <- feols(downgrading ~ Var1 + Var2   + GDD_main + HDD_main  + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = temp_reg_input_RC2)
    RSS_reg_RC2[k,d,h] <- sum(resid(temp)^2)
    rm(temp, temp_reg_input_RC2,knot)
  }
  print(h/length(heat_threshold_range))
  }
  
  rm(reg_input_RC2)
  print(k / length(knot_quantiles))
}

rm(k,h,d,i)

# 6 (=0.7), 31, 3
k <- which(RSS_reg_RC2 == min(RSS_reg_RC2), arr.ind = TRUE)[1]
d <- which(RSS_reg_RC2 == min(RSS_reg_RC2), arr.ind = TRUE)[2]
h <- which(RSS_reg_RC2 == min(RSS_reg_RC2), arr.ind = TRUE)[3]
rm(RSS_reg_RC2)

reg_input_RC2 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

reg_input_RC2$precip_main <- NA
reg_input_RC2$precip_control <- NA
reg_input_RC2$GDD_main <- NA
reg_input_RC2$HDD_main <- NA
reg_input_RC2$GDD_control <- NA
reg_input_RC2$HDD_control <- NA

for (i in 1:nrow(reg_input_RC2)){
  reg_input_RC2[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC2[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC2[i,"year"])]
  reg_input_RC2[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC2[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC2[i,"year"])]
  
  reg_input_RC2[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC2[i,"year"])] 
  reg_input_RC2[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC2[i,"year"])] 
  
  reg_input_RC2[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC2[i,"year"])]
  reg_input_RC2[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC2[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC2[i,"year"])]
}

knot <- quantile(reg_input_RC2$precip_main,knot_quantiles[k], type=1)
temp_new_ts_precip_ls <- as.data.frame(lspline(reg_input_RC2$precip_main,knots= knot))
colnames(temp_new_ts_precip_ls) <- c("Var1","Var2")

temp_reg_input_RC2 <- cbind(reg_input_RC2,temp_new_ts_precip_ls)
rm(temp_new_ts_precip_ls)

# Spline regression

RC2_model <- feols(downgrading ~ Var1 + Var2   + GDD_main + HDD_main  + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = temp_reg_input_RC2)
AIC(RC2_model)
rm(temp, temp_reg_input_RC2)

# Plot the effect

# Prepare the data (effects and 95% confidence bands)
RC2_effects <- as.data.frame(matrix(NA,nrow=length(seq(min(reg_input_RC2$precip_main), max(reg_input_RC2$precip_main),0.001)), ncol=7))
colnames(RC2_effects) <- c("precipitation","linear_effect","linear_down","linear_up","nonlinear_effect","nonlinear_down","nonlinear_up")

RC2_effects[,1] <- seq(min(reg_input_RC2$precip_main), max(reg_input_RC2$precip_main),0.001)

RC2_effects[,2] <- (RC2_effects[,1] * core_model$coefficients[1]) * 100
RC2_effects[,3] <- RC2_effects[,2] - (RC2_effects[,1]* (1.96 * core_model$se[1]))*100
RC2_effects[,4] <- RC2_effects[,2] + (RC2_effects[,1]* (1.96 * core_model$se[1]))*100

RC2_effects[,5] <- (lspline(RC2_effects[,1], knots = quantile(RC2_effects[,1],knot_quantiles[k], type=1)) %*% RC2_model$coefficients[1:2])*100
RC2_effects[,6] <- RC2_effects[,5] - (lspline(RC2_effects[,1], knots = quantile(RC2_effects[,1],knot_quantiles[k], type=1)) %*% (RC2_model$se[1:2] * 1.96))*100
RC2_effects[,7] <- RC2_effects[,5] + (lspline(RC2_effects[,1], knots = quantile(RC2_effects[,1],knot_quantiles[k], type=1)) %*% (RC2_model$se[1:2] * 1.96))*100


plot_RC2 <- ggplot()+ggtitle("")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=9, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=9),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=1.05)+
  xlab("Cumulative precipitation [mm]") + ylab("increased risk of a downgrading\n\n[percentage points]")+
  scale_x_continuous(breaks = seq(20,260, by=20), limits = c(min(RC2_effects[,1]),max(RC2_effects[,1])))+
  scale_y_continuous(breaks = seq(0,60, by=10), limits = c(0,60))+
 
  
  # Calculated effect
  geom_line(data=RC2_effects, aes(y=linear_effect, x=precipitation), size=1, colour="dodgerblue4")+
  geom_line(data=RC2_effects, aes(y=linear_down, x=precipitation,), size=0.5, colour="dodgerblue4", alpha=1)+
  geom_line(data=RC2_effects, aes(y=linear_up, x=precipitation,), size=0.5, colour="dodgerblue4", alpha=1)+
  geom_ribbon(data=RC2_effects,aes(ymin=linear_down, ymax=linear_up, x=precipitation), fill="dodgerblue4",alpha=0.4)+
  geom_line(data=RC2_effects, aes(y=nonlinear_effect, x=precipitation), size=0.6, colour="red")

ggsave(plot=plot_RC2,"nonlinear_effect_plot.png", width = 16, height = 8, unit="cm")
  
# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.3 Split of period 1 or 2
# ---------------------------------------------------------
# ---------------------------------------------------------

# a) Split of period 1
# b) Split of period 2

# ---------------------------------------------------------
# 4.3a Split of period 1 or 2
# ---------------------------------------------------------

h = 3
d = 31

reg_input_RC3 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]
RSS_reg_RC3 <- vector(length= 31)

list_RC3_model <- list()

for (l in 1:(31)){
  
  reg_input_RC3$precip_main <- NA
  reg_input_RC3$precip_control_P2 <- NA
  reg_input_RC3$precip_control_p3 <- NA
  
  reg_input_RC3$GDD_main <- NA
  reg_input_RC3$HDD_main <- NA
  
  reg_input_RC3$GDD_control_P2 <- NA
  reg_input_RC3$GDD_control_P3 <- NA
  
  reg_input_RC3$HDD_control_P2 <- NA
  reg_input_RC3$HDD_control_P3 <- NA
  
  for (i in 1:nrow(reg_input_RC3)){
    reg_input_RC3[i,"precip_main"] <-array_cumulative_precipitation_main[l, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC3[i,"year"])]
    reg_input_RC3[i,"precip_control_P2"] <- array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC3[i,"year"])] - reg_input_RC3[i,"precip_main"]
    reg_input_RC3[i,"precip_control_P3"] <-  array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC3[i,"year"])]
    
    reg_input_RC3[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [l, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
    reg_input_RC3[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [l, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
    
    reg_input_RC3[i,"GDD_control_P2"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] - reg_input_RC3[i,"GDD_main"]
    reg_input_RC3[i,"HDD_control_P2"] <- reg_input_RC3[i,"HDD_main"] - list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])]-reg_input_RC3[i,"HDD_main"] 
    
    reg_input_RC3[i,"GDD_control_P3"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC3[i,"year"])]
    reg_input_RC3[i,"HDD_control_P3"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC3[i,"year"])]
  }
  
  list_RC3_model[[l]] <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control_P2 + GDD_control_P2 + HDD_control_P2 + precip_control_P3 + GDD_control_P3 + HDD_control_P3| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC3)
  RSS_reg_RC3[l] <- sum(resid(list_RC3_model[[l]])^2)
}
rm(l)

l <- which.min(RSS_reg_RC3)

RC3_model_best <- list_RC3_model[[l]]
RC3_model_Model2 <- list_RC3_model[[7]]
RC3_model_Model3 <- list_RC3_model[[14]]
RC3_model_Model4 <- list_RC3_model[[21]]
RC3_model_Model5 <- list_RC3_model[[28]]

library(performance)

coef_RC3 <- as.data.frame(matrix(NA, nrow=10, ncol=8))
colnames(coef_RC3) <- c("Model","Period","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

check_collinearity(RC3_model_Model4)

# Model
coef_RC3[1:2,1] <- "P1 + P2 (32-34) + P3"
coef_RC3[3:4,1] <- "P1 + P2 (32-38) + P3"
coef_RC3[5:6,1] <- "P1 + P2 (32-45) + P3"
coef_RC3[7:8,1] <- "P1 + P2 (32-52) + P3"
coef_RC3[9:10,1] <- "P1 + P2 (32-59) + P3"

# Period
coef_RC3[,2] <- rep(c("P1","P2"),5)

# Point estimate & se
coef_RC3[1,3] <- RC3_model_best$coefficients[1]
coef_RC3[2,3] <- RC3_model_best$coefficients[4]
coef_RC3[1,4] <- RC3_model_best$se [1]
coef_RC3[2,4] <- RC3_model_best$se [1]

coef_RC3[3,3] <- RC3_model_Model2$coefficients[1]
coef_RC3[4,3] <- RC3_model_Model2$coefficients[4]
coef_RC3[3,4] <- RC3_model_Model2$se [1]
coef_RC3[4,4] <- RC3_model_Model2$se [1]

coef_RC3[5,3] <- RC3_model_Model3$coefficients[1]
coef_RC3[6,3] <- RC3_model_Model3$coefficients[4]
coef_RC3[5,4] <- RC3_model_Model3$se [1]
coef_RC3[6,4] <- RC3_model_Model3$se [1]

coef_RC3[7,3] <- RC3_model_Model4$coefficients[1]
coef_RC3[8,3] <- RC3_model_Model4$coefficients[4]
coef_RC3[7,4] <- RC3_model_Model4$se [1]
coef_RC3[8,4] <- RC3_model_Model4$se [1]

coef_RC3[9,3] <- RC3_model_Model5$coefficients[1]
coef_RC3[10,3] <- RC3_model_Model5$coefficients[4]
coef_RC3[9,4] <- RC3_model_Model5$se [1]
coef_RC3[10,4] <- RC3_model_Model5$se [1]

coef_RC3[,5] <- coef_RC3[,3] - 1.96 * coef_RC3[,4]
coef_RC3[,6] <- coef_RC3[,3] + 1.96 * coef_RC3[,4]

coef_RC3[,7] <- coef_RC3[,3] - 2.576 * coef_RC3[,4]
coef_RC3[,8] <- coef_RC3[,3] + 2.576 * coef_RC3[,4]

coef_RC3[,3] <- coef_RC3[,3] * 100
coef_RC3[,5:8] <- coef_RC3[,5:8] * 100

coef_RC3[,2] <- as.factor(coef_RC3[,2])

plot_coefficient_RC3 <- ggplot(coef_RC3)+ggtitle("Precipitation effect")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_colour_manual(name = "Period",values = c("dodgerblue4","orange4"))+
  #legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
  xlab("")+ylab("marginal precipitation effect [%]")+ ylim(-0.1, 0.3)+
  
  geom_linerange(aes(x=Period, ymin= CI95_down, ymax=CI95_up, colour= Period), lwd= 0.4)+
  #geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up), lwd= 0.8)+
  geom_point(aes(x=Period,y=point, colour=Period), size= 1.3)+facet_grid(~Model)''


# ---------------------------------------------------------
# 4.3b Split of period 1 or 2
# ---------------------------------------------------------

# Loop that changes the length of Period 2 (32-60 days prior to harvest)
# Period 3 is from Period 2 to planting
# Up to 60 because of agronomic reasoning

reg_input_RC3 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]
RSS_reg_RC3 <- vector(length= (60 -d ))

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
h = 3
d = 31

# We define the second period to be 60 days before harvest
RSS_reg_RC3 <- vector(length= (60 -d))
list_RC3_model <- list()

for (l in 1:(60-d)){

reg_input_RC3$precip_main <- NA
reg_input_RC3$precip_control_P2 <- NA
reg_input_RC3$precip_control_p3 <- NA

reg_input_RC3$GDD_main <- NA
reg_input_RC3$HDD_main <- NA

reg_input_RC3$GDD_control_P2 <- NA
reg_input_RC3$GDD_control_P3 <- NA

reg_input_RC3$HDD_control_P2 <- NA
reg_input_RC3$HDD_control_P3 <- NA

for (i in 1:nrow(reg_input_RC3)){
  reg_input_RC3[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC3[i,"year"])]
  reg_input_RC3[i,"precip_control_P2"] <- reg_input_RC3[i,"precip_main"] - array_cumulative_precipitation_main[d+l, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC3[i,"year"])]
  reg_input_RC3[i,"precip_control_P3"] <-  array_cumulative_precipitation_control[d+l, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC3[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC3[i,"year"])]
  
  reg_input_RC3[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
  reg_input_RC3[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
  
  reg_input_RC3[i,"GDD_control_P2"] <- reg_input_RC3[i,"GDD_main"] - list_GDD_array_cumulative_GDD_HDD_main[[h]] [d+l, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
  reg_input_RC3[i,"HDD_control_P2"] <- reg_input_RC3[i,"HDD_main"] - list_HDD_array_cumulative_GDD_HDD_main[[h]] [d+l, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC3[i,"year"])] 
  
  reg_input_RC3[i,"GDD_control_P3"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d+l, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC3[i,"year"])]
  reg_input_RC3[i,"HDD_control_P3"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d+l, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC3[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC3[i,"year"])]
}

list_RC3_model[[l]] <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control_P2 + GDD_control_P2 + HDD_control_P2 + precip_control_P3 + GDD_control_P3 + HDD_control_P3| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC3)
RSS_reg_RC3[l] <- sum(resid(list_RC3_model[[l]])^2)
}
rm(l)

l <- which.min(RSS_reg_RC3)

RC3_model_best <- list_RC3_model[[l]]
RC3_model_Model2 <- list_RC3_model[[7]]
RC3_model_Model3 <- list_RC3_model[[14]]
RC3_model_Model4 <- list_RC3_model[[21]]
RC3_model_Model5 <- list_RC3_model[[28]]

coef_RC3 <- as.data.frame(matrix(NA, nrow=10, ncol=8))
colnames(coef_RC3) <- c("Model","Period","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

# Model
coef_RC3[1:2,1] <- "P1 + P2 (32-34) + P3"
coef_RC3[3:4,1] <- "P1 + P2 (32-38) + P3"
coef_RC3[5:6,1] <- "P1 + P2 (32-45) + P3"
coef_RC3[7:8,1] <- "P1 + P2 (32-52) + P3"
coef_RC3[9:10,1] <- "P1 + P2 (32-59) + P3"

# Period
coef_RC3[,2] <- rep(c("P1","P2"),5)

# Point estimate & se
coef_RC3[1,3] <- RC3_model_best$coefficients[1]
coef_RC3[2,3] <- RC3_model_best$coefficients[4]
coef_RC3[1,4] <- RC3_model_best$se [1]
coef_RC3[2,4] <- RC3_model_best$se [1]

coef_RC3[3,3] <- RC3_model_Model2$coefficients[1]
coef_RC3[4,3] <- RC3_model_Model2$coefficients[4]
coef_RC3[3,4] <- RC3_model_Model2$se [1]
coef_RC3[4,4] <- RC3_model_Model2$se [1]

coef_RC3[5,3] <- RC3_model_Model3$coefficients[1]
coef_RC3[6,3] <- RC3_model_Model3$coefficients[4]
coef_RC3[5,4] <- RC3_model_Model3$se [1]
coef_RC3[6,4] <- RC3_model_Model3$se [1]

coef_RC3[7,3] <- RC3_model_Model4$coefficients[1]
coef_RC3[8,3] <- RC3_model_Model4$coefficients[4]
coef_RC3[7,4] <- RC3_model_Model4$se [1]
coef_RC3[8,4] <- RC3_model_Model4$se [1]

coef_RC3[9,3] <- RC3_model_Model5$coefficients[1]
coef_RC3[10,3] <- RC3_model_Model5$coefficients[4]
coef_RC3[9,4] <- RC3_model_Model5$se [1]
coef_RC3[10,4] <- RC3_model_Model5$se [1]

coef_RC3[,5] <- coef_RC3[,3] - 1.96 * coef_RC3[,4]
coef_RC3[,6] <- coef_RC3[,3] + 1.96 * coef_RC3[,4]

coef_RC3[,7] <- coef_RC3[,3] - 2.576 * coef_RC3[,4]
coef_RC3[,8] <- coef_RC3[,3] + 2.576 * coef_RC3[,4]

coef_RC3[,3] <- coef_RC3[,3] * 100
coef_RC3[,5:8] <- coef_RC3[,5:8] * 100

coef_RC3[,2] <- as.factor(coef_RC3[,2])

plot_coefficient_RC3 <- ggplot(coef_RC3)+ggtitle("Precipitation effect")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_colour_manual(name = "Period",values = c("dodgerblue4","orange4"))+
        #legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
  xlab("")+ylab("marginal precipitation effect [%]")+ ylim(-0.1, 0.3)+
  
  geom_linerange(aes(x=Period, ymin= CI95_down, ymax=CI95_up, colour= Period), lwd= 0.4)+
  #geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up), lwd= 0.8)+
  geom_point(aes(x=Period,y=point, colour=Period), size= 1.3)+facet_grid(~Model)

ggsave(plot=plot_coefficient_RC3,"plot_RC3.png", width = 16, height = 9, unit="cm")

# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.4 Alternative estimators
# ---------------------------------------------------------
# ---------------------------------------------------------

# a) Logit model
# b) Poisson regression
# c) Probit model

reg_input_RC4 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

reg_input_RC4$precip_main <- NA
reg_input_RC4$precip_control <- NA
reg_input_RC4$GDD_main <- NA
reg_input_RC4$HDD_main <- NA
reg_input_RC4$GDD_control <- NA
reg_input_RC4$HDD_control <- NA

d = 31
h = 3

for (i in 1:nrow(reg_input_RC4)){
  reg_input_RC4[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC4[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC4[i,"year"])]
  reg_input_RC4[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC4[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC4[i,"year"])]
  
  reg_input_RC4[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC4[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC4[i,"year"])] 
  reg_input_RC4[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC4[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC4[i,"year"])] 
  
  reg_input_RC4[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC4[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC4[i,"year"])]
  reg_input_RC4[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC4[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC4[i,"year"])]
}

# ---------------------------------------------------------
# 4.4a Logit model
# ---------------------------------------------------------

RC4_logit <- glm(downgrading  ~  precip_main + GDD_main + HDD_main + precip_control + GDD_control + HDD_control + as.factor(variety) + as.factor(site) -1, data=reg_input_RC4, family=binomial(link='logit'))
coeftest(RC4_logit, vcov = vcovCL, cluster = ~ cluster_regionXyear)
PseudoR2(RC4_logit, which="McFaddenAdj")
# ---------------------------------------------------------
# 4.4b Poisson model
# ---------------------------------------------------------

RC4_poisson <- glm(downgrading  ~  precip_main + GDD_main + HDD_main + precip_control + GDD_control + HDD_control + as.factor(variety) + as.factor(site) -1, data=reg_input_RC4, family='poisson')
coeftest(RC4_poisson, vcov = vcovCL, cluster = ~ cluster_regionXyear)
PseudoR2(RC4_poisson , which="McFaddenAdj")

# ---------------------------------------------------------
# 4.4c Probit model
# ---------------------------------------------------------

RC4_probit <- glm(downgrading  ~  precip_main + GDD_main + HDD_main + precip_control + GDD_control + HDD_control + as.factor(variety) + as.factor(site) -1, data=reg_input_RC4, family=binomial(link='probit'))
coeftest(RC4_probit, vcov = vcovCL, cluster = ~ cluster_regionXyear)
PseudoR2(RC4_probit , which="McFaddenAdj")

# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.5 Alternative parameter estimation
# ---------------------------------------------------------
# ---------------------------------------------------------

# Get probability distribution for a downgrading event with core model
prob_core <- core_model$fitted.values

# Threshold as a quantile of the distribution
prob_threshold <- quantile(prob_core, 0.70, type = 1)

# Grid search for parameters
prediction_accuracy_reg_core  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames( prediction_accuracy_reg_core) <- heat_threshold_range

reg_input_RC5 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]
reg_input_RC5$estimated_prob <- NA
reg_input_RC5$accuracy_prob <- NA

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_RC5$precip_main <- NA
    reg_input_RC5$precip_control <- NA
    reg_input_RC5$GDD_main <- NA
    reg_input_RC5$HDD_main <- NA
    reg_input_RC5$GDD_control <- NA
    reg_input_RC5$HDD_control <- NA
    reg_input_RC5$estimated_prob <- NA
    reg_input_RC5$accuracy_prob <- NA
    
    for(i in 1:nrow(reg_input_RC5)){
      reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
      reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
      
      reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
      reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
      
      reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
      reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
      
    }
    
    temp <- feols(downgrading ~ precip_main  + GDD_main + HDD_main  + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)
    
    # Fitted value
    reg_input_RC5$estimated_prob <- temp$fitted.values
    
    for (i in 1:nrow(reg_input_RC5)){
      
      if (reg_input_RC5[i,"estimated_prob"] >= prob_threshold) {
        reg_input_RC5[i,"accuracy_prob"] <- 1
      } else (reg_input_RC5[i,"accuracy_prob"] <- 0) 
        }
    
    temp2 <- reg_input_RC5[,c("downgrading","accuracy_prob")]
    temp2 <- temp2[which(temp2$downgrading == 1),]
    
   
    prediction_accuracy_reg_core[d,h] <- sum( temp2$accuracy_prob) / sum(temp2$downgrading)
    rm(temp,temp2)
  }
  print(h/length(heat_threshold_range))
}

which(prediction_accuracy_reg_core == max(prediction_accuracy_reg_core), arr.ind = TRUE)
rm(h,d,i)

# summary
# quantile 70: h= 3; d=17
# quantile 75: h= 2; d=19
# quantile 80: h= 1; d= 28
# quantile 85: h= 3; d= 31; prob = 87%
# quantile 90: h= 3; d= 29

# 70% quantile

h = 3
d= 17

reg_input_RC5$precip_main <- NA
reg_input_RC5$precip_control <- NA
reg_input_RC5$GDD_main <- NA
reg_input_RC5$HDD_main <- NA
reg_input_RC5$GDD_control <- NA
reg_input_RC5$HDD_control <- NA

for (i in 1:nrow(reg_input_RC5)){
  reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
  
  reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  
  reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
}

RC5_model_q70 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)

# 75% quantile

h = 2
d= 19

reg_input_RC5$precip_main <- NA
reg_input_RC5$precip_control <- NA
reg_input_RC5$GDD_main <- NA
reg_input_RC5$HDD_main <- NA
reg_input_RC5$GDD_control <- NA
reg_input_RC5$HDD_control <- NA

for (i in 1:nrow(reg_input_RC5)){
  reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
  
  reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  
  reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
}

RC5_model_q75 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)

# 80% quantile

h = 1
d= 28

reg_input_RC5$precip_main <- NA
reg_input_RC5$precip_control <- NA
reg_input_RC5$GDD_main <- NA
reg_input_RC5$HDD_main <- NA
reg_input_RC5$GDD_control <- NA
reg_input_RC5$HDD_control <- NA

for (i in 1:nrow(reg_input_RC5)){
  reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
  
  reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  
  reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
}

RC5_model_q80 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)

# 85% quantile

h = 3
d= 31

reg_input_RC5$precip_main <- NA
reg_input_RC5$precip_control <- NA
reg_input_RC5$GDD_main <- NA
reg_input_RC5$HDD_main <- NA
reg_input_RC5$GDD_control <- NA
reg_input_RC5$HDD_control <- NA

for (i in 1:nrow(reg_input_RC5)){
  reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
  
  reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  
  reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
}

RC5_model_q85 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)

# 90% quantile

h = 3
d= 31

reg_input_RC5$precip_main <- NA
reg_input_RC5$precip_control <- NA
reg_input_RC5$GDD_main <- NA
reg_input_RC5$HDD_main <- NA
reg_input_RC5$GDD_control <- NA
reg_input_RC5$HDD_control <- NA

for (i in 1:nrow(reg_input_RC5)){
  reg_input_RC5[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC5[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC5[i,"year"])]
  
  reg_input_RC5[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  reg_input_RC5[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC5[i,"year"])] 
  
  reg_input_RC5[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
  reg_input_RC5[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC5[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC5[i,"year"])]
}

RC5_model_q90 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC5)

# plot results

coef_plot_RC5 <- as.data.frame(matrix(NA, nrow=5, ncol=7))
colnames(coef_plot_RC5) <- c("Model","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

coef_plot_RC5[,1] <- as.factor(c("Model A1", "Model A2", "Model A3", "Model A4", "Model A5"))

# Point estimates
coef_plot_RC5[1,2] <- RC5_model_q70$coefficients[1]
coef_plot_RC5[2,2] <- RC5_model_q75$coefficients[1]
coef_plot_RC5[3,2] <- RC5_model_q80$coefficients[1]
coef_plot_RC5[4,2] <- RC5_model_q85$coefficients[1]
coef_plot_RC5[5,2] <- RC5_model_q90$coefficients[1]

# SE
coef_plot_RC5[1,3] <- RC5_model_q70$se[1]
coef_plot_RC5[2,3] <- RC5_model_q75$se[1]
coef_plot_RC5[3,3] <- RC5_model_q80$se[1]
coef_plot_RC5[4,3] <- RC5_model_q85$se[1]
coef_plot_RC5[5,3] <- RC5_model_q90$se[1]

# 95% CI
coef_plot_RC5[,4] <- coef_plot_RC5[,2] - 1.96 * coef_plot_RC5[,3]
coef_plot_RC5[,5] <- coef_plot_RC5[,2] + 1.96 * coef_plot_RC5[,3]

# 99% CI
coef_plot_RC5[,6] <- coef_plot_RC5[,2] - 2.576 * coef_plot_RC5[,3]
coef_plot_RC5[,7] <- coef_plot_RC5[,2] + 2.576 * coef_plot_RC5[,3]

coef_plot_RC5[,2] <- coef_plot_RC5[,2] * 100 
coef_plot_RC5[,4:7] <- coef_plot_RC5[,4:7] * 100 

#plot

plot_coefficient_RC5 <- ggplot(coef_plot_RC5)+ggtitle("Precipitation effect")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
  xlab("")+ylab("marginal precipitation effect [%]")+ ylim(-0.02, 0.25)+
  
  geom_linerange(aes(x=Model, ymin= CI99_down, ymax=CI99_up ), lwd= 0.4, color ="dodgerblue2")+
  geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up ), lwd= 0.8, color ="dodgerblue4")+
  geom_point(aes(x=Model,y=point), size= 1.3)


# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.6 Without 2014 (catastrophic year)
# ---------------------------------------------------------
# ---------------------------------------------------------

# a) Re-estimation of model including parameters
# b) Parameters from all year (original) on this sub-sample

# ---------------------------------------------------------
# 4.6a: Re-estimation of model including parameters
# ---------------------------------------------------------

RSS_reg_2014  <- matrix(NA, nrow = number_days_prior_harvest, ncol = length(heat_threshold_range))
colnames( RSS_reg_2014) <- heat_threshold_range

reg_input_2014 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]
reg_input_2014 <- reg_input_2014[which(reg_input_2014$year != "2014"),]

# First, grid search for parameters
for (h in 1:length(heat_threshold_range)){
  for (d in 1:number_days_prior_harvest){
    
    reg_input_2014$precip_main <- NA
    reg_input_2014$precip_control <- NA
    reg_input_2014$GDD_main <- NA
    reg_input_2014$HDD_main <- NA
    reg_input_2014$GDD_control <- NA
    reg_input_2014$HDD_control <- NA
    
    for(i in 1:nrow(reg_input_2014)){
      reg_input_2014[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_2014[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_2014[i,"year"])]
      reg_input_2014[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_2014[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_2014[i,"year"])]
      
      reg_input_2014[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014[i,"year"])] 
      reg_input_2014[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014[i,"year"])] 
      
      reg_input_2014[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014[i,"year"])]
      reg_input_2014[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014[i,"year"])]
      
    }
    
    temp <- feols(downgrading ~ precip_main  + GDD_main + HDD_main  + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_2014)
    RSS_reg_2014[d,h] <- sum(resid(temp)^2)
    rm(temp)
  }
  print(h/length(heat_threshold_range))
}

which(RSS_reg_2014 == min(RSS_reg_2014), arr.ind = TRUE)
rm(h,d,i)

# Heat threshold (h) and number of days prior to harvest (d) maximizing goodness of fit 
h = which(RSS_reg_2014 == min(RSS_reg_2014), arr.ind = TRUE)[2]
d = which(RSS_reg_2014 == min(RSS_reg_2014), arr.ind = TRUE)[1]
rm(RSS_reg_2014)

# Second, re-estimation of model with largest goodness of fit

reg_input_2014$precip_main <- NA
reg_input_2014$precip_control <- NA
reg_input_2014$GDD_main <- NA
reg_input_2014$HDD_main <- NA
reg_input_2014$GDD_control <- NA
reg_input_2014$HDD_control <- NA

for (i in 1:nrow(reg_input_2014)){
  reg_input_2014[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_2014[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_2014[i,"year"])]
  reg_input_2014[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_2014[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_2014[i,"year"])]
  
  reg_input_2014[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014[i,"year"])] 
  reg_input_2014[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014[i,"year"])] 
  
  reg_input_2014[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014[i,"year"])]
  reg_input_2014[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014[i,"year"])]
}
model_2014 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_2014)
sum(resid(model_2014)^2)
AIC(model_2014)
BIC(model_2014)

# ---------------------------------------------------------
# 4.6b: Parameters from all year on this sub-sample
# ---------------------------------------------------------

reg_input_2014b <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]
reg_input_2014b <- reg_input_2014[which(reg_input_2014$year != "2014"),]

d <- 31
h <- 2

reg_input_2014b$precip_main <- NA
reg_input_2014b$precip_control <- NA
reg_input_2014b$GDD_main <- NA
reg_input_2014b$HDD_main <- NA
reg_input_2014b$GDD_control <- NA
reg_input_2014b$HDD_control <- NA

for (i in 1:nrow(reg_input_2014b)){
  reg_input_2014b[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_2014b[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_2014b[i,"year"])]
  reg_input_2014b[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_2014b[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_2014b[i,"year"])]
  
  reg_input_2014b[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014b[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014b[i,"year"])] 
  reg_input_2014b[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_2014b[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_2014b[i,"year"])] 
  
  reg_input_2014b[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014b[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014b[i,"year"])]
  reg_input_2014b[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_2014b[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_2014b[i,"year"])]
}

model_2014b <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_2014b)
sum(resid(model_2014b)^2)
AIC(model_2014b)
BIC(model_2014b)

# ---------------------------------------------------------
# ---------------------------------------------------------
# 4.6 Parameter sensitivity
# ---------------------------------------------------------
# ---------------------------------------------------------

# a) Different number of days d
# b) Different heat thresholds h

# ---------------------------------------------------------
# 4.6a Different number of days d
# ---------------------------------------------------------

# Estimate model for d = 10,20,40,50,60
# We keep the identified heat threshold constant
h <- 3

reg_input_RC6 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# d = 10 days
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

d = 10

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_10 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# d = 20 days
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

d = 20

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_20 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# d = 40 days
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

d = 40

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_40 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# d = 50

reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

d = 50

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}



RC6_model_50 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)


# d = 60

reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

d = 60

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_60 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# plot

coef_plot_RC6_d <- as.data.frame(matrix(NA, nrow=5, ncol=7))
colnames(coef_plot_RC6_d) <- c("Model","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

coef_plot_RC6_d[,1] <- as.factor(c("Model A1 (10)", "Model A2 (20)", "Model A3 (40)", "Model A4 (50)","Model A5 (60)" ))

# Point estimates
coef_plot_RC6_d[1,2] <- RC6_model_10$coefficients[1]
coef_plot_RC6_d[2,2] <- RC6_model_20$coefficients[1]
coef_plot_RC6_d[3,2] <- RC6_model_40$coefficients[1]
coef_plot_RC6_d[4,2] <- RC6_model_50$coefficients[1]
coef_plot_RC6_d[5,2] <- RC6_model_60$coefficients[1]

# SE
coef_plot_RC6_d[1,3] <- RC6_model_10$se[1]
coef_plot_RC6_d[2,3] <- RC6_model_20$se[1]
coef_plot_RC6_d[3,3] <- RC6_model_40$se[1]
coef_plot_RC6_d[4,3] <- RC6_model_50$se[1]
coef_plot_RC6_d[5,3] <- RC6_model_60$se[1]

# 95% CI
coef_plot_RC6_d[,4] <- coef_plot_RC6_d[,2] - 1.96 * coef_plot_RC6_d[,3]
coef_plot_RC6_d[,5] <- coef_plot_RC6_d[,2] + 1.96 * coef_plot_RC6_d[,3]

# 99% CI
coef_plot_RC6_d[,6] <- coef_plot_RC6_d[,2] - 2.576 * coef_plot_RC6_d[,3]
coef_plot_RC6_d[,7] <- coef_plot_RC6_d[,2] + 2.576 * coef_plot_RC6_d[,3]

coef_plot_RC6_d[,2] <- coef_plot_RC6_d[,2] * 100 
coef_plot_RC6_d[,4:7] <- coef_plot_RC6_d[,4:7] * 100 

#plot

plot_coefficient_RC6_d <- ggplot(coef_plot_RC6_d)+ggtitle("Precipitation effect")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
  xlab("")+ylab("marginal precipitation effect [%]")+ ylim(-0.1, 0.25)+
  
  geom_linerange(aes(x=Model, ymin= CI99_down, ymax=CI99_up ), lwd= 0.4, color ="dodgerblue2")+
  geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up ), lwd= 0.8, color ="dodgerblue4")+
  geom_point(aes(x=Model,y=point), size= 1.3)

ggsave(plot=plot_coefficient_RC6_d,"coefficient_plot_RC6_d.png", width = 16, height = 8, unit="cm")


# ---------------------------------------------------------
# 4.6a Different heat thresholds h
# ---------------------------------------------------------

# Estimate model for h = 1,2,4,5,6
# We keep the identified number of days n constant
d <- 31

reg_input_RC6 <- downgrading_data[,c("year","site","variety","downgrading","class","cluster_regionXyear")]

# h = 1 (=25)
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

h = 1

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_h1 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# h = 2 (=26)
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

h = 2

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_h2 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# h = 4 (28)
reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

h = 4

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_h4 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# h = 5 (29)

reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

h = 5

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_h5 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# h = 6 (=30)

reg_input_RC6$precip_main <- NA
reg_input_RC6$precip_control <- NA
reg_input_RC6$GDD_main <- NA
reg_input_RC6$HDD_main <- NA
reg_input_RC6$GDD_control <- NA
reg_input_RC6$HDD_control <- NA

# Delete single line below if user does not know best h yet!
h = 6

for (i in 1:nrow(reg_input_RC6)){
  reg_input_RC6[i,"precip_main"] <-array_cumulative_precipitation_main[d, which(dimnames(array_cumulative_precipitation_main)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_main)[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"precip_control"] <- array_cumulative_precipitation_control[d, which(dimnames(array_cumulative_precipitation_control)[[2]] == reg_input_RC6[i,"site"]), which(dimnames(array_cumulative_precipitation_control)[[3]] == reg_input_RC6[i,"year"])]
  
  reg_input_RC6[i,"GDD_main"] <- list_GDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  reg_input_RC6[i,"HDD_main"] <- list_HDD_array_cumulative_GDD_HDD_main[[h]] [d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_main[[h]])[[3]] == reg_input_RC6[i,"year"])] 
  
  reg_input_RC6[i,"GDD_control"] <- list_GDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_GDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
  reg_input_RC6[i,"HDD_control"] <-list_HDD_array_cumulative_GDD_HDD_control[[h]][d, which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[2]] == reg_input_RC6[i,"site"]), which(dimnames(list_HDD_array_cumulative_GDD_HDD_control[[h]])[[3]] == reg_input_RC6[i,"year"])]
}

RC6_model_h6 <- feols(downgrading ~ precip_main  + HDD_main + GDD_main   + precip_control + GDD_control + HDD_control| variety+site, cluster = ~ cluster_regionXyear, data = reg_input_RC6)

# plot

coef_plot_RC6_h <- as.data.frame(matrix(NA, nrow=5, ncol=7))
colnames(coef_plot_RC6_h) <- c("Model","point","se","CI95_down","CI95_up","CI99_down","CI99_up")

coef_plot_RC6_h[,1] <- as.factor(c("Model A1 (25?C)", "Model A2 (26?C)", "Model A3 (28?C)", "Model A4 (29?C)","Model A5 (30?C)" ))

# Point estimates
coef_plot_RC6_h[1,2] <- RC6_model_h1$coefficients[1]
coef_plot_RC6_h[2,2] <- RC6_model_h2$coefficients[1]
coef_plot_RC6_h[3,2] <- RC6_model_h4$coefficients[1]
coef_plot_RC6_h[4,2] <- RC6_model_h5$coefficients[1]
coef_plot_RC6_h[5,2] <- RC6_model_h5$coefficients[1]

# SE
coef_plot_RC6_h[1,3] <- RC6_model_h1$se[1]
coef_plot_RC6_h[2,3] <- RC6_model_h2$se[1]
coef_plot_RC6_h[3,3] <- RC6_model_h4$se[1]
coef_plot_RC6_h[4,3] <- RC6_model_h5$se[1]
coef_plot_RC6_h[5,3] <- RC6_model_h6$se[1]

# 95% CI
coef_plot_RC6_h[,4] <- coef_plot_RC6_h[,2] - 1.96 * coef_plot_RC6_h[,3]
coef_plot_RC6_h[,5] <- coef_plot_RC6_h[,2] + 1.96 * coef_plot_RC6_h[,3]

# 99% CI
coef_plot_RC6_h[,6] <- coef_plot_RC6_h[,2] - 2.576 * coef_plot_RC6_h[,3]
coef_plot_RC6_h[,7] <- coef_plot_RC6_h[,2] + 2.576 * coef_plot_RC6_h[,3]

coef_plot_RC6_h[,2] <- coef_plot_RC6_h[,2] * 100 
coef_plot_RC6_h[,4:7] <- coef_plot_RC6_h[,4:7] * 100 

#plot

plot_coefficient_RC6_h <- ggplot(coef_plot_RC6_h)+ggtitle("Precipitation effect")+
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        plot.subtitle = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=12, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  geom_hline(yintercept=c(0), colour="darkgray", size=0.5, linetype="dashed")+
  xlab("")+ylab("marginal precipitation effect [%]")+ ylim(-0.1, 0.25)+
  
  geom_linerange(aes(x=Model, ymin= CI99_down, ymax=CI99_up ), lwd= 0.4, color ="dodgerblue2")+
  geom_linerange(aes(x=Model, ymin= CI95_down, ymax=CI95_up ), lwd= 0.8, color ="dodgerblue4")+
  geom_point(aes(x=Model,y=point), size= 1.3)

ggsave(plot=plot_coefficient_RC6_h,"coefficient_plot_RC6_h.png", width = 16, height = 8, unit="cm")

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 5) Economic relevance of precipitation effects
# -------------------------------------------------------------------------------------------------
# =================================================================================================

# ---------------------------------------------------------
# 5.1 Simulation of economic losses
# ---------------------------------------------------------

# Omit observations without yield data
downgrading_economics_data <- downgrading_data[which(!is.na(downgrading_data$yield_dtPerHa)),]

class_prices <- read.csv("Meta/class_prices.csv", sep=";")
colnames(class_prices) <- c("class", seq(2008,2021,1))

# We use prices of the next year to fill missing values
# Missing values result when producers and buyers cannot agree on a reference price
# Note that there is almost no price volatility because the market is highly protected so that this should not affect the results

# For 2008
class_prices[2:6,2] <- class_prices[2:6,3]
# For 2010
class_prices[2:6,4] <- class_prices[2:6,5]
# For 2013
class_prices[2:6,7] <- class_prices[2:6,8]

class_prices[,2] <- as.numeric(class_prices[,2])

# Match observation with price of class (no downgrading)
downgrading_economics_data$price_class <- NA

for(i in 1:nrow(downgrading_economics_data)){
  downgrading_economics_data[i,"price_class"] <- class_prices[which(class_prices$class == downgrading_economics_data[i,"class"]), which(colnames(class_prices) == downgrading_economics_data[i,"year"])]
}

# Match observation with price including downgrading
downgrading_economics_data$price_downgrading <- NA

for(i in 1:nrow(downgrading_economics_data)){
  if(downgrading_economics_data[i,"downgrading"] == 0){
    
    downgrading_economics_data[i,"price_downgrading"] <- class_prices[which(class_prices$class == downgrading_economics_data[i,"class"]), which(colnames(class_prices) == downgrading_economics_data[i,"year"])]
  }else (downgrading_economics_data[i,"price_downgrading"] <- class_prices[which(class_prices$class == "Futterweizen"), which(colnames(class_prices) == downgrading_economics_data[i,"year"])])
}

downgrading_economics_data$sim_ref_rev <- downgrading_economics_data$yield_dtPerHa * downgrading_economics_data$price_class 
downgrading_economics_data$sim_real_rev <- downgrading_economics_data$yield_dtPerHa * downgrading_economics_data$price_downgrading
downgrading_economics_data$sim_loss_abs <- abs(downgrading_economics_data$sim_real_rev - downgrading_economics_data$sim_ref_rev)
downgrading_economics_data$sim_loss_rel <- 100*((abs(downgrading_economics_data$sim_real_rev - downgrading_economics_data$sim_ref_rev)) / downgrading_economics_data$sim_ref_rev)

# Expected losses
mean_loss_all <- mean(downgrading_economics_data$sim_loss_abs)
mean_loss_Top <- mean(downgrading_economics_data[which(downgrading_economics_data$class == "TOP"),"sim_loss_abs"])
mean_loss_I <- mean(downgrading_economics_data[which(downgrading_economics_data$class == "I"),"sim_loss_abs"])
mean_loss_II <- mean(downgrading_economics_data[which(downgrading_economics_data$class == "II"),"sim_loss_abs"])
mean_loss_biscuit <- mean(downgrading_economics_data[which(downgrading_economics_data$class == "Biskuitweizen"),"sim_loss_abs"])

# a) Plot simulated profit reductions

sub_downgrading_economics_data <- downgrading_economics_data[which(downgrading_economics_data$sim_loss_abs > 0),c("variety","year","class","sim_loss_abs")]
sub_downgrading_economics_data[sub_downgrading_economics_data == "Biskuitweizen"] <- "Biscuit"
sub_downgrading_economics_data[sub_downgrading_economics_data == "TOP"] <- "Top"

plot_rev_reduction <- ggplot(sub_downgrading_economics_data, aes(x=class,y=sim_loss_abs))+
  xlab("price class")+ylab("simulated revenue reduction [CHF / ha]")+ggtitle("a) profit reductions after downgrading")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))+
  scale_x_discrete(limits = c("Top", "I", "II","Biscuit"))+ 
  geom_boxplot(fill=c("#1f78b4","#a6cee3","#33a02c","#b2df8a"), outlier.size=0.00)


# ----------------------------------------------
# 5.2 Frequency Frequency of downgrading events
# ----------------------------------------------

downgrading_occurrence <- as.data.frame(matrix(NA,nrow=4,ncol=12))
colnames(downgrading_occurrence) <- seq(2008,2019,1)
row.names(downgrading_occurrence) <- c("TOP","I","II","Biskuitweizen") 

for (i in 1:nrow(downgrading_occurrence)){
  temp1 <- downgrading_data[which(downgrading_data$class == row.names(downgrading_occurrence)[i]),]
  
  for (t in 1:ncol(downgrading_occurrence)){
    temp2 <- temp1[which(temp1$year == colnames(downgrading_occurrence)[t]),]
    downgrading_occurrence[i,t] <- nrow(temp2[which(temp2$downgrading == 1),]) / nrow(temp2)
    rm(temp2)
  }
  rm(temp1)
}
rm(i,t)

row.names(downgrading_occurrence)[1] <- "Top"
row.names(downgrading_occurrence)[4] <- "Biscuit"

downgrading_occurrence$class <- row.names(downgrading_occurrence)
downgrading_occurrence_melted <- melt(downgrading_occurrence, id.vars = "class")
colnames(downgrading_occurrence_melted) <- c("price_class","year","value")


plot_downgrading_frequency <- ggplot(downgrading_occurrence_melted, aes(x=year,y=value, fill=factor(price_class, levels = c("Top","I","II","Biscuit"), ordered = T)))+ theme_bw()+
  xlab("year")+ylab("share of downgraded observations")+ ggtitle(" b) frequency of downgrading events")+
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom",
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8))+
  scale_y_continuous(labels=scales::percent)+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual("price class", values = c("Top" = "#1f78b4", "I" = "#a6cee3", "II" = "#33a02c", "Biscuit" = "#b2df8a"))

plot_economics <- ggarrange(plot_rev_reduction, plot_downgrading_frequency, ncol = 2)
ggsave(plot=plot_economics,"plot_economics.png", width = 16, height = 10, unit="cm")

# =================================================================================================
# -------------------------------------------------------------------------------------------------
# 6) Appendix plots
# -------------------------------------------------------------------------------------------------
# =================================================================================================

# ----------------------------------------------
# 6.1 Hagberg Falling Number and yield
# ----------------------------------------------

hist_data <- downgrading_data[,c("class", "timeToFallDry_sec", "yield_dtPerHa")] 
hist_data <- hist_data[which(hist_data$class != "III"),]
colnames(hist_data)[1] <- "price_class"
hist_data[hist_data == "TOP"] <- "Top"
hist_data[hist_data == "Biskuitweizen"] <- "Biscuit"

hist_data$price_class <- factor(hist_data$price_class, levels = c("Top", "I", "II","Biscuit"))



plot_HFN <- ggplot(hist_data, aes(x=timeToFallDry_sec, fill=factor(price_class, levels = c("Top","I","II","Biscuit"), ordered = T)))+
  xlab("Hagberg Falling Number [seconds]")+ylab("number of observations")+ggtitle("a) Hagberg Falling Number")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="bottom",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  scale_fill_manual("price class", values = c("Top" = "#1f78b4", "I" = "#a6cee3", "II" = "#33a02c", "Biscuit" = "#b2df8a"))+
  geom_histogram(bins = 20, alpha = 0.6, position = 'identity', binwidth = 10, colour="black")+
  geom_vline(xintercept = 220, linetype="dashed",size=1.5)

plot_yield <- ggplot(hist_data, aes(x=yield_dtPerHa, fill=factor(price_class, levels = c("Top","I","II","Biscuit"), ordered = T)))+
  xlab("yield [dt / ha]")+ylab("number of observations")+ggtitle("b) Crop yield quantity")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="bottom",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  ylim(0,60)+
  scale_fill_manual("price class", values = c("Top" = "#1f78b4", "I" = "#a6cee3", "II" = "#33a02c", "Biscuit" = "#b2df8a"))+
  geom_histogram(bins = 2, alpha = 0.6, position = 'identity', binwidth = 2, colour="black")

plot_app_agronomic <- ggarrange(plot_HFN, plot_yield, ncol=2)
ggsave(plot=plot_app_agronomic,"plot_app_agronomic.png", width = 16, height = 10, unit="cm")

# ----------------------------------------------
# 6.2 Planting and harvest date
# ----------------------------------------------

data_sowing_melt <- melt(data_sowing, id.vars = "site")
doy_planting <- as.numeric(strftime(data_sowing_melt$value, format = "%j"))

data_harvest_melt <- melt(data_harvest, id.vars = "site")
doy_harvest <- as.numeric(strftime(data_harvest_melt$value, format = "%j"))

plot_planting <- ggplot()+ aes(doy_planting ) +
  xlab("day of year")+ylab("number of observations")+ggtitle("a) Planting date")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="bottom",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  ylim(0,15)+
  geom_histogram(binwidth=1, colour="black", fill="sienna")

plot_harvest <- ggplot()+ aes(doy_harvest) +
  xlab("day of year")+ylab("number of observations")+ggtitle("b) Harvest date")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="bottom",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  ylim(0,15) +
  geom_histogram(binwidth=1, colour="black", fill="#FFCC00")

plot_app_dates <- ggarrange(plot_planting, plot_harvest, ncol=1)
ggsave(plot=plot_app_dates,"plot_app_dates.png", width = 16, height = 10, unit="cm")

# ----------------------------------------------
# 6.3 Price data
# ----------------------------------------------

class_prices <- read.csv("Meta/class_prices.csv", sep=";")[-5,]
colnames(class_prices) <- c("price_class", seq(2008,2021,1))

class_prices[1,1] <- "Feed"
class_prices[2,1] <- "Top"
class_prices[5,1] <- "Biscuit"

class_prices[,1] <- as.factor(class_prices[,1])
class_prices[,2] <- as.numeric(class_prices[,2])

class_prices_melted <- melt(class_prices, id.vars = "price_class")
class_prices_melted <- class_prices_melted[complete.cases(class_prices_melted),]
class_prices_melted[,1] <- factor(class_prices_melted[,1], levels = c("Top","I","II","Biscuit","Feed")) 

plot_prices <- ggplot(class_prices_melted, aes(x=variable, y=value, group=price_class, colour = price_class))+
  xlab("year")+ylab("price [CHF / dt]")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="right",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  ylim(20,60)+
  scale_color_manual("price class", values = c("Top" = "#1f78b4", "I" = "#a6cee3", "II" = "#33a02c", "Biscuit" = "#b2df8a", "Feed" = "#b1876b"))+
  geom_line() + geom_point()
ggsave(plot=plot_prices,"plot_ref_prices.png", width = 16, height = 8, unit="cm")

# ----------------------------------------------
# 6.4 Simulated yearly precipitation effect
# ----------------------------------------------

precip_P1 <- as.data.frame(array_cumulative_precipitation_main[31,,])
precip_P1$site <- as.factor(row.names(precip_P1))
precip_P1_melted <- melt(precip_P1, id.vars = "site")
summary(precip_P1_melted$value)


precip_P2 <- as.data.frame(array_cumulative_precipitation_control[31,,])
precip_P2$site <- as.factor(row.names(precip_P2))
precip_P2_melted <- melt(precip_P2, id.vars = "site")
summary(precip_P2_melted$value)
sd(precip_P2_melted$value, na.rm=T)

GDD_P1 <- as.data.frame(list_GDD_array_cumulative_GDD_HDD_main [[3]][31,,])
GDD_P1$site <- as.factor(row.names(GDD_P1))
GDD_P1_melted <- melt(GDD_P1, id.vars = "site")
summary(GDD_P1_melted$value)
sd(GDD_P1_melted$value, na.rm=T)

GDD_P2 <- as.data.frame(list_GDD_array_cumulative_GDD_HDD_control [[3]][31,,])
GDD_P2$site <- as.factor(row.names(GDD_P2))
GDD_P2_melted <- melt(GDD_P2, id.vars = "site")
summary(GDD_P2_melted$value)
sd(GDD_P2_melted$value, na.rm=T)

HDD_P1 <- as.data.frame(list_HDD_array_cumulative_GDD_HDD_main [[3]][31,,])
HDD_P1$site <- as.factor(row.names(HDD_P1))
HDD_P1_melted <- melt(HDD_P1, id.vars = "site")
summary(HDD_P1_melted$value)
sd(HDD_P1_melted$value, na.rm=T)

HDD_P2 <- as.data.frame(list_HDD_array_cumulative_GDD_HDD_control [[3]][31,,])
HDD_P2$site <- as.factor(row.names(HDD_P2))
HDD_P2_melted <- melt(HDD_P2, id.vars = "site")
summary(HDD_P2_melted$value)
sd(HDD_P2_melted$value, na.rm=T)


precipitation_effect <- core_model$coefficients[1] * 100
precip_31_melted$abs_effect <- precip_31_melted[,3] * precipitation_effect

plot_precip_risk <- ggplot(precip_31_melted, aes(x=variable, y=abs_effect)) +
  xlab("")+ylab("increased risk of a downgrading\n\n[percentage points]")+ theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 9),
        legend.position="bottom",
        legend.title = element_text(size=8), 
        legend.text = element_text(size=8),
        axis.title.y = element_text(size = 9))+
  
  geom_bar(stat="identity", fill = "red") + facet_wrap(~ site, ncol = 2)
