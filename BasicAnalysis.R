########################################################################################
# BasicAnalysis.R
# Reads in data, combines into single data.frame and does some exploratory analysis
# 
# Created by Nikki Rubinstein
# Fri Jun  3 04:42:52 UTC 2016
########################################################################################

library(plyr)
library(dplyr)

addictionData <- NULL

file <- "addictionDataWave"
for(f in 1:9){
  addictionData <- rbind.fill(addictionData, read.csv(paste0("Data/addictionDataWave",f,".csv")))
}