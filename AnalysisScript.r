library(dplyr)

# Read in data

addictionData <- read.csv("Data/addictionData.csv",
                         stringsAsFactors = F,
                         na.strings = c("NA"," ","."))

waves <- c("_T1","_T2","_T3","_T4","_T5","_T6","_T7","_T8","_T9")

addictionDataSplit <- list()

for (i in 2:9) {
  addictionDataSplit[[i]] <- addictionData[,grepl(waves[i],names(addictionData))]
  addictionData <- addictionData[,-grep(waves[i],names(addictionData))]
}
addictionDataSplit[[1]] <- addictionData

for (i in 1:9) {
  write.csv(addictionDataSplit[[i]],
            file = paste0("Data/addictionDataWave",i,".csv"))
}