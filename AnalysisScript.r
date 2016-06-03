library(dplyr)

# Read in data

addictionData <- read.csv("Data/addictionData_new.csv",
                         stringsAsFactors = F,
                         na.strings = c("NA"," ","."))

waves <- paste0("_T",1:9)

addictionDataSplit <- list()

for (i in 2:9) {
  addictionDataSplit[[i]] <- addictionData[,grepl(waves[i],names(addictionData))]
  addictionDataSplit[[i]]$id <- addictionData[,'id']
  addictionDataSplit[[i]]$Wave <- i
  addictionData <- addictionData[,-grep(waves[i],names(addictionData))]
}
addictionDataSplit[[1]] <- addictionData
addictionDataSplit[[1]]$Wave <- 1

for (i in 1:9) {
  write.csv(addictionDataSplit[[i]],
            file = paste0("Data/addictionDataWave",i,".csv"),quote = F,row.names = F)
}

