library(plyr)
library(ggplot2)

# Read in data

addictionData <- read.csv("Data/addictionData.csv",
                          stringsAsFactors = F,
                          na.strings = c("NA"," ","."))

waves <- c("_T1","_T2","_T3","_T4","_T5","_T6","_T7","_T8","_T9")

addictionDataSplit <- list()

for (i in 2:9) {
  addictionDataSplit[[i]] <- addictionData[,grepl(waves[i],names(addictionData))]
  addictionData <- addictionData[,-grep(waves[i],names(addictionData))]
  names(addictionDataSplit[[i]]) <- gsub(waves[i],"",names(addictionDataSplit[[i]]))
  addictionDataSplit[[i]]$Wave <- i
  addictionDataSplit[[i]]$id <- addictionData$id
}
addictionDataSplit[[1]] <- addictionData
addictionDataSplit[[1]]$Wave <- 1
names(addictionDataSplit[[1]]) <- gsub(waves[1],"",names(addictionDataSplit[[1]]))

for (i in 1:9) {
  write.csv(addictionDataSplit[[i]],
            file = paste0("Data/addictionDataWave",i,".csv"),
            row.names = F)
}

addictionData_Long <- addictionDataSplit[[1]]

for (i in 2:9) {
  addictionData_Long <- rbind.fill(addictionData_Long,addictionDataSplit[[i]])
}

persistentIds = addictionData_Long$id[addictionData_Long$Wave==9 &
                                        !is.na(addictionData_Long$videogameaddiction)]

ggplot(addictionData_Long[addictionData_Long$id %in% persistentIds,],aes(x = Wave,y = videogameaddiction))+
  geom_line(aes(by=as.factor(id)))
