########################################################################################
# BasicAnalysis.R
# Reads in data, combines into single data.frame and does some exploratory analysis
# 
# Created by Nikki Rubinstein
# Fri Jun  3 04:42:52 UTC 2016
########################################################################################

library(plyr)

# Initialise a list to hold data from each wave
data <- list()

# Read in data from each wave
for (wave in 1:9) {
  data[[wave]] <- read.csv(paste0("Data/addictionDataWave",wave,".csv"))
}

# Some exploratory data analysis
print("Dimensions of data for each wave...")
for (wave in 1:9) {
  print(paste0("Wave ",wave,": ",paste(dim(data[[wave]]),collapse = ", ")))
}
summary(data[[1]]) # First wave summary
shapiro.test(data[[1]]$videogameaddiction) # Shapiro-Wilk normality test
meanVGA <- mean(data[[1]]$videogameaddiction,
                na.rm = T)
medianVGA <- median(data[[1]]$videogameaddiction,
                  na.rm = T)
sdVGA <- sd(data[[1]]$videogameaddiction,
            na.rm = T)
minVGA <- min(data[[1]]$videogameaddiction,
    na.rm = T)
maxVGA <- max(data[[1]]$videogameaddiction,
    na.rm = T)
quantile(data[[1]]$videogameaddiction,
         seq(0,1,by=0.1),
         na.rm = T)

# Identifying outlier indices
outliers <- which((data[[1]]$videogameaddiction > (meanVGA+2*sdVGA)) |
  (data[[1]]$videogameaddiction < (meanVGA-2*sdVGA)))

for(f in 1:9){
  dataLong <- rbind.fill(data, read.csv(paste0("Data/addictionDataWave",f,".csv")))
}