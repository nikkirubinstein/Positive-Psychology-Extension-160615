########################################################################################
# BasicAnalysis.R
# Reads in data, combines into single data.frame and does some exploratory analysis and
# modelling (e.g. central tendancy, normality, histograms, t-tests, anovas, CFA and SEM)
# 
# Created by Nikki Rubinstein and Tim Esler
# Fri Jun  3 04:42:52 UTC 2016
########################################################################################




### BEFORE WE GET STARTED ##############################################################

# We first need to install any necessary packages that we'll be using:
#
#     install.packages("lavaan")
#     system(sudo apt-get install r-cran-xml)
#
# Now restart your session of R (we only need to do this because we running R on Linux), then type:
#
#     install.packages("semPlot")




### Load required packages #############################################################

library(plyr)
library(lavaan)
library(semPlot)
library(ggplot2)




### Read data files into R #############################################################

# For this workshop we are using longitudinal data provided by Dan Loton
# taken over time from 9 time points (waves). The raw data is stored in 9 CSV files.
#
# We will:
#     - Load each dataset into R
#     - Run an preliminary exploration of the data to gather info about 
#       missing values, dimensionality, variations in column names across datsets

# Initialise a list to hold data from each wave
dataList <- list()

# Read in data from each wave
for (wave in 1:9) {
  dataList[[wave]] <- read.csv(paste0("Data/addictionDataWave",wave,".csv"))
}

# Some exploratory data analysis
print("Dimensions of data for each wave...")
for (wave in 1:9) {
  print(paste0("Wave ",wave,": ",paste(dim(dataList[[wave]]),collapse = ", ")))
}

# View column names from each dataset
print(names(dataList[[1]]))
print(names(dataList[[2]]))
print(names(dataList[[9]]))

# The summary() function will calculate useful summary stats for each column in a data.frame
summary(dataList[[1]]) # First wave summary




### Calculate various statistics (normality, histograms, mean, min, quantiles) #########

# Shapiro-Wilk normality test
shapiro.test(dataList[[1]]$videogameaddiction)

# Histgram
hist(dataList[[1]]$videogameaddiction,20)

# Mean
meanVGA <- mean(dataList[[1]]$videogameaddiction,
                na.rm = T)

# Median
medianVGA <- median(dataList[[1]]$videogameaddiction,
                  na.rm = T)

# Standard deviation
sdVGA <- sd(dataList[[1]]$videogameaddiction,
            na.rm = T)

# Minimum
minVGA <- min(dataList[[1]]$videogameaddiction,
    na.rm = T)

# Maximum
maxVGA <- max(dataList[[1]]$videogameaddiction,
    na.rm = T)

# Deciles
quantile(dataList[[1]]$videogameaddiction,
         seq(0,1,by=0.1),
         na.rm = T)

# Identifying outlier indices (univariate)
outliers <- which((dataList[[1]]$videogameaddiction > (meanVGA+2*sdVGA)) |
  (dataList[[1]]$videogameaddiction < (meanVGA-2*sdVGA)))




### Combine data in 'long' format #####################################################

# Combine data in long format - rbind.fill is so awesome (it's all we need)
dataLong <- rbind.fill(dataList)
dim(dataLong)

# Does the data have the desired dimension? If not, lets fix it here and try stitching again.
names(dataList[[2]]) <- names(dataList[[3]])
dataLong <- rbind.fill(dataList)
dim(dataLong)




### Combine data in 'long' format #####################################################

# This is a little more tricky since we need to ensure that id's match across the data.

# Intialise a new dataset with the id's from the first wave (all the ids)
dataWide <- data.frame(id = dataList[[1]]$id)

# Iterate through each wave
for (wave in 1:9) {
  
  # Get temporary dataset for this wave
  currentWaveData <- dataList[[wave]][,names(dataList[[wave]])!="Wave"]
  
  # Add a "_#" suffix to the column names so we can tell the waves apart
  names(currentWaveData)[names(currentWaveData)!="id"] <- 
    paste0(names(currentWaveData)[names(currentWaveData)!="id"],"_",wave)
  
  # Join the columns (variables) from the current wave onto the wide data, matching by id
  dataWide <- join(dataWide,currentWaveData,by="id",match="first")
}
# Check dimension, is it what we expect?
dim(dataWide)




#### Exploratory data visualisation ###################################################

# As an example, we will plot the video game addiction across the 9 waves. For this
# example we want a separate line for each participant plotted against wave number.
# Plotting tasks are usually better suited to long- or intermediate-formatted data.

# Get the data for those participants which remained until the last wave
persistentPeopleIDs <- na.omit(dataWide[,c("id","videogameaddiction_9")])[,1]
dataPersistent <- dataLong[dataLong$id %in%  persistentPeopleIDs,]


F1 <- ggplot(data = dataPersistent,
             aes(x = Wave, y = videogameaddiction)) +
  geom_line(aes(by = as.factor(id), col = as.factor(id))) + guides(colour = "none") + geom_smooth()

print(F1)




### T-tests, ANOVA, and regression  ###################################################

# Calculate a t-statistic (Q118 = Had a romantic r/ship break up in past month)
t.test(videogameaddiction ~ Q25,
       data = dataLong)

# Run an ANOVA (Q110_1 relates to the level of social support, an 8(?) factor scale)
fit <- aov(videogameaddiction ~ as.factor(Q110_1),
           data = dataLong)
summary(fit)
plot(fit)

# MANOVA
fit <- manova(cbind(videogameaddiction,stress) ~ as.factor(Q110_1)*as.factor(Q110_2)*as.factor(Q110_3),
              data = dataLong)
summary(fit)

# Regression - simple linear model (using all columns!)
fitLM <- lm(videogameaddiction ~ Q118 + as.factor(Q110_1)*as.factor(Q110_2)*as.factor(Q110_3) + Q111_1*Q111_10,
            data = dataLong)
summary(fitLM)
plot(fitLM)

# Binomial/logistic GLM (also see glmnet for L1 and L2 regularisation)
fitGLM <- glm((Q118-1) ~ stress + as.factor(Q110_1) + Q111_1*Q111_10,
              data = dataLong,
              family = binomial)
summary(fitGLM)
plot(fitGLM)

# Some more intense popular modelling packages available:
#   - xgboost: the most accurate, robust and generalisable tree-based ensembling algorithm ever!
#   - randomForest
#   - glmnet: ridge regression and LASSO
#   - nnet: neural networks





### Confirmatory factor analysis, SEM and mediation ##################################

# Define SEM model structure
vgaModel <- '# Define latent variables
             VGALatent_1 =~ Q77_1_1 + Q77_2_1 + Q77_3_1 + Q77_4_1 + Q77_4_1 + Q77_5_1 + Q77_6_1 + Q77_7_1 + Q77_8_1 + Q77_9_1 + Q77_10_1 + Q77_11_1 + Q77_12_1 + Q77_13_1
             VGALatent_2 =~ Q77_1_2 + Q77_2_2 + Q77_3_2 + Q77_4_2 + Q77_4_2 + Q77_5_2 + Q77_6_2 + Q77_7_2 + Q77_8_2 + Q77_9_2 + Q77_10_2 + Q77_11_2 + Q77_12_2 + Q77_13_2
             
             # Model is predicting VGALatent_2 from VGALatent_1
             VGALatent_2 ~ VGALatent_1'

# Build SEM
vgaMdlFit <- sem(model = vgaModel,
                 data = dataWide)
summary(vgaMdlFit, fit.measures = TRUE)

# Standardise solution of latent variable model
standardizedSolution(vgaMdlFit)

# Visualise model paths
semPaths(vgaMdlFit,
         what = "est",
         layout = "tree",
         title = TRUE,
         style = "LISREL") 

# Create a mediation setup with 3 variables
vgaMediationModel <- '# Define latent variables
             VGALatent_1 =~ Q77_1_1 + Q77_2_1 + Q77_3_1 + Q77_4_1 + Q77_4_1 + Q77_5_1 + Q77_6_1 + Q77_7_1 + Q77_8_1 + Q77_9_1 + Q77_10_1 + Q77_11_1 + Q77_12_1 + Q77_13_1
             DepressionLatent_1 =~ Q111_3_1 + Q111_5_1 + Q111_10_1 + Q111_13_1 + Q111_16_1 + Q111_17_1 + Q111_21_1
             StressLatent_1 =~  Q111_1_1 + Q111_6_1 + Q111_8_1 + Q111_11_1 + Q111_12_1 + Q111_14_1 + Q111_18_1
            
             # Define direct effect
             VGALatent_1 ~ c*StressLatent_1

             # Define mediation
             DepressionLatent_1 ~ a*StressLatent_1
             VGALatent_1 ~ b*DepressionLatent_1

             # Define indirect effect
             ab := a*b

             # Define total effect
             total := c + (a*b)'

fitMediation <- sem(vgaMediationModel,
                    data = dataWide)
summary(fitMediation)
semPaths(fitMediation,
         what = "est",
         layout = "tree",
         title = TRUE,
         style = "LISREL") 



