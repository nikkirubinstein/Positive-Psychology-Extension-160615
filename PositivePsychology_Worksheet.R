##################################################################
# PositivePsychology_Worksheet.R
# Reads in data, combines into single data.frame and does 
# some exploratory analysis and modelling (e.g. central tendancy,
# normality, histograms, t-tests, anovas, CFA and SEM)
# 
# Created by Nikki Rubinstein and Tim Esler
# Fri Jun  3 04:42:52 UTC 2016
##################################################################




### BEFORE WE GET STARTED ########################################

# We first need to install any necessary packages that we'll be
# using:
#
#     install.packages("lavaan")
#     system(sudo apt-get install r-cran-xml)
#
# Now restart your session of R (we only need to do this because
# we are running R on Linux), then type:
#
#     install.packages("semPlot")




### Load required packages #######################################

library(plyr)
library(lavaan)
library(semPlot)
library(ggplot2)




### Read data files into R #######################################

# For this workshop we are using longitudinal data provided by Dan
# Loton taken over time from 9 time points (waves). The raw data
# is stored in 9 CSV files.
#
# We will:
#   - Load each dataset into R
#   - Run an preliminary exploration of the data to gather info
#     about missing values, dimensionality, variations in column
#     names across datsets

# Initialise a list to hold data from each wave
dataList <- list()

# We can read the first wave of data into the first element of
# our list in the following way:
wave <- 1
fileName <- paste0("Data/addictionDataWave",wave,".csv")
dataList[[wave]] <- read.csv(fileName)

# Now let's write a "for" loop to automate the import of all 9
# waves. Remember the syntax for a for loop in R is:
#
#   for (currentNum in vectorOfNumbers) {
#     do something (e.g. read data into a list!)
#   }
#

# 1. WRITE A DATA IMPORT LOOP HERE:
for (wave in 1:9) {
  
}

# Some exploratory data analysis
#
# Now let's use the 'dim()' function to check the dimensionality
# of each wave. Again, we could write code for each wave, or run
# it inside a loop.
print("Dimensions of data for each wave...")

# 2. ADD CODE TO CHECK DATA DIMENSIONS HERE:




# We can then list the columns in a few datasets using the
# 'names()' function. Alternatively, we can use the 'summary()'
# function to get simple summary statistics of each column.

# 3. USE THESE FUNCTIONS TO HAVE A FIRST LOOK AT A FEW OF THE WAVES:




### Calculate various statistics (normality, histograms, mean, min, quantiles)

# Shapiro-Wilk normality test
shapiro.test(dataList[[1]]$videogameaddiction)

# 4. CHECK THE HELP FOR R'S HISTOGRAM PLOTTING FUNCTION 'hist()'.
# USE IT TO PLOT A HISTOGRAM OF videogameaddiction IN WAVE 1:



# We can also very easily calculate means, medians, min, max etc
# Note the use of the standard na.rm argument.

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

# 5. USE THE HELP FILE FOR THE FUNCTION 'quantile()' TO CALCULATE
# DECILES FOR THE videogameaddiction COLUMN IN WAVE 2:




# Identifying outlier indices (univariate)
outliers <- which((dataList[[1]]$videogameaddiction > (meanVGA+2*sdVGA)) |
                    (dataList[[1]]$videogameaddiction < (meanVGA-2*sdVGA)))




### Combine data in 'long' format ##############################

# Combine data in long format - rbind.fill is so awesome (it's
# all we need). rbind.fill() is part of the plyr package and
# uses the column names in each data.frame to match columns to
# each other (note that this requires consistent variable
# names across waves!).

# 6. USE 'rbind.fill()' TO COMBINE ALL 9 WAVES IN THE DATA LIST
# INTO A SINGLE data.frame NAMED dataLong:



# Does the data have the desired dimension? 

# 7. (DIFFICULT) CHECK THE DIMENSION OF THE NEW DATA FRAME dataLong.
# IS IT AS EXPECTED?
# IF NOT, LET'S FIND THE PROBLEMATIC COLUMN (HINT: USE THE %in%
# COMMAND TO COMPARE COLUMN NAMES)
names(dataList[[2]]) <- names(dataList[[3]])
dataLong <- rbind.fill(dataList)
dim(dataLong)




### Combine data in 'wide' format ##############################

# This is a little more tricky since we need to ensure that id's
# match across the data.

# Intialise a new dataset with the id's from the first wave (all
# the ids)
dataWide <- data.frame(id = dataList[[1]]$id)

# Iterate through each wave
for (wave in 1:9) {
  
  # Get temporary dataset for this wave (excluding the column "wave")
  currentWaveData <- dataList[[wave]][,names(dataList[[wave]])!="Wave"]
  
  # Add a "_#" suffix to the column names so we can tell the waves apart
  names(currentWaveData)[names(currentWaveData)!="id"] <- 
    paste0(names(currentWaveData)[names(currentWaveData)!="id"],"_",wave)
  
  # Join the columns (variables) from the current wave onto the
  # wide data, matching by id
  dataWide <- join(dataWide,currentWaveData,by="id",match="first")
}
# Check dimension, is it what we expect?
dim(dataWide)




#### Exploratory data visualisation ############################

# As an example, we will plot the video game addiction across
# the 9 waves. For this example we want a separate line for each
# participant plotted against wave number. Plotting tasks are
# usually better suited to long- or intermediate-formatted data.

# Get the data for those participants which remained until the last wave
IDsForPersistentPeople <- na.omit(dataWide[,c("id","videogameaddiction_9")])[,1]
dataPersistent <- dataLong[dataLong$id %in%  IDsForPersistentPeople,]

# 8. USE GGPLOT TO PLOT VIDEO GAME ADDICTION ACROSS THE 9 WAVES
# USING THE DATA SUBSET CREATED ABOVE. PLOT A SEPARATE LINE FOR
# EACH PARTICIPANT





### T-tests, ANOVA, and regression  ###################################################

# Calculate a t-statistic (Q118 = Had a romantic r/ship break up in past month)
t.test(videogameaddiction ~ Q25,
       data = dataLong)

# Run an ANOVA (Q110_1 relates to the level of social support, an 8(?) factor scale)

# 9. USING THE LONG DATA, LOOK AT THE HELP FOR 'aov()' AND
# RUN AN ANOVA FOR videogameaddiction
# AGAINST GROUPS IN column Q110_1 (NOTE THAT Q110_1 MUST FIRST
# BE CONVERTED TO A FACTOR USING 'as.factor()'). 
#
# SEE WHAT HAPPENS WHEN YOU PASS THE 'aov()' OUTPUT TO THE
# 'summary()' AND 'plot()' FUNCTIONS.


# MANOVA
fit <- manova(cbind(videogameaddiction,stress) ~ as.factor(Q110_1)*as.factor(Q110_2)*as.factor(Q110_3),
              data = dataLong)
summary(fit)

# Regression - simple linear model
fitLM <- lm(videogameaddiction ~ Q118 + as.factor(Q110_1)*as.factor(Q110_2)*as.factor(Q110_3) + Q111_1*Q111_10,
            data = dataLong)
summary(fitLM)
plot(fitLM)

# 10. BUILD A SIMPLE BINOMIAL MODEL (GLM) USING THE 'glm()' FUNCTION.



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

# 11. CREATE A MEDIATION SETUP WITH THREE VARIABLES.
# E.G. WE COULD CREATE LATENT VARIABLES FOR videogameaddiction,
# depression, AND sex, THEN MODEL THE DIRECT EFFECT OF depression
# ON videogameaddiction, AND THE INDIRECT EFFECT OF depression ON
# videogameaddiction VIA ITS INFLUENCE ON stress.





### THE END - you now know everything I do! #########################



