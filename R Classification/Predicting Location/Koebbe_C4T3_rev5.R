# Title: Techniques for Wifi Locationing

# Last update: 24 Jan 2022

# File: Koebbe_C4T3.R

# Project name: C:\Users\klkoe\Documents\School\UT DA 2021\C4T3


#################
# Project Notes #
#################

# Summary:   

# 1) Investigate the feasibility of using "wifi fingerprinting" to determine a person's location in indoor spaces.
# 2) Evaluate multiple machine learning models to see which produces the best result, enabling us to make a recommendation to the client.

###############
# Housekeeping
###############

# Clear console: CTRL + L

rm(list = ls())
getwd()
setwd("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C4T3")
dir()

#################
# Load packages #
#################

install.packages("caret")
install.packages ("tidyr")
install.packages("doParallel")

library(caret)
library (tidyr)
library(skimr)
library(doParallel)
library(dplyr)

#######################
# Parallel processing #
#######################

detectCores()          
cl <- makeCluster(2)   
registerDoParallel(cl) 
getDoParWorkers()      

###############
# Import data #
###############

## Load existing dataset ##

trndf <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
valdf <- read.csv("validationData.csv", stringsAsFactors = FALSE)


#################
# Evaluate data #
#################

str(trndf)
str(valdf)

# Train

head(trndf)
tail(trndf)

anyNA(trndf) #No#
anyDuplicated((trndf)) #2908#
trndf1 <- unique(trndf) # extract unique values / remove duplicates
anyDuplicated(trndf1) #0#

skimtrn1 <- skim(trndf1, c(521:529))
skimtrn1

counts <- trndf1 %>% count(BUILDINGID, sort=TRUE)
counts

countsF <- trndf1 %>% count(BUILDINGID, FLOOR, sort=TRUE)
countsF

#################
# Preprocessing #
#################

## Training Set ##

# Remove unnecessary attributes #

trndf2 <- trndf1[,-c(521:522, 526:529, drop=FALSE)]
head(trndf2)


# Create unique attribute for floor and space ID

trndf3 <- unite(trndf2, col = "LocID", c("FLOOR", "SPACEID"), sep = "", remove = FALSE)
trndf3$LocID <- factor(trndf3$LocID)
str(trndf3$LocID)


# Create new ds for building 0

bld0 <- subset(trndf3, BUILDINGID == 0)
str(bld0)
head(bld0)

counts1 <- bld0 %>% count(FLOOR, sort=TRUE)
counts1


#################
# Save datasets #
#################

write.csv(trndf3, "training", row.names = F)
write.csv(bld0, "Building0", row.names = F)


######################
# EDA/Visualizations #
######################
?axis

hist(trndf2$BUILDINGID, main="Frequency of Observations by Building", xlab="Building ID", ylab="Observations", col="blue")
hist(trndf2$FLOOR)
hist(trndf2$SPACEID)
hist(trndf2$LATITUDE)
hist(trndf2$LONGITUDE)

plot(trndf2$BUILDINGID, trndf2$FLOOR)
plot(trndf2$BUILDINGID, trndf2$SPACEID)
plot(trndf1$BUILDINGID, trndf1$LONGITUDE)
plot(trndf1$LATITUDE, trndf1$FLOOR)
plot(trndf1$LONGITUDE, trndf1$FLOOR)
plot(trndf1$LATITUDE, trndf1$LONGITUDE, main="Latitude/Longitude Observations for All Buildings", xlab="Latitude Position", ylab="Longitude Position", col="magenta")
plot(trndf1$LONGITUDE, trndf1$LATITUDE, main="Latitude/Longitude Observations for All Buildings", xlab="Longitude Position", ylab="Latitude Position", col="blue")
plot(trndf1$LATITUDE, trndf1$SPACEID)
plot(trndf1$LONGITUDE, trndf1$SPACEID)

hist(bld0$FLOOR, main="Frequency of Observations in Building 0 by Floor", xlab="Floor", ylab="Observations", col="red")

hist(bld0$SPACEID) 

plot(bld0$BUILDINGID, bld0$FLOOR)
plot(bld0$BUILDINGID, bld0$SPACEID)
plot(bld0$FLOOR, bld0$SPACEID)


###################
# Train/test sets #
###################

# Ensure dropped levels and remove remaining location variables

bld0$LocID <- factor(bld0$LocID)
str(bld0$LocID)
str(bld0[,520:524])

bld0df <- bld0[,-c(522:524, drop=FALSE)]
head(bld0df)

# Building 0 #

set.seed(123) 
inTraining0 <- createDataPartition(bld0$LocID, p=0.7, list=FALSE)
Train0 <- bld0df[inTraining0,]   
Test0 <- bld0df[-inTraining0,]   
nrow(Train0) 
nrow(Test0)  

#################
# Train control #
#################

fitControl <- trainControl(method="repeatedcv", number=5, repeats=1) 

################
# Train models #
################


##  RF ##

set.seed(123)
RF0fit <- train(LocID~., data=Train0, method="rf", importance=T, trControl=fitControl)
RF0fit
plot(RF0fit)

# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 2978, 2978, 2983, 2978, 2991 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy     Kappa    
# 2   0.007782724  0.0000000
# 32   0.719431183  0.7182556
# 520   0.718031167  0.7168613
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 32.


## C5.0 ##

set.seed(123)
C50fit <- train(LocID~., data=Train0, method="C5.0", importance=T, trControl=fitControl)
C50fit
plot(C50fit)

# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 2978, 2978, 2983, 2978, 2991 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.5314937  0.5295562
# rules  FALSE   10      0.6431589  0.6416880
# rules  FALSE   20      0.6630129  0.6616233
# rules   TRUE    1      0.5417261  0.5398306
# rules   TRUE   10      0.6445237  0.6430566
# rules   TRUE   20      0.6670973  0.6657256
# tree   FALSE    1      0.5435519  0.5416727
# tree   FALSE   10      0.6441807  0.6427058
# tree   FALSE   20      0.6605688  0.6591628
# tree    TRUE    1      0.5503057  0.5484463
# tree    TRUE   10      0.6388728  0.6373720
# tree    TRUE   20      0.6600540  0.6586435
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = rules and winnow = TRUE.

## Naive Bayes ##

set.seed(123)
NB0fit <- train(LocID~., data=Train0, method="naive_bayes", trControl=fitControl)
NB0fit
plot(NB0fit)

# Naive Bayes 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 2978, 2978, 2983, 2978, 2991 
# Resampling results across tuning parameters:
#   
#   usekernel  Accuracy   Kappa    
# FALSE      0.1100229  0.1063134
# TRUE      0.1701336  0.1652163
# 
# Tuning parameter 'laplace' was held constant at a value of 0
# Tuning parameter 'adjust' was held constant at a
# value of 1
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were laplace = 0, usekernel = TRUE and adjust = 1.

## KNN ##

set.seed(123)
knn0fit <- train(LocID~., data=Train0, method="kknn", trControl=fitControl)
knn0fit
plot(knn0fit)

# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 2978, 2978, 2983, 2978, 2991 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.5476298  0.5457635
# 7     0.5465545  0.5446821
# 9     0.5465545  0.5446821
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning parameter 'kernel' was held constant at
# a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 5, distance = 2 and kernel = optimal.


#######################
# Variable Importance #
#######################

varImp(RF0fit)

varImp(C50fit)
varImp(NB0fit)
varImp(knn0fit)


###################
# Model selection #
###################


bestalgo0 <- resamples(list(RF=RF0fit, C5=C50fit, NB=NB0fit, KNN=knn0fit))
summary(bestalgo0)


# Call:
#   summary.resamples(object = bestalgo0)
# 
# Models: RF, C5, NB, KNN 
# Number of resamples: 5 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# RF  0.6969292 0.7110215 0.7129506 0.7194312 0.7289720 0.7472826    0
# C5  0.6528705 0.6595461 0.6612903 0.6670973 0.6715621 0.6902174    0
# NB  0.1602136 0.1655541 0.1708945 0.1701336 0.1725543 0.1814516    0
# KNN 0.5327103 0.5353805 0.5475543 0.5476298 0.5510753 0.5714286    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# RF  0.6956603 0.7097973 0.7117627 0.7182556 0.7278426 0.7462149    0
# C5  0.6514365 0.6581494 0.6598820 0.6657256 0.6702301 0.6889299    0
# NB  0.1553687 0.1607008 0.1658435 0.1652163 0.1676172 0.1765514    0
# KNN 0.5307748 0.5334837 0.5456883 0.5457635 0.5492112 0.5696597    0


### Save/load top performing model ###

saveRDS(RF0fit, "RF0fit.rds")  

####################
# Model validation #
####################

RFPred0 <- predict(RF0fit, Test0)
postResample(RFPred0, Test0$LocID)
plot(RFPred0)

# Accuracy     Kappa 
# 0.7636603 0.7626738

####################
# Predict new data #
####################

RFPredbld0 <- predict(RF0fit, Test0)
head(RFPredbld0)
plot(RFPredbld0)
summary (RFPredbld0)


stopCluster(cl)
