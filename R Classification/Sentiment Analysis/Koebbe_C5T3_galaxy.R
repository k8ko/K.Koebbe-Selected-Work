# Title: Predictive Modeling with Big Data

# Last update: 30 January 2022

# File: Koebbe_C5T3_galaxy.R

# Project name: UTDA 2021 / C5T3


#################
# Project Notes #
#################

# Summary:   

# Build models that understand the patterns in the two small matrices and 
# then use those models with the Large Matrix to predict sentiment for iPhone and Galaxy.


###############
# Housekeeping
###############

# Clear console: CTRL + L

rm(list = ls())
getwd()
setwd("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C5T3")
dir()

#################
# Load packages #
#################

install.packages("caret")
install.packages("corrplot")
install.packages("doParallel") 
install.packages("e1071")
install.packages("kknn")
library(caret)
library(corrplot)
library(doParallel)      
library(plotly)
library(e1071)
library(kknn)



#######################
# Parallel processing #
#######################

detectCores()          
cl <- makeCluster(12)   
registerDoParallel(cl) 
getDoParWorkers()      

###############
# Import data #
###############

## Load existing dataset ##

large <- read.csv("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C5T3\\Koebbe_LargeMatrix_galaxy.csv", stringsAsFactors = FALSE)
galaxy <- read.csv("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C5T3\\galaxy_smallmatrix_labeled_9d.csv", stringsAsFactors = FALSE)


# Load preprocessed dataset(s) #

read.csv("galaxyDS.csv", stringsAsFactors = FALSE) 

#################
# Evaluate data #
#################

## galaxy ##

str(galaxy)  
#data.frame':	12911 obs. of  59 variables:
summary(galaxy)
head(galaxy)
tail(galaxy)

## Large ##

str(galaxy) 
summary(galaxy)
head(galaxy)
tail(galaxy)

## Header Reference ##

# iOS - counts mentions of iOS on a webpage
# galaxycampos - counts positive sentiment mentions of the galaxy camera
# galaxydisneg - counts negative sentiment mentions of the Galaxy display
# htcperunc - counts the unclear sentiment mentions of HTC performance


#################
# Preprocessing #
#################

## galaxy ##

anyNA(galaxy)
anyDuplicated((galaxy))
galaxy1 <- unique(galaxy) # extract unique values / remove duplicates
anyDuplicated(galaxy1) #0#

## large ##

anyNA(large)
anyDuplicated((large))


#################
# Save datasets #
#################

write.csv(galaxy1, "galaxyDS", row.names = F)


##################
# Visualizations #
##################

plot_ly(galaxy1, x= galaxy1$galaxysentiment, type='histogram') %>% layout (title = "Galaxy Sentiment", xaxis = list(title="Sentiment Score"), yaxis=list(title="Frequency of Observations"))

#####################
# Feature selection #
#####################

### Correlation analysis ###

options(max.print=1000000)

corrAll <- cor(galaxy1)
corrAll
corrplot(corrAll, method = "circle")
corrplot(corrAll, order = "hclust")
corrIV <- cor(galaxy1[,1:58])
corrIVhigh <- findCorrelation(corrIV, cutoff=0.8)   
corrIVhigh #   29 34 44 49 37 54 56 21 31 26 51 41 46 42 16 53 40 25  5
galaxyCOR <- galaxy1[, -c(29, 44, 54, 56, 26, 41, 42, 53, 25)]
  
str(galaxyCOR)
#data.frame':	2640 obs. of  50 variables:


## Feature variance ##

nzvMetrics <- nearZeroVar(galaxy1, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(galaxy1, saveMetrics = FALSE) 
nzv #45 variables

galaxyNZV <- galaxy1[,-nzv]
str(galaxyNZV)
#data.frame':	2640 obs. of  14 variables:


## Recursive feature elimination ##

set.seed(123)
galaxySample <- galaxy1[sample(1:nrow(galaxy1), 1000, replace=FALSE),]

# apply factor #

galaxySample$galaxysentiment <- factor(galaxySample$galaxysentiment)

RFcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", repeats=5, verbose=FALSE)
rfeResults <- rfe(galaxySample[,1:58], galaxySample$galaxysentiment, sizes=(1:58), rfeControl=RFcontrol)
rfeResults 

plot(rfeResults, type=c("g", "o"))
predictors(rfeResults)
varImp(rfeResults)

galaxyRFE <- galaxy1[,predictors(rfeResults)]
galaxyRFE$galaxysentiment <- galaxy1$galaxysentiment

str(galaxyRFE)
#data.frame':	2640 obs. of  18 variables:


############################
# Additional Preprocessing #
############################

# Change DV to factor

galaxy1$galaxysentiment <- factor(galaxy1$galaxysentiment)
galaxyCOR$galaxysentiment <- factor(galaxyCOR$galaxysentiment)
galaxyNZV$galaxysentiment <- factor(galaxyNZV$galaxysentiment)
galaxyRFE$galaxysentiment <- factor(galaxyRFE$galaxysentiment)

###################
# Train/test sets #
###################

set.seed(123) 
inTraining <- createDataPartition(galaxy1$galaxysentiment, p=0.70, list=FALSE)
oobTrain <- galaxy1[inTraining,]   
oobTest <- galaxy1[-inTraining,]   
nrow(oobTrain) 
nrow(oobTest)  

set.seed(123) 
inTraining1 <- createDataPartition(galaxyCOR$galaxysentiment, p=0.70, list=FALSE)
corTrain <- galaxyCOR[inTraining,]   
corTest <- galaxyCOR[-inTraining,]   
nrow(corTrain) 
nrow(corTest) 

set.seed(123) 
inTraining2 <- createDataPartition(galaxyNZV$galaxysentiment, p=0.70, list=FALSE)
nzvTrain <- galaxyNZV[inTraining,]   
nzvTest <- galaxyNZV[-inTraining,]   
nrow(nzvTrain) 
nrow(nzvTest) 

set.seed(123) 
inTraining3 <- createDataPartition(galaxyRFE$galaxysentiment, p=0.70, list=FALSE)
rfeTrain <- galaxyRFE[inTraining,]   
rfeTest <- galaxyRFE[-inTraining,]   
nrow(rfeTrain) 
nrow(rfeTest) 


#################
# Train control #
#################

fitControl <- trainControl(method="repeatedcv", number=5, repeats=1) 

################
# Train models #
################


### RANDOM FOREST ###
#####################

##  OOB ##

set.seed(123)
oobRFfit <- train(galaxysentiment~., data=oobTrain, method="rf", importance=T, trControl=fitControl)
plot(oobRFfit)
varImp(oobRFfit)

##  COR ##

set.seed(123)
corRFfit <- train(galaxysentiment~., data=corTrain, method="rf", importance=T, trControl=fitControl)
plot(corRFfit)
varImp(corRFfit)

##  NZV ##

set.seed(123)
nzvRFfit <- train(galaxysentiment~., data=nzvTrain, method="rf", importance=T, trControl=fitControl)
plot(nzvRFfit)
varImp(nzvRFfit)

##  RFE ##

set.seed(123)
rfeRFfit <- train(galaxysentiment~., data=rfeTrain, method="rf", importance=T, trControl=fitControl)
plot(rfeRFfit)
varImp(rfeRFfit)

oobRFfit #   2    0.6032322  0.3117829
corRFfit #    2    0.6037756  0.3140554
nzvRFfit #   2    0.6318883  0.3828352
rfeRFfit #   2    0.6162023  0.3409966


### c5.0 ###
############

set.seed(123)
oobC5fit <- train(galaxysentiment~., data=oobTrain, method="C5.0", importance=T, trControl=fitControl)
plot(oobC5fit)
varImp(oobC5fit)

##  COR ##

set.seed(123)
corC5fit <- train(galaxysentiment~., data=corTrain, method="C5.0", importance=T, trControl=fitControl)
plot(corC5fit)
varImp(corC5fit)

##  NZV ##

set.seed(123)
nzvC5fit <- train(galaxysentiment~., data=nzvTrain, method="C5.0", importance=T, trControl=fitControl)
plot(nzvC5fit)
varImp(nzvC5fit)

##  RFE ##

set.seed(123)
rfeC5fit <- train(galaxysentiment~., data=rfeTrain, method="C5.0", importance=T, trControl=fitControl)
plot(rfeC5fit)
varImp(rfeC5fit)

oobC5fit # rules   TRUE    1      0.6383704  0.4001069
corC5fit #  rules  FALSE    1      0.6335085  0.3957186
nzvC5fit # rules  FALSE    1      0.6324288  0.3963900
rfeC5fit #   rules  FALSE    1      0.6334997  0.3949042

### SVM ###
###########

set.seed(123)
oobSVMfit <- train(galaxysentiment~., data=oobTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(oobSVMfit)
varImp(oobSVMfit)

##  COR ##

set.seed(123)
corSVMfit <- train(galaxysentiment~., data=corTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(corSVMfit)
varImp(corSVMfit)

##  NZV ##

set.seed(123)
nzvSVMfit <- train(galaxysentiment~., data=nzvTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(nzvSVMfit)
varImp(nzvSVMfit)

##  RFE ##

set.seed(123)
rfeSVMfit <- train(galaxysentiment~., data=rfeTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(rfeSVMfit)
varImp(rfeSVMfit)

oobSVMfit #  0.25  0.5832452  0.2819406
corSVMfit #  0.25  0.5853971  0.2821959
nzvSVMfit #  0.25  0.5729310  0.2444939
rfeSVMfit #   0.25  0.5745804  0.2502329

### KNN ###
###########

set.seed(123)
oobKNNfit <- train(galaxysentiment~., data=oobTrain, method="kknn", importance=T, trControl=fitControl)
plot(oobKNNfit)
varImp(oobKNNfit)

##  COR ##

set.seed(123)
corKNNfit <- train(galaxysentiment~., data=corTrain, method="kknn", importance=T, trControl=fitControl)
plot(corKNNfit)
varImp(corKNNfit)

##  NZV ##

set.seed(123)
nzvKNNfit <- train(galaxysentiment~., data=nzvTrain, method="kknn", importance=T, trControl=fitControl)
plot(nzvKNNfit)
varImp(nzvKNNfit)

##  RFE ##

set.seed(123)
rfeKNNfit <- train(galaxysentiment~., data=rfeTrain, method="kknn", importance=T, trControl=fitControl)
plot(rfeKNNfit)
varImp(rfeKNNfit)

oobKNNfit #  9     0.5183551  0.2705101
corKNNfit #  9     0.5221492  0.2772308
nzvKNNfit #  9     0.5313238  0.2793814
rfeKNNfit #  9     0.5508053  0.3182716

###################
# Model selection #
###################

oobFit <- resamples(list(rf=oobRFfit, C5=oobC5fit, SVM=oobSVMfit, KNN=oobKNNfit))
summary(oobFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5962060 0.5989160 0.6010782 0.6032322 0.6054054 0.6145553    0
# C5  0.6189189 0.6368564 0.6415094 0.6383704 0.6422764 0.6522911    0 #
# SVM 0.5795148 0.5826558 0.5962060 0.5913504 0.5972973 0.6010782    0
# KNN 0.4959350 0.5175202 0.5176152 0.5183551 0.5216216 0.5390836    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.2950883 0.3046866 0.3146865 0.3117829 0.3159168 0.3285365    0
# C5  0.3652976 0.3932485 0.3964823 0.4001069 0.4018053 0.4437005    0 #
# SVM 0.2781921 0.2816383 0.3038706 0.2974603 0.3089054 0.3146951    0
# KNN 0.2567251 0.2588522 0.2684725 0.2705101 0.2781474 0.2903533    0

corFit <- resamples(list(rf=corRFfit, C5=corC5fit, SVM=corSVMfit, KNN=corKNNfit))
summary(corFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5989160 0.6016260 0.6027027 0.6037756 0.6037736 0.6118598    0
# C5  0.6162162 0.6280323 0.6314363 0.6335085 0.6395664 0.6522911    0 #
# SVM 0.5822102 0.5853659 0.6010782 0.5962226 0.6027027 0.6097561    0
# KNN 0.5067751 0.5108108 0.5256065 0.5221492 0.5311653 0.5363881    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3058003 0.3073741 0.3120589 0.3140554 0.3193170 0.3257270    0
# C5  0.3598148 0.3735792 0.3970662 0.3957186 0.4026852 0.4454474    0  #
# SVM 0.2764057 0.2808279 0.3119902 0.3029141 0.3171975 0.3281493    0
# KNN 0.2526003 0.2730717 0.2761939 0.2772308 0.2790446 0.3052434    0

nzvFit <- resamples(list(rf=nzvRFfit, C5=nzvC5fit, SVM=nzvSVMfit, KNN=nzvKNNfit))
summary(nzvFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6216216 0.6260163 0.6334232 0.6318883 0.6388140 0.6395664    0
# C5  0.6189189 0.6314363 0.6361186 0.6324288 0.6368564 0.6388140    0  #
# SVM 0.5902965 0.5902965 0.5934959 0.5983906 0.6081081 0.6097561    0
# KNN 0.5121951 0.5189189 0.5311653 0.5313238 0.5363881 0.5579515    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3593508 0.3606990 0.3788349 0.3828352 0.4037587 0.4115327    0
# C5  0.3674907 0.3803297 0.4034233 0.3963900 0.4143065 0.4163996    0  #
# SVM 0.2807877 0.2812342 0.2841565 0.2983677 0.3222330 0.3234272    0
# KNN 0.2406800 0.2469098 0.2751602 0.2793814 0.3040165 0.3301406    0

rfeFit <- resamples(list(rf=rfeRFfit, C5=rfeC5fit, SVM=rfeSVMfit, KNN=rfeKNNfit))
summary(rfeFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6054054 0.6097561 0.6151762 0.6162023 0.6226415 0.6280323    0
# C5  0.6162162 0.6287263 0.6341463 0.6334997 0.6388140 0.6495957    0 #
# SVM 0.5702703 0.5718157 0.5795148 0.5794497 0.5826558 0.5929919    0
# KNN 0.5324324 0.5525606 0.5528455 0.5508053 0.5528455 0.5633423    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3191586 0.3347606 0.3348566 0.3409966 0.3554952 0.3607122    0
# C5  0.3568919 0.3803944 0.3944775 0.3949042 0.4080144 0.4347429    0 #
# SVM 0.2447243 0.2474095 0.2634826 0.2618922 0.2657287 0.2881160    0
# KNN 0.2792559 0.3152141 0.3246927 0.3182716 0.3316951 0.3405004    0

TopFit <- resamples(list(oob=oobC5fit, cor=corC5fit, nzv=nzvC5fit, rfe=rfeC5fit))
summary(TopFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# oob 0.6189189 0.6368564 0.6415094 0.6383704 0.6422764 0.6522911    0  #
# cor 0.6162162 0.6280323 0.6314363 0.6335085 0.6395664 0.6522911    0
# nzv 0.6189189 0.6314363 0.6361186 0.6324288 0.6368564 0.6388140    0
# rfe 0.6162162 0.6287263 0.6341463 0.6334997 0.6388140 0.6495957    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# oob 0.3652976 0.3932485 0.3964823 0.4001069 0.4018053 0.4437005    0  #
# cor 0.3598148 0.3735792 0.3970662 0.3957186 0.4026852 0.4454474    0
# nzv 0.3674907 0.3803297 0.4034233 0.3963900 0.4143065 0.4163996    0
# rfe 0.3568919 0.3803944 0.3944775 0.3949042 0.4080144 0.4347429    0


### Save/load top performing model ###

saveRDS(oobC5fit, "oobC5fitG.rds")  
oobC5fitG <- readRDS ()

####################
# Model validation #
####################

oobC5Pred <- predict(oobC5fit, oobTest)
postResample(oobC5Pred, oobTest$galaxysentiment)
plot(oobC5Pred, oobTest$galaxysentiment)

# Accuracy     Kappa 
# 0.6341772 0.3917670 

cmoobC5 <- confusionMatrix(oobC5Pred, oobTest$galaxysentiment) 
cmoobC5

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   
#     0   1   2   3   4   5
# 0 123   0   2   4   6  14
# 1   0   0   0   0   0   0
# 2   0   0   1   0   0   0
# 3   0   0   2  13   1  11
# 4   3   0   3   2  20   8
# 5  45  29  30  49  80 344
# 
# Overall Statistics
# 
# Accuracy : 0.6342          
# 95% CI : (0.5995, 0.6678)
# No Information Rate : 0.4772          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.3918          
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.7193  0.00000 0.026316  0.19118  0.18692   0.9125
# Specificity            0.9580  1.00000 1.000000  0.98061  0.97657   0.4358
# Pos Pred Value         0.8255      NaN 1.000000  0.48148  0.55556   0.5962
# Neg Pred Value         0.9251  0.96329 0.953105  0.92792  0.88462   0.8451
# Prevalence             0.2165  0.03671 0.048101  0.08608  0.13544   0.4772
# Detection Rate         0.1557  0.00000 0.001266  0.01646  0.02532   0.4354
# Detection Prevalence   0.1886  0.00000 0.001266  0.03418  0.04557   0.7304
# Balanced Accuracy      0.8386  0.50000 0.513158  0.58589  0.58174   0.6742

#######################
# Feature Engineering #
#######################

# PCA #

preprocessParams <- preProcess(oobTrain[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

preprocessParams1 <- preProcess(oobTrain[,-59], method=c("center", "scale", "pca"), thresh = 0.90)
print(preprocessParams1)

train.pca <- predict(preprocessParams, oobTrain[,-59])
train.pca$galaxysentiment <- oobTrain$galaxysentiment
test.pca <- predict(preprocessParams, oobTest[,-59])
test.pca$galaxysentiment <- oobTest$galaxysentiment
str(train.pca)
str(test.pca)

train.pca <- predict(preprocessParams1, oobTrain[,-59])
train.pca$galaxysentiment <- oobTrain$galaxysentiment
test.pca <- predict(preprocessParams1, oobTest[,-59])
test.pca$galaxysentiment <- oobTest$galaxysentiment
str(train.pca)
str(test.pca)

####################
# Predict new data #
####################

oobC5PredNew <- predict(oobC5fit, large)
head(oobC5PredNew)
summary (oobC5PredNew)

#     0     1     2     3     4     5 
# 14508     0   546   461   194  7993 

plot(oobC5PredNew, main="Frequency of Predicted Values - Samsung Galaxy", xlab="Predicted Value", ylab="Frequency")

stopCluster(cl)

