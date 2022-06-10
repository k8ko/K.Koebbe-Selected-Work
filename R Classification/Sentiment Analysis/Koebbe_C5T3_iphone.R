# Title: Predictive Modeling with Big Data

# Last update: 30 January 2022

# File: Koebbe_C5T3_iphone.R

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
install.packages("tidyr")
library(caret)
library(corrplot)
library(doParallel)      
library(plotly)
library(e1071)
library(kknn)
library(tidyr)
library(dplyr)


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

iphone <- read.csv("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C5T3\\iphone_smallmatrix_labeled_8d.csv", stringsAsFactors = FALSE)
large <- read.csv("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C5T3\\Koebbe_LargeMatrix_iphone.csv", stringsAsFactors = FALSE)


# Load preprocessed dataset(s) #

read.csv("iPhoneDS.csv", stringsAsFactors = FALSE) 


#################
# Evaluate data #
#################

## IPHONE ##

str(iphone)  
#'data.frame':	12973 obs. of  59 variables:
summary(iphone)
head(iphone)
tail(iphone)

counts <- iphone %>% count(iphonesentiment, sort=TRUE)
counts

## Header Reference ##

# iOS - counts mentions of iOS on a webpage
# iphonecampos - counts positive sentiment mentions of the iphone camera
# galaxydisneg - counts negative sentiment mentions of the Galaxy display
# htcperunc - counts the unclear sentiment mentions of HTC performance

## Large Matrix ##
str(large)


#################
# Preprocessing #
#################

## IPHONE ##

anyNA(iphone)
anyDuplicated((iphone))
iphone1 <- unique(iphone) # extract unique values / remove duplicates
anyDuplicated(iphone1) #0#

## Large Matrix ##

anyNA(large)
anyDuplicated((large))
# iphone1 <- unique(large) # extract unique values / remove duplicates
# anyDuplicated(large1) #0#

#################
# Save datasets #
#################

write.csv(iphone1, "iPhoneDS", row.names = F)

##################
# Visualizations #
##################

plot_ly(iphone1, x= iphone1$iphonesentiment, type='histogram') %>% layout (title = "iPhone Sentiment", xaxis = list(title="Sentiment Score"), yaxis=list(title="Frequency of Observations"))


#####################
# Feature selection #
#####################

### Correlation analysis ###

options(max.print=1000000)

corrAll <- cor(iphone1)
corrAll
corrplot(corrAll, method = "circle")
corrplot(corrAll, order = "hclust")
corrIV <- cor(iphone1[,1:58])
corrIVhigh <- findCorrelation(corrIV, cutoff=0.8)   
corrIVhigh #  29 34 49 44 37 54 56 21 31 26 51 41 46 42 16 53 40 25  5
iphoneCOR <- iphone1[, -c(34, 49, 37, 21, 31, 41, 42, 53, 40)]
  
str(iphoneCOR)
#data.frame':	2582 obs. of  50 variables:


## Feature variance ##

nzvMetrics <- nearZeroVar(iphone1, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(iphone1, saveMetrics = FALSE) 
nzv #45 variables

iphoneNZV <- iphone1[,-nzv]
str(iphoneNZV)
#'data.frame':	2582 obs. of  14 variables:


## Recursive feature elimination ##


set.seed(123)
iphoneSample <- iphone1[sample(1:nrow(iphone1), 1000, replace=FALSE),]

# apply factor #

iphoneSample$iphonesentiment <- factor(iphoneSample$iphonesentiment)

RFcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", repeats=5, verbose=FALSE)
rfeResults <- rfe(iphoneSample[,1:58], iphoneSample$iphonesentiment, sizes=(1:58), rfeControl=RFcontrol)
rfeResults 

# Resampling performance over subset size:
#   
#   Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 1   0.6058 0.3285    0.02759 0.05184         
# 2   0.6078 0.3393    0.03302 0.05910         
# 3   0.6274 0.3719    0.02421 0.04474         
# 4   0.6216 0.3776    0.03120 0.05317         
# 5   0.6306 0.3921    0.02842 0.05184         
# 6   0.6374 0.4053    0.03265 0.05902         
# 7   0.6471 0.4227    0.03052 0.05498         
# 8   0.6525 0.4294    0.03257 0.06014         
# 9   0.6486 0.4334    0.03178 0.05729         
# 10   0.6566 0.4448    0.03027 0.05332         
# 11   0.6615 0.4511    0.03253 0.05704         
# 12   0.6607 0.4481    0.03205 0.05707         
# 13   0.6624 0.4494    0.03002 0.05369         
# 14   0.6630 0.4502    0.03127 0.05487         
# 15   0.6614 0.4451    0.02827 0.05038         
# 16   0.6600 0.4500    0.03006 0.05272         
# 17   0.6632 0.4535    0.02999 0.05227         
# 18   0.6639 0.4531    0.03083 0.05434        *
#   19   0.6616 0.4474    0.02934 0.05235         
# 20   0.6614 0.4460    0.02886 0.05211         
# 21   0.6594 0.4419    0.03079 0.05434         
# 22   0.6588 0.4392    0.03022 0.05436         
# 23   0.6566 0.4346    0.02938 0.05263         
# 24   0.6566 0.4347    0.03017 0.05365         
# 25   0.6582 0.4409    0.02976 0.05322         
# 26   0.6604 0.4432    0.02752 0.04972         
# 27   0.6582 0.4391    0.02732 0.04905         
# 28   0.6578 0.4378    0.02831 0.05146         
# 29   0.6566 0.4351    0.02714 0.04910         
# 30   0.6552 0.4330    0.02690 0.04808         
# 31   0.6557 0.4334    0.02660 0.04801         
# 32   0.6558 0.4331    0.02695 0.04884         
# 33   0.6552 0.4323    0.02826 0.05061         
# 34   0.6572 0.4353    0.02718 0.04911         
# 35   0.6563 0.4330    0.02764 0.04972         
# 36   0.6576 0.4378    0.02794 0.04998         
# 37   0.6572 0.4365    0.02817 0.05040         
# 38   0.6578 0.4373    0.02688 0.04863         
# 39   0.6574 0.4370    0.02643 0.04708         
# 40   0.6566 0.4352    0.02729 0.04924         
# 41   0.6556 0.4333    0.02715 0.04841         
# 42   0.6568 0.4348    0.02750 0.04867         
# 43   0.6564 0.4341    0.02794 0.05026         
# 44   0.6552 0.4312    0.02652 0.04780         
# 45   0.6552 0.4311    0.02800 0.05070         
# 46   0.6561 0.4320    0.02721 0.04883         
# 47   0.6564 0.4324    0.02908 0.05213         
# 48   0.6556 0.4312    0.02691 0.04796         
# 49   0.6554 0.4324    0.02569 0.04622         
# 50   0.6554 0.4324    0.02797 0.05005         
# 51   0.6558 0.4328    0.02844 0.05057         
# 52   0.6558 0.4330    0.02790 0.04968         
# 53   0.6554 0.4319    0.02808 0.05089         
# 54   0.6559 0.4324    0.02709 0.04867         
# 55   0.6575 0.4347    0.02754 0.04886         
# 56   0.6559 0.4316    0.02906 0.05161         
# 57   0.6567 0.4329    0.02781 0.04965         
# 58   0.6561 0.4319    0.02656 0.04769         
# 
# The top 5 variables (out of 18):
#   iphone, iphonedisneg, iphoneperpos, iphonedisunc, htcphone

plot(rfeResults, type=c("g", "o"))
predictors(rfeResults)
varImp(rfeResults)

# Overall
# iphone        13.632657
# iphonedisneg   6.623015
# iphoneperpos   5.968005
# iphonedisunc   5.841586
# htcphone       5.415955
# iphonedispos   5.383515
# iphoneperneg   5.215989
# iphonecamneg   4.195568
# htccampos      4.025089
# iphonecamunc   3.990471
# iphoneperunc   3.767413
# samsunggalaxy  3.705635
# iphonecampos   3.622313
# samsungdisunc  3.385473
# htcdispos      3.190315
# ios            2.960509
# samsungcamunc  2.944413
# googleandroid  2.923422
# htcdisneg      2.914656
# htcperpos      2.893369
# htccamneg      2.843322
# googleperpos   2.813065
# googleperneg   2.792564
# htcperneg      2.734217
# sonyxperia     2.572267
# sonyperpos     2.321175
# htccamunc      1.982144

iphoneRFE <- iphone1[,predictors(rfeResults)]
iphoneRFE$iphonesentiment <- iphone1$iphonesentiment

str(iphoneRFE)


############################
# Additional Preprocessing #
############################

# Change DV to factor

iphone1$iphonesentiment <- factor(iphone1$iphonesentiment)
iphoneCOR$iphonesentiment <- factor(iphoneCOR$iphonesentiment)
iphoneNZV$iphonesentiment <- factor(iphoneNZV$iphonesentiment)
iphoneRFE$iphonesentiment <- factor(iphoneRFE$iphonesentiment)

###################
# Train/test sets #
###################

set.seed(123) 
inTraining <- createDataPartition(iphone1$iphonesentiment, p=0.70, list=FALSE)
oobTrain <- iphone1[inTraining,]   
oobTest <- iphone1[-inTraining,]   
nrow(oobTrain) 
nrow(oobTest)  

set.seed(123) 
inTraining1 <- createDataPartition(iphoneCOR$iphonesentiment, p=0.70, list=FALSE)
corTrain <- iphoneCOR[inTraining,]   
corTest <- iphoneCOR[-inTraining,]   
nrow(corTrain) 
nrow(corTest) 

set.seed(123) 
inTraining2 <- createDataPartition(iphoneNZV$iphonesentiment, p=0.70, list=FALSE)
nzvTrain <- iphoneNZV[inTraining,]   
nzvTest <- iphoneNZV[-inTraining,]   
nrow(nzvTrain) 
nrow(nzvTest) 

set.seed(123) 
inTraining3 <- createDataPartition(iphoneRFE$iphonesentiment, p=0.70, list=FALSE)
rfeTrain <- iphoneRFE[inTraining,]   
rfeTest <- iphoneRFE[-inTraining,]   
nrow(rfeTrain) 
nrow(rfeTest) 


#################
# Train control #
#################

fitControl <- trainControl(method="repeatedcv", number=5, repeats=1) 

################
# Train models #
################
modelLookup("svmLinear2")

### RANDOM FOREST ###
#####################

##  OOB ##

set.seed(123)
oobRFfit <- train(iphonesentiment~., data=oobTrain, method="rf", importance=T, trControl=fitControl)
plot(oobRFfit)
varImp(oobRFfit)

##  COR ##

set.seed(123)
corRFfit <- train(iphonesentiment~., data=corTrain, method="rf", importance=T, trControl=fitControl)
plot(corRFfit)
varImp(corRFfit)

##  NZV ##

set.seed(123)
nzvRFfit <- train(iphonesentiment~., data=nzvTrain, method="rf", importance=T, trControl=fitControl)
plot(nzvRFfit)
varImp(nzvRFfit)

##  RFE ##

set.seed(123)
rfeRFfit <- train(iphonesentiment~., data=rfeTrain, method="rf", importance=T, trControl=fitControl)
plot(rfeRFfit)
varImp(rfeRFfit)

oobRFfit

# mtry  Accuracy   Kappa    
# 2    0.6108319  0.3417831

corRFfit

# mtry  Accuracy   Kappa    
# 2    0.6152488  0.3502909

nzvRFfit

# mtry  Accuracy   Kappa    
# 2    0.6462310  0.4222065

rfeRFfit

# mtry  Accuracy   Kappa    
# 2    0.6318509  0.3886837

### c5.0 ###
############

set.seed(123)
oobC5fit <- train(iphonesentiment~., data=oobTrain, method="C5.0", importance=T, trControl=fitControl)
plot(oobC5fit)
varImp(oobC5fit)

##  COR ##

set.seed(123)
corC5fit <- train(iphonesentiment~., data=corTrain, method="C5.0", importance=T, trControl=fitControl)
plot(corC5fit)
varImp(corC5fit)

##  NZV ##

set.seed(123)
nzvC5fit <- train(iphonesentiment~., data=nzvTrain, method="C5.0", importance=T, trControl=fitControl)
plot(nzvC5fit)
varImp(nzvC5fit)

##  RFE ##

set.seed(123)
rfeC5fit <- train(iphonesentiment~., data=rfeTrain, method="C5.0", importance=T, trControl=fitControl)
plot(rfeC5fit)
varImp(rfeC5fit)


oobC5fit

#rules   TRUE    1      0.6445507  0.4235068

corC5fit

#rules  FALSE    1      0.6406757  0.4207564

nzvC5fit

#tree   FALSE   10      0.6462204  0.4289801

rfeC5fit

# rules  FALSE    1      0.6439891  0.4287880

### SVM ###
###########

set.seed(123)
oobSVMfit <- train(iphonesentiment~., data=oobTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(oobSVMfit)
varImp(oobSVMfit)

##  COR ##

set.seed(123)
corSVMfit <- train(iphonesentiment~., data=corTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(corSVMfit)
varImp(corSVMfit)

##  NZV ##

set.seed(123)
nzvSVMfit <- train(iphonesentiment~., data=nzvTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(nzvSVMfit)
varImp(nzvSVMfit)

##  RFE ##

set.seed(123)
rfeSVMfit <- train(iphonesentiment~., data=rfeTrain, method="svmLinear2", importance=T, trControl=fitControl)
plot(rfeSVMfit)
varImp(rfeSVMfit)

oobSVMfit

#  cost  Accuracy   Kappa    
# 0.25  0.5881675  0.3133425

corSVMfit

#  cost  Accuracy   Kappa    
#  0.5887367  0.3145377

nzvSVMfit

# cost  Accuracy   Kappa    
# 0.25  0.5893014  0.3075503

rfeSVMfit

# 0.25  0.6042431  0.3381098

### KNN ###
###########

set.seed(123)
oobKNNfit <- train(iphonesentiment~., data=oobTrain, method="kknn", importance=T, trControl=fitControl)
plot(oobKNNfit)
varImp(oobKNNfit)

##  COR ##

set.seed(123)
corKNNfit <- train(iphonesentiment~., data=corTrain, method="kknn", importance=T, trControl=fitControl)
plot(corKNNfit)
varImp(corKNNfit)

##  NZV ##

set.seed(123)
nzvKNNfit <- train(iphonesentiment~., data=nzvTrain, method="kknn", importance=T, trControl=fitControl)
plot(nzvKNNfit)
varImp(nzvKNNfit)

##  RFE ##

set.seed(123)
rfeKNNfit <- train(iphonesentiment~., data=rfeTrain, method="kknn", importance=T, trControl=fitControl)
plot(rfeKNNfit)
varImp(rfeKNNfit)

oobKNNfit

# kmax  Accuracy   Kappa   
# 9     0.5135429  0.2930574

corKNNfit

#   9     0.5179461  0.2972364

nzvKNNfit

#  9     0.5168534  0.2908953

rfeKNNfit

# 9     0.5273568  0.3081839


###################
# Model selection #
###################

oobFit <- resamples(list(rf=oobRFfit, C5=oobC5fit, SVM=oobSVMfit, KNN=oobKNNfit))
summary(oobFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6011080 0.6094183 0.6104972 0.6108319 0.6126374 0.6204986    0
# C5  0.6315789 0.6454294 0.6456044 0.6445507 0.6491713 0.6509695    0  ##
# SVM 0.5817175 0.5844875 0.5879121 0.5881675 0.5927978 0.5939227    0
# KNN 0.4986150 0.5137363 0.5138122 0.5135429 0.5152355 0.5263158    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3262394 0.3373560 0.3412405 0.3417831 0.3480763 0.3560035    0
# C5  0.4030535 0.4189098 0.4237039 0.4235068 0.4355775 0.4362892    0  ##
# SVM 0.2940946 0.3033471 0.3154853 0.3133425 0.3158323 0.3379532    0
# KNN 0.2733753 0.2819727 0.2877597 0.2930574 0.3075311 0.3146483    0

corFit <- resamples(list(rf=corRFfit, C5=corC5fit, SVM=corSVMfit, KNN=corKNNfit))
summary(corFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6038781 0.6149584 0.6177285 0.6152488 0.6181319 0.6215470    0
# C5  0.6315789 0.6353591 0.6371191 0.6406757 0.6483516 0.6509695    0  ##
# SVM 0.5851648 0.5900277 0.5955679 0.5970348 0.5966851 0.6177285    0
# KNN 0.4958449 0.5152355 0.5165746 0.5179461 0.5302198 0.5318560    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3302412 0.3492796 0.3531810 0.3502909 0.3572581 0.3614945    0
# C5  0.4028754 0.4138515 0.4167653 0.4207564 0.4286324 0.4416576    0  ##
# SVM 0.3102600 0.3110687 0.3294231 0.3303847 0.3402819 0.3608898    0
# KNN 0.2673402 0.2826407 0.2842414 0.2972364 0.3222879 0.3296717    0

nzvFit <- resamples(list(rf=nzvRFfit, C5=nzvC5fit, SVM=nzvSVMfit, KNN=nzvKNNfit))
summary(nzvFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6318681 0.6426593 0.6454294 0.6462310 0.6537396 0.6574586    0  ##
# C5  0.6426593 0.6428571 0.6436464 0.6462204 0.6509695 0.6509695    0
# SVM 0.5734072 0.5769231 0.5939227 0.5898478 0.6011080 0.6038781    0
# KNN 0.4972376 0.5041551 0.5235457 0.5168534 0.5274725 0.5318560    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3980278 0.4120298 0.4221327 0.4222065 0.4356200 0.4432220    0
# C5  0.4194622 0.4222690 0.4305210 0.4289801 0.4336973 0.4389509    0  ##
# SVM 0.2704391 0.2950527 0.3131639 0.3057558 0.3225693 0.3275541    0
# KNN 0.2513012 0.2660101 0.3002062 0.2908953 0.3146519 0.3223069    0

rfeFit <- resamples(list(rf=rfeRFfit, C5=rfeC5fit, SVM=rfeSVMfit, KNN=rfeKNNfit))
summary(rfeFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.6204986 0.6208791 0.6288089 0.6318509 0.6426593 0.6464088    0
# C5  0.6298343 0.6371191 0.6398892 0.6439891 0.6565097 0.6565934    0  ##
# SVM 0.5961538 0.6011080 0.6094183 0.6114072 0.6215470 0.6288089    0
# KNN 0.5180055 0.5220994 0.5318560 0.5273568 0.5318560 0.5329670    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.3649671 0.3723275 0.3840767 0.3886837 0.4052642 0.4167831    0
# C5  0.4048828 0.4147660 0.4243201 0.4287880 0.4452142 0.4547568    0  ##
# SVM 0.3237536 0.3287797 0.3400708 0.3490520 0.3749575 0.3776983    0
# KNN 0.2903576 0.2931922 0.3123965 0.3081839 0.3145980 0.3303755    0

TopFit <- resamples(list(oob=oobC5fit, cor=corC5fit, nzv=nzvRFfit, rfe=rfeC5fit))
summary(TopFit)

# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# oob 0.6315789 0.6454294 0.6456044 0.6445507 0.6491713 0.6509695    0  ##
# cor 0.6315789 0.6353591 0.6371191 0.6406757 0.6483516 0.6509695    0
# nzv 0.6318681 0.6426593 0.6454294 0.6462310 0.6537396 0.6574586    0
# rfe 0.6298343 0.6371191 0.6398892 0.6439891 0.6565097 0.6565934    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# oob 0.4030535 0.4189098 0.4237039 0.4235068 0.4355775 0.4362892    0 
# cor 0.4028754 0.4138515 0.4167653 0.4207564 0.4286324 0.4416576    0
# nzv 0.3980278 0.4120298 0.4221327 0.4222065 0.4356200 0.4432220    0
# rfe 0.4048828 0.4147660 0.4243201 0.4287880 0.4452142 0.4547568    0 ##


### Save/load top performing model ###

saveRDS(nzvC5fit, "nzvC5fit.rds")  
oobC5Fit1 <- readRDS ()

####################
# Model validation #
####################


nzvC5Pred <- predict(nzvC5fit, nzvTest)
postResample(nzvC5Pred, nzvTest$iphonesentiment)
plot(nzvC5Pred, nzvTest$iphonesentiment)

# Accuracy     Kappa 
# 0.6662354 0.4638266 

cmoobC5 <- confusionMatrix(nzvC5Pred, nzvTest$iphonesentiment) 
cmoobC5

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2   3   4   5
# 0 134   0   4   1   7   8
# 1   0   0   0   0   0   0
# 2   0   0   0   0   0   0
# 3   1   0   0  21   5   8
# 4   3   0   1   1  31   6
# 5  48  29  33  36  67 329
# 
# Overall Statistics
# 
# Accuracy : 0.6662          
# 95% CI : (0.6318, 0.6994)
# No Information Rate : 0.4541          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4638          
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.7204  0.00000  0.00000  0.35593  0.28182   0.9373
# Specificity            0.9659  1.00000  1.00000  0.98039  0.98341   0.4953
# Pos Pred Value         0.8701      NaN      NaN  0.60000  0.73810   0.6070
# Neg Pred Value         0.9160  0.96248  0.95084  0.94851  0.89193   0.9048
# Prevalence             0.2406  0.03752  0.04916  0.07633  0.14230   0.4541
# Detection Rate         0.1734  0.00000  0.00000  0.02717  0.04010   0.4256
# Detection Prevalence   0.1992  0.00000  0.00000  0.04528  0.05433   0.7012
# Balanced Accuracy      0.8432  0.50000  0.50000  0.66816  0.63261   0.7163



#######################
# Feature Engineering #
#######################

# PCA #

preprocessParams <- preProcess(oobTrain[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

train.pca <- predict(preprocessParams, oobTrain[,-59])
train.pca$iphonesentiment <- oobTrain$iphonesentiment
test.pca <- predict(preprocessParams, oobTest[,-59])
test.pca$iphonesentiment <- oobTest$iphonesentiment
str(train.pca)
str(test.pca)

# Created from 1809 samples and 58 variables
# 
# Pre-processing:
#   - centered (58)
# - ignored (0)
# - principal component signal extraction (58)
# - scaled (58)
# 
# PCA needed 28 components to capture 95 percent of the variance

preprocessParams <- preProcess(oobTrain[,-59], method=c("center", "scale", "pca"), thresh = 0.90)
print(preprocessParams)

train.pca1 <- predict(preprocessParams, oobTrain[,-59])
train.pca1$iphonesentiment <- oobTrain$iphonesentiment
test.pca1 <- predict(preprocessParams, oobTest[,-59])
test.pca1$iphonesentiment <- oobTest$iphonesentiment
str(train.pca1)
str(test.pca1)

# Created from 1809 samples and 58 variables
# 
# Pre-processing:
#   - centered (58)
# - ignored (0)
# - principal component signal extraction (58)
# - scaled (58)
# 
# PCA needed 20 components to capture 90 percent of the variance

# Will research this further later #

# train.pca2 <- predict(oobC5fit, oobTrain[,-59])
# train.pca2$iphonesentiment <- oobTrain$iphonesentiment
# test.pca2 <- predict(oobC5fit, oobTest[,-59])
# test.pca2$iphonesentiment <- oobTest$iphonesentiment
# str(train.pca2)
# str(test.pca2)
# 
# ## oobC5PCA ##
# 
# set.seed(123)
# oobC5PCA <- train(iphonesentiment~., data=train.pca2, method="C5.0", importance=T, trControl=fitControl)
# plot(oobC5fit)
# varImp(oobC5fit)

####################
# Predict new data #
####################

nzvC5PredNew <- predict(nzvC5fit, large)
head(nzvC5PredNew)
summary (nzvC5PredNew)
plot(nzvC5PredNew, main="Frequency of Predicted Values - iPhone", xlab="Predicted Value", ylab="Frequency")

# summary (nzvC5PredNew, main="Frequency of Predicted Values - iPhone", xlab="Predicted Value", ylab="Frequency")
#   0     1     2     3     4     5 
# 15223     0     0  1108   145  7226 

stopCluster(cl)

