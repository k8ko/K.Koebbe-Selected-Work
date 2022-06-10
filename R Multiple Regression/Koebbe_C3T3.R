# Title: Brand & Product Preference Analysis

# Last update: 16 Jan 2022

# File: Koebbe_C3T3.R

# Project name: UT DA 2021 / C3T3


#################
# Project Notes #
#################

## Summary ##

# Use multiple regression in R to determine brand and product preferences #

## Deliverable(s) ## 

# 1) Predict sales of four different product types: PC (1379.4199), Laptops (190.0588), Netbooks (103.7575), and Smartphones (1114.3819) #
# 2) Assess the impact services reviews and customer reviews have on sales of different product types #


###############
# Housekeeping
###############

# Clear console: CTRL + L

rm(list = ls())
getwd()
setwd("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C3T3")
dir()

#################
# Load packages #
#################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doParallel") 
install.packages("plyr")
library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(doParallel) 
library(plyr)


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

currentPA <- read.csv("existingproductattributes2017.csv", stringsAsFactors = FALSE)
str(currentPA)

newPA <- read.csv("newproductattributes2017.csv", stringsAsFactors = FALSE)
str(newPA)

# Load preprocessed dataset(s) #

read.csv("CurrentPA.csv", stringsAsFactors = FALSE) 
read.csv("NewPA.csv", stringsAsFactors = FALSE) 

#################
# Evaluate data #
#################

## currentPA ##

str(currentPA)  

# 'data.frame':	80 obs. of  18 variables:
# $ ProductType          : chr  "PC" "PC" "PC" "Laptop" ...
# $ ProductNum           : int  101 102 103 104 105 106 107 108 109 110 ...
# $ Price                : num  949 2250 399 410 1080 ...
# $ x5StarReviews        : int  3 2 3 49 58 83 11 33 16 10 ...
# $ x4StarReviews        : int  3 1 0 19 31 30 3 19 9 1 ...
# $ x3StarReviews        : int  2 0 0 8 11 10 0 12 2 1 ...
# $ x2StarReviews        : int  0 0 0 3 7 9 0 5 0 0 ...
# $ x1StarReviews        : int  0 0 0 9 36 40 1 9 2 0 ...
# $ PositiveServiceReview: int  2 1 1 7 7 12 3 5 2 2 ...
# $ NegativeServiceReview: int  0 0 0 8 20 5 0 3 1 0 ...
# $ Recommendproduct     : num  0.9 0.9 0.9 0.8 0.7 0.3 0.9 0.7 0.8 0.9 ...
# $ BestSellersRank      : int  1967 4806 12076 109 268 64 NA 2 NA 18 ...
# $ ShippingWeight       : num  25.8 50 17.4 5.7 7 1.6 7.3 12 1.8 0.75 ...
# $ ProductDepth         : num  23.9 35 10.5 15 12.9 ...
# $ ProductWidth         : num  6.62 31.75 8.3 9.9 0.3 ...
# $ ProductHeight        : num  16.9 19 10.2 1.3 8.9 ...
# $ ProfitMargin         : num  0.15 0.25 0.08 0.08 0.09 0.05 0.05 0.05 0.05 0.05 ...
# $ Volume               : int  12 8 12 196 232 332 44 132 64 40 ...

head(currentPA)

# ProductType ProductNum   Price x5StarReviews x4StarReviews x3StarReviews x2StarReviews x1StarReviews
# 1          PC        101  949.00             3             3             2             0             0
# 2          PC        102 2249.99             2             1             0             0             0
# 3          PC        103  399.00             3             0             0             0             0
# 4      Laptop        104  409.99            49            19             8             3             9
# 5      Laptop        105 1079.99            58            31            11             7            36
# 6 Accessories        106  114.22            83            30            10             9            40
# PositiveServiceReview NegativeServiceReview Recommendproduct BestSellersRank ShippingWeight ProductDepth
# 1                     2                     0              0.9            1967           25.8        23.94
# 2                     1                     0              0.9            4806           50.0        35.00
# 3                     1                     0              0.9           12076           17.4        10.50
# 4                     7                     8              0.8             109            5.7        15.00
# 5                     7                    20              0.7             268            7.0        12.90
# 6                    12                     5              0.3              64            1.6         5.80
# ProductWidth ProductHeight ProfitMargin Volume
# 1         6.62         16.89         0.15     12
# 2        31.75         19.00         0.25      8
# 3         8.30         10.20         0.08     12
# 4         9.90          1.30         0.08    196
# 5         0.30          8.90         0.09    232
# 6         4.00          1.00         0.05    332

tail(currentPA)

# ProductType ProductNum  Price x5StarReviews x4StarReviews x3StarReviews x2StarReviews x1StarReviews
# 75      Tablet        189 419.00             3             1             0             0             0
# 76  Smartphone        191 200.00            62            25            10            11            12
# 77  Smartphone        192  99.00            18            17             6             2            12
# 78     Netbook        182 349.99            22            10             6             2            10
# 79  Smartphone        197 499.00           368            28            14            10            23
# 80 GameConsole        200 299.99           421            87            20            14            39
# PositiveServiceReview NegativeServiceReview Recommendproduct BestSellersRank ShippingWeight ProductDepth
# 75                     0                     0              0.9             544           2.20         7.00
# 76                     9                     3              0.8             720           0.90         2.80
# 77                     5                     4              0.7            5742           0.70         2.80
# 78                     3                     3              0.3            2723           5.00         7.57
# 79                    22                     3              0.9           14086           0.90         2.70
# 80                    29                    14              0.9             352          10.94        12.00
# ProductWidth ProductHeight ProfitMargin Volume
# 75        10.20          0.40         0.18     12
# 76         5.40          0.30         0.14    248
# 77         5.30          0.40         0.17     72
# 78        10.47          1.43         0.12     88
# 79         5.00          0.40         0.10   1472
# 80        11.50          7.25         0.12   1684

anyNA(currentPA) ##YES##
anyDuplicated((currentPA)) ##NO##

## newPA ##

str(newPA)

# 'data.frame':	24 obs. of  18 variables:
# $ ProductType          : chr  "PC" "PC" "Laptop" "Laptop" ...
# $ ProductNum           : int  171 172 173 175 176 178 180 181 183 186 ...
# $ Price                : num  699 860 1199 1199 1999 ...
# $ x5StarReviews        : int  96 51 74 7 1 19 312 23 3 296 ...
# $ x4StarReviews        : int  26 11 10 2 1 8 112 18 4 66 ...
# $ x3StarReviews        : int  14 10 3 1 1 4 28 7 0 30 ...
# $ x2StarReviews        : int  14 10 3 1 3 1 31 22 1 21 ...
# $ x1StarReviews        : int  25 21 11 1 0 10 47 18 0 36 ...
# $ PositiveServiceReview: int  12 7 11 2 0 2 28 5 1 28 ...
# $ NegativeServiceReview: int  3 5 5 1 1 4 16 16 0 9 ...
# $ Recommendproduct     : num  0.7 0.6 0.8 0.6 0.3 0.6 0.7 0.4 0.7 0.8 ...
# $ BestSellersRank      : int  2498 490 111 4446 2820 4140 2699 1704 5128 34 ...
# $ ShippingWeight       : num  19.9 27 6.6 13 11.6 5.8 4.6 4.8 4.3 3 ...
# $ ProductDepth         : num  20.63 21.89 8.94 16.3 16.81 ...
# $ ProductWidth         : num  19.2 27 12.8 10.8 10.9 ...
# $ ProductHeight        : num  8.39 9.13 0.68 1.4 0.88 1.2 0.95 1.5 0.97 0.37 ...
# $ ProfitMargin         : num  0.25 0.2 0.1 0.15 0.23 0.08 0.09 0.11 0.09 0.1 ...
# $ Volume               : int  0 0 0 0 0 0 0 0 0 0 ...

head(newPA)

# ProductType ProductNum   Price x5StarReviews x4StarReviews x3StarReviews x2StarReviews x1StarReviews
# 1          PC        171  699.00            96            26            14            14            25
# 2          PC        172  860.00            51            11            10            10            21
# 3      Laptop        173 1199.00            74            10             3             3            11
# 4      Laptop        175 1199.00             7             2             1             1             1
# 5      Laptop        176 1999.00             1             1             1             3             0
# 6     Netbook        178  399.99            19             8             4             1            10
# PositiveServiceReview NegativeServiceReview Recommendproduct BestSellersRank ShippingWeight ProductDepth
# 1                    12                     3              0.7            2498           19.9        20.63
# 2                     7                     5              0.6             490           27.0        21.89
# 3                    11                     5              0.8             111            6.6         8.94
# 4                     2                     1              0.6            4446           13.0        16.30
# 5                     0                     1              0.3            2820           11.6        16.81
# 6                     2                     4              0.6            4140            5.8         8.43
# ProductWidth ProductHeight ProfitMargin Volume
# 1        19.25          8.39         0.25      0
# 2        27.01          9.13         0.20      0
# 3        12.80          0.68         0.10      0
# 4        10.80          1.40         0.15      0
# 5        10.90          0.88         0.23      0
# 6        11.42          1.20         0.08      0

tail(newPA)

# ProductType ProductNum  Price x5StarReviews x4StarReviews x3StarReviews x2StarReviews x1StarReviews
# 19      Accessories        302   8.50            25             2             2             4            15
# 20         Software        303  70.99            29            18             3             1             8
# 21          Printer        304 199.99            88             8             3             1             3
# 22  PrinterSupplies        305  20.99             5             0             0             0             0
# 23 ExtendedWarranty        306  99.99             0             1             1             1             1
# 24      GameConsole        307 425.00          1525           252            99            56            45
# PositiveServiceReview NegativeServiceReview Recommendproduct BestSellersRank ShippingWeight ProductDepth
# 19                     2                     1              0.5              38            1.0          7.3
# 20                     4                     2              0.8             122            0.2          8.0
# 21                     5                     1              0.8              40           42.0         17.3
# 22                     1                     0              1.0            1011            1.0          4.7
# 23                     0                     3              0.4               5            0.2          0.0
# 24                    59                    13              0.9             215           20.0          8.5
# ProductWidth ProductHeight ProfitMargin Volume
# 19          7.0          1.60         0.10      0
# 20          7.0          1.00         0.20      0
# 21         23.5         25.80         0.90      0
# 22          2.9          6.30         0.30      0
# 23          0.0          0.00         0.40      0
# 24          6.0          1.75         0.18      0

anyNA(newPA) ##NO##
anyDuplicated((newPA)) ##NO##

dir()

#################
# Preprocessing #
#################

## currentPA ##

currentPA$feature_to_remove <- NULL
str(currentPA) 

#names(currentPA) <- c("ColumnName","ColumnName","ColumnName") 

dummy <- dummyVars("~.", data = currentPA)
DcurrentPA <- data.frame(predict(dummy, newdata = currentPA))
str(DcurrentPA)

summary(DcurrentPA)
DcurrentPA$BestSellersRank[is.na(DcurrentPA$BestSellersRank)] <- mean(DcurrentPA$BestSellersRank,na.rm = TRUE)
anyNA(DcurrentPA)
DcurrentPA

## newPA ##

str(newPA) 

newPA$feature_to_remove <- NULL

dummy1 <- dummyVars("~.", data = newPA)
DnewPA <- data.frame(predict(dummy1, newdata = newPA))
str(DnewPA)


#################
# Save datasets #
#################

write.csv(DcurrentPA, "CurrentPA.csv", row.names = F)
write.csv(DnewPA, "NewPA.csv", row.names = F)

######################
# EDA/Visualizations #
######################

summary(DcurrentPA)

count(DcurrentPA$ProductTypePC)
count(DcurrentPA$ProductTypeLaptop)
count(DcurrentPA$ProductTypeNetbook)
count(DcurrentPA$ProductTypeSmartphone)

hist(DcurrentPA$Volume)
hist(DcurrentPA$PositiveServiceReview)
hist(DcurrentPA$NegativeServiceReview)
plot(DcurrentPA$Volume, DcurrentPA$PositiveServiceReview)
plot(DcurrentPA$Volume, DcurrentPA$NegativeServiceReview)
plot(DcurrentPA$Volume, DcurrentPA$ProductTypePC)
plot(DcurrentPA$Volume, DcurrentPA$ProductTypeLaptop)
plot(DcurrentPA$Volume, DcurrentPA$ProductTypeNetbook)
plot(DcurrentPA$Volume, DcurrentPA$ProductTypeSmartphone)
qqnorm(DcurrentPA$Volume)

############
# Sampling #
############

# #  10% sample #
# 
# set.seed(1) 
# currentPA10p <- currentPA[sample(1:nrow(currentPA), round(nrow(currentPA)*.1),replace=FALSE),]
# nrow(currentPA10p)
# head(currentPA10p) 


#####################
# Feature selection #
#####################

### Correlation analysis ###

corrAll <- cor(DcurrentPA[,1:29])
corrAll
corrplot(corrAll, method = "circle")
corrplot(corrAll, order = "hclust")
corrIV <- cor(DcurrentPA[,1:29])
corrIVhigh <- findCorrelation(corrIV, cutoff=0.8)   
corrIVhigh
colnames(DcurrentPA[c(17, 16, 15, 18, 19, 3)]) 

# Remove highly correlated variables #

currentPA_corr <- corrAll[,-corrIVhigh]
currentPA2 <- cor(currentPA_corr)
summary(currentPA2[upper.tri(currentPA2)])
currentPA2
corrplot(currentPA2, method = "circle")
corrplot(currentPA2, order = "hclust")

dfcor <- DcurrentPA[, -c(17, 16, 15, 18, 19)]
dfcor

str(dfcor)
str(DcurrentPA)

##############
# Train sets #
##############

set.seed(123) 
inTraining <- createDataPartition(DcurrentPA$Volume, p=0.75, list=FALSE)
oobTrain <- DcurrentPA[inTraining,]   
oobTest <- DcurrentPA[-inTraining,]   
nrow(oobTrain) 
nrow(oobTest)  

# Repeat above for dfcor #

set.seed(123) 
inTraining <- createDataPartition(dfcor$Volume, p=0.75, list=FALSE)
CorTrain <- dfcor[inTraining,]   
CorTest <- dfcor[-inTraining,]   
nrow(CorTrain) 
nrow(CorTest) 

#################
# Train control #
#################

fitControl <- trainControl(method="repeatedcv", number=5, repeats=1) 

################
# Train models #
################

modelLookup('rf')

##  Random Forest ##

# OOB #

set.seed(123)
oobRFtest <- train(Volume~., data=oobTrain, method="rf", importance=T, trControl=fitControl)
rfGrid <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
oobRFtest <- train(Volume~.,data=oobTrain,method="rf",
                  importance=T,
                  trControl=fitControl,
                  tuneGrid=rfGrid)

plot(oobRFtest)
print(oobRFtest)
varImp(oobRFtest)

# Note output #

# mtry  RMSE      Rsquared   MAE     
# 6     912.2902  0.8344831  395.7821

# x5StarReviews                100.00
# PositiveServiceReview         87.05
# x1StarReviews                 76.82
# x3StarReviews                 73.12
# x2StarReviews                 71.89
# x4StarReviews                 69.07
# NegativeServiceReview         48.84
# ProductTypeGameConsole        47.80
# ProductTypePC                 38.70
# ShippingWeight                38.58
# ProductHeight                 38.36
# BestSellersRank               37.11
# Price                         36.80
# ProductNum                    31.38
# ProductTypeExtendedWarranty   30.49
# ProductTypePrinter            29.36
# ProductTypePrinterSupplies    27.94
# ProductTypeAccessories        24.79
# ProductTypeSoftware           24.17
# ProfitMargin                  22.77

# CorTest #

set.seed(123)
CorRFTest <- train(Volume~., data=CorTrain, method="rf", importance=T, trControl=fitControl)
rfGrid2 <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
CorRFTest <- train(Volume~.,data=CorTrain,method="rf",
                      importance=T,
                      trControl=fitControl,
                      tuneGrid=rfGrid2)

plot(CorRFTest)
print(CorRFTest)
varImp(CorRFTest)

# Note Output #

# mtry  RMSE      Rsquared   MAE     
# 6     955.9153  0.7577637  499.9848

# PositiveServiceReview       100.000
# NegativeServiceReview        55.851
# ProductNum                   36.764
# ProductTypeGameConsole       30.874
# ProductDepth                 28.676
# ShippingWeight               27.233
# ProductHeight                25.421
# ProductTypePrinterSupplies   25.032
# ProductTypeExtendedWarranty  24.841
# ProductWidth                 22.229
# ProductTypePC                20.650
# ProductTypePrinter           20.605
# ProductTypeAccessories       18.205
# BestSellersRank              15.721
# Price                        14.200
# ProductTypeLaptop            10.049
# ProductTypeSoftware           9.561
# ProductTypeDisplay            8.857
# ProfitMargin                  5.759
# Recommendproduct              5.630


# SVM #

modelLookup('svmLinear')

# SVMoob #

set.seed(123)
SVMoobTest <- train(Volume~., data=oobTrain, method="svmLinear", importance=T, trControl=fitControl)
SVMGrid <- expand.grid(mtry=c(4,5,6))  

set.seed(123)
SVMoobTest <- train(Volume~.,data=oobTrain,method="svmLinear",
                      importance=T,
                      trControl=fitControl,
                      tuneGrid=SVMGrid)

plot(SVMoobTest)
print(SVMoobTest)
varImp(SVMoobTest)

VarImp <- varImp(SVMoobTest)
plot(VarImp)


# Note Output #

# RMSE      Rsquared   MAE     
# 170.9926  0.9279922  120.6896

# Overall
# x5StarReviews          100.0000
# x4StarReviews           93.5551
# PositiveServiceReview   82.7288
# x3StarReviews           69.6304
# x2StarReviews           67.6755
# NegativeServiceReview   34.2345
# x1StarReviews           25.1087
# ProductNum              17.0167
# ProductTypeGameConsole  15.6737
# ProductWidth             7.7594
# ProductDepth             7.0574
# ShippingWeight           6.9374
# ProductHeight            5.5053
# Price                    4.0041
# BestSellersRank          3.3656
# ProfitMargin             3.0095
# ProductTypePrinter       1.5856
# Recommendproduct         1.4820
# ProductTypeAccessories   1.0427
# ProductTypePC            0.9843

# SVM Cor #

set.seed(123)
SVMCorTest <- train(Volume~., data=CorTrain, method="svmLinear", importance=T, trControl=fitControl)
SVMGrid2 <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
SVMCorTest <- train(Volume~.,data=CorTrain,method="svmLinear",
                      importance=T,
                      trControl=fitControl,
                      tuneGrid=SVMGrid2)

plot(SVMCorTest)
print(SVMCorTest)
varImp(SVMCorTest)

# Note Output #

# RMSE      Rsquared   MAE     
# 1287.787  0.6346727  582.2205

# Overall
# PositiveServiceReview       100.0000
# NegativeServiceReview        41.3816
# ProductNum                   20.5692
# ProductTypeGameConsole       18.9459
# ProductWidth                  9.3793
# ProductDepth                  8.5307
# ShippingWeight                8.3857
# ProductHeight                 6.6547
# Price                         4.8400
# BestSellersRank               4.0683
# ProfitMargin                  3.6377
# ProductTypePrinter            1.9167
# Recommendproduct              1.7914
# ProductTypeAccessories        1.2603
# ProductTypePC                 1.1898
# ProductTypePrinterSupplies    1.1768
# ProductTypeExtendedWarranty   0.7086
# ProductTypeNetbook            0.7043
# ProductTypeSmartphone         0.4918
# ProductTypeTablet             0.4029

# Gradient Boosting #

modelLookup('xgbTree')

# oobGB #

set.seed(123)
GBoobTest <- train(Volume~., data=oobTrain, method="xgbTree", importance=T, trControl=fitControl)
GBGrid <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
GBoobTest <- train(Volume~.,data=oobTrain,method="xgbTree",
                      importance=T,
                      trControl=fitControl,
                      tuneGrid=GBGrid)

plot(GBoobTest)
print(GBoobTest)
varImp(GBoobTest)

# Note Output #

# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'min_child_weight' was held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 50, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.8,
# min_child_weight = 1 and subsample = 1.
# eta  max_depth  colsample_bytree  subsample  nrounds  RMSE      Rsquared   MAE     
# 0.3  3          0.8               1.00        50      655.9202  0.9393156  271.2886

# Overall
# x5StarReviews          1.000e+02
# x4StarReviews          2.877e+01
# x1StarReviews          8.149e-01
# PositiveServiceReview  1.912e-02
# BestSellersRank        1.862e-02
# Recommendproduct       7.035e-03
# x2StarReviews          5.801e-03
# ShippingWeight         4.083e-03
# ProfitMargin           3.521e-03
# Price                  2.920e-03
# ProductTypePrinter     2.842e-03
# ProductTypeSoftware    1.625e-03
# ProductNum             1.623e-03
# ProductTypeDisplay     8.365e-04
# ProductWidth           6.209e-04
# x3StarReviews          2.344e-04
# ProductTypeAccessories 2.305e-04
# ProductHeight          2.071e-04
# ProductTypeLaptop      1.751e-04
# NegativeServiceReview  5.833e-05

set.seed(123)
GBoobTest <- train(Volume~., data=oobTrain, method="xgbLinear", importance=T, trControl=fitControl)
GBGrid <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
GBoobTest <- train(Volume~.,data=oobTrain,method="xgbLinear",
                   importance=T,
                   trControl=fitControl,
                   tuneGrid=GBGrid)

plot(GBoobTest)
print(GBoobTest)
varImp(GBoobTest)

# GB Cor#

set.seed(123)
GBCorTest <- train(Volume~., data=CorTrain, method="xgbTree", importance=T, trControl=fitControl)
GBGrid2 <- expand.grid(mtry=c(4,5,6))  
set.seed(123)
GBCorTest <- train(Volume~.,data=CorTrain,method="xgbTree",
                      importance=T,
                      trControl=fitControl,
                      tuneGrid=GBGrid2)

plot(GBCorTest)
print(GBCorTest)
varImp(GBCorTest)

# Note Output #

# eta  max_depth  colsample_bytree  subsample  nrounds  RMSE       Rsquared   MAE     
# 0.3  3          0.6               1.00        50       938.0761  0.7233050  436.9814
#Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'min_child_weight' was held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 50, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.6,
# min_child_weight = 1 and subsample = 1.

# PositiveServiceReview      1.000e+02
# NegativeServiceReview      9.855e+01
# BestSellersRank            6.456e+01
# ProductWidth               1.104e+01
# ProductNum                 7.860e+00
# Price                      3.903e+00
# ProductTypeAccessories     9.849e-01
# ProductTypeGameConsole     7.513e-01
# ProductHeight              7.225e-01
# ProductDepth               5.789e-01
# ShippingWeight             4.768e-01
# ProfitMargin               2.776e-01
# ProductTypeLaptop          1.882e-01
# ProductTypePrinterSupplies 1.297e-01
# ProductTypePrinter         8.755e-02
# Recommendproduct           7.642e-02
# ProductTypeTablet          1.997e-02
# ProductTypeDisplay         9.740e-03
# ProductTypeSoftware        8.824e-03
# ProductTypeSmartphone      2.038e-03

###################
# Model selection #
###################

# OOB # 

oobSelection <- resamples(list(rf=oobRFtest, svm=SVMoobTest, gb=GBoobTest))
summary(oobSelection)

# Note output #

# Models: rf, svm, gb 
# Number of resamples: 5 
# 
# MAE 
# Min.     1st Qu.    Median     Mean  3rd Qu.      Max. NA's
# rf  98.53738815 130.6025011 198.05930 407.5403 276.1296 1334.3725    0
# svm  0.08629602   0.3242384 105.07262 120.6896 243.5865  254.3784    0
# gb  13.14354193  22.8635119  33.44185 271.2886  76.5429 1210.4513    0
# 
# RMSE 
#             Min.     1st Qu.    Median     Mean  3rd Qu.      Max. NA's
# rf  134.91108459 171.9546789 293.07904 893.7904 636.8308 3232.1763    0
# svm   0.09365472   0.6572659 138.22170 170.9926 293.4449  422.5454    0
# gb   18.03520406  29.3493961  63.12838 655.9202 143.3219 3025.7664    0
# 
# Rsquared 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5942952 0.7715275 0.8844639 0.8283552 0.9295867 0.9619027    0
# svm 0.8086411 0.8962882 0.9350315 0.9279922 1.0000000 1.0000000    0
# gb  0.7606745 0.9458520 0.9942706 0.9393156 0.9971362 0.9986446    0


# Repeat for additional ds #

CorSelection <- resamples(list(rf=CorRFTest, svm=SVMCorTest, gb=GBCorTest))
summary(CorSelection)

# Note Output #

# Models: rf, svm, gb 
# Number of resamples: 5 
# 
# MAE 
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
# rf  176.46859 240.0763 280.5082 499.9848 417.2263 1385.645    0
# svm 121.04242 316.7372 499.3210 582.2205 648.4878 1325.514    0
# gb   78.29565 230.9148 237.4095 436.9814 270.4565 1367.830    0
# 
# RMSE 
#         Min.  1st Qu.    Median      Mean   3rd Qu.     Max. NA's
# rf  228.2625 309.7515  330.8192  955.9153  596.3872 3314.356    0
# svm 172.8274 506.3285 1275.8999 1287.7867 1383.0654 3100.812    0
# gb  126.6957 379.1329  435.0513  938.0761  488.2252 3261.275    0
# 
# Rsquared 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5010201 0.7526842 0.7846995 0.7577637 0.8494708 0.9009440    0
# svm 0.2769810 0.5446199 0.6892478 0.6346727 0.7796435 0.8828714    0
# gb  0.4492171 0.6519895 0.7400778 0.7233050 0.8343563 0.9408846    0

### Save/load top performing model ###

saveRDS(oobSelection, "oobSelectionSVM.rds")  
SVMoob <- readRDS ("oobSelectionSVM.rds")

####################
# Model validation #
####################

SVMoobPred <- predict(SVMoobTest, oobTest)
postResample(SVMoobPred, oobTest$Volume)
plot(SVMoobPred, oobTest$Volume)

# Note Output #

# RMSE    Rsquared         MAE 
# 457.5004954   0.8389279 258.9646935 

####################
# Predict new data #
####################

SVMPredNew <- predict(SVMoobTest, DnewPA)
SVMPredNew
summary (SVMPredNew)
plot(SVMPredNew)

# Note Output #

# header #
#  1        2        3        4        5        6 
# 686.5056 391.6489 231.8516 125.7193 190.0588 103.7575 

# summary (SVMPredNew)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 63.49  186.53  373.77  970.57  730.75 6178.46 

output <- "newproductattributes.csv"
output$SVMoobPred <-SVMPredNew
write.csv(output, file="C3.T3output.csv", row.names = TRUE)


stopCluster(cl)

