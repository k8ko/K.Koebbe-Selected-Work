# Title: Assessment of Electrical Sub-metering Power Management

# Last update: 22 Jan 2022

# File: Koebbe_C4T2.R

# Project name: UT DA 2021 / C4T2


#################
# Project Notes #
#################

# Summary:   

# Deliverables:

# 1) Smart energy usage: Modeling patterns of energy usage by time of day and day of the year in a typical residence  whose electrical system is monitored by multiple sub-meters.
# 2) Indoor locationing: Determining a person's physical position in a multi-building indoor space using wifi fingerprinting.

###############
# Housekeeping
###############

# Clear console: CTRL + L

rm(list = ls())
getwd()
setwd("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C4T2")
dir()

#################
# Load packages #
#################

install.packages("caret")
install.packages("tidyverse")
install.packages("doParallel") 
install.packages("RMySQL")
install.packages("lubridate")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")

library(caret)
library(tidyverse)
library(doParallel)    
library(RMySQL)
library(lubridate)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(forecast)


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

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
str(con)


# Load preprocessed dataset(s) #

read.csv("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C4T2\\combinedyeardata.csv", stringsAsFactors = FALSE)


#################
# Evaluate data #
#################

dbListTables(con)
dbListFields(con, 'yr_2006')
yr06 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_2006")
yr07 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_2007")
yr08 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_2008")
yr09 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_2009")
yr10 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_2010")

str(yr06)  
summary(yr06)
head(yr06)
tail(yr06)
anyNA(yr06)
anyDuplicated((yr06))

str(yr07)  
summary(yr07)
head(yr07)
tail(yr07)
anyNA(yr07)
anyDuplicated((yr07))

str(yr08)  
summary(yr08)
head(yr08)
tail(yr08)
anyNA(yr08)
anyDuplicated((yr08))

str(yr09)  
summary(yr09)
head(yr09)
tail(yr09)
anyNA(yr09)
anyDuplicated((yr09))

str(yr10)  
summary(yr10)
head(yr10)
tail(yr10)
anyNA(yr10)
anyDuplicated((yr10))

# Note output #

# > str(yr06)  
# 'data.frame':	21992 obs. of  5 variables:
#   $ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
# $ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
# $ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...

# > summary(yr06)
# Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3 
# Length:21992       Length:21992       Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
# Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
# Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 0.00  
# Mean   : 1.249   Mean   : 2.215   Mean   : 7.41  
# 3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.00  
# Max.   :77.000   Max.   :74.000   Max.   :20.00  

# > head(yr06)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2006-12-16 17:24:00              0              1             17
# 2 2006-12-16 17:25:00              0              1             16
# 3 2006-12-16 17:26:00              0              2             17
# 4 2006-12-16 17:27:00              0              1             17
# 5 2006-12-16 17:28:00              0              1             17
# 6 2006-12-16 17:29:00              0              2             17

# > tail(yr06)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 21987 2006-12-31 23:54:00              0              0              0
# 21988 2006-12-31 23:55:00              0              0              0
# 21989 2006-12-31 23:56:00              0              0              0
# 21990 2006-12-31 23:57:00              0              0              0
# 21991 2006-12-31 23:58:00              0              0              0
# 21992 2006-12-31 23:59:00              0              0              0

# > anyNA(yr06)
# [1] FALSE

# > anyDuplicated((yr06))
# [1] 0
# > 

#   > str(yr07)  
# 'data.frame':	521669 obs. of  5 variables:
#   $ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

# > summary(yr07)
# Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3  
# Length:521669      Length:521669      Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
# Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 0.000  
# Mean   : 1.232   Mean   : 1.638   Mean   : 5.795  
# 3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
# Max.   :78.000   Max.   :78.000   Max.   :20.000  

# > head(yr07)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2007-01-01 00:00:00              0              0              0
# 2 2007-01-01 00:01:00              0              0              0
# 3 2007-01-01 00:02:00              0              0              0
# 4 2007-01-01 00:03:00              0              0              0
# 5 2007-01-01 00:04:00              0              0              0
# 6 2007-01-01 00:05:00              0              0              0

# > tail(yr07)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 521664 2007-12-31 23:54:00              0              0             18
# 521665 2007-12-31 23:55:00              0              0             18
# 521666 2007-12-31 23:56:00              0              0             18
# 521667 2007-12-31 23:57:00              0              0             18
# 521668 2007-12-31 23:58:00              0              0             18
# 521669 2007-12-31 23:59:00              0              0             18

# > anyNA(yr07)
# [1] FALSE

# > anyDuplicated((yr07))
# [1] 0
# > 

# > str(yr08)  
# 'data.frame':	526905 obs. of  5 variables:
#   $ Date          : chr  "2008-01-01" "2008-01-01" "2008-01-01" "2008-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  18 18 18 18 18 17 18 18 18 18 ...

#> summary(yr08)
# Date               Time           Sub_metering_1  Sub_metering_2   Sub_metering_3  
# Length:526905      Length:526905      Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
# Class :character   Class :character   1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000  
# Mode  :character   Mode  :character   Median : 0.00   Median : 0.000   Median : 1.000  
# Mean   : 1.11   Mean   : 1.256   Mean   : 6.034  
# 3rd Qu.: 0.00   3rd Qu.: 1.000   3rd Qu.:17.000  
# Max.   :80.00   Max.   :76.000   Max.   :31.000  
#
#> head(yr08)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2008-01-01 00:00:00              0              0             18
# 2 2008-01-01 00:01:00              0              0             18
# 3 2008-01-01 00:02:00              0              0             18
# 4 2008-01-01 00:03:00              0              0             18
# 5 2008-01-01 00:04:00              0              0             18
# 6 2008-01-01 00:05:00              0              0             17
#
#> tail(yr08)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 526900 2008-12-31 23:54:00              0              0              0
# 526901 2008-12-31 23:55:00              0              0              0
# 526902 2008-12-31 23:56:00              0              0              0
# 526903 2008-12-31 23:57:00              0              0              0
# 526904 2008-12-31 23:58:00              0              0              0
# 526905 2008-12-31 23:59:00              0              0              0

# > anyNA(yr08)
# [1] FALSE

# > anyDuplicated((yr08))
# [1] 0
# > 
# > str(yr09)  
# 'data.frame':	521320 obs. of  5 variables:
#   $ Date          : chr  "2009-01-01" "2009-01-01" "2009-01-01" "2009-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

# > summary(yr09)
# Date               Time           Sub_metering_1   Sub_metering_2   Sub_metering_3  
# Length:521320      Length:521320      Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
# Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000  
# Mean   : 1.137   Mean   : 1.136   Mean   : 6.823  
# 3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:18.000  
# Max.   :82.000   Max.   :77.000   Max.   :31.000  

# > head(yr09)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2009-01-01 00:00:00              0              0              0
# 2 2009-01-01 00:01:00              0              0              0
# 3 2009-01-01 00:02:00              0              0              0
# 4 2009-01-01 00:03:00              0              0              0
# 5 2009-01-01 00:04:00              0              0              0
# 6 2009-01-01 00:05:00              0              0              0

# > tail(yr09)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 521315 2009-12-31 23:54:00              0              0             18
# 521316 2009-12-31 23:55:00              0              0             18
# 521317 2009-12-31 23:56:00              0              0             19
# 521318 2009-12-31 23:57:00              0              0             18
# 521319 2009-12-31 23:58:00              0              0             18
# 521320 2009-12-31 23:59:00              0              0             19

# > anyNA(yr09)
# [1] FALSE

# > anyDuplicated((yr09))
# [1] 0
# > 
#   > str(yr10)  
# 'data.frame':	457394 obs. of  5 variables:
#   $ Date          : chr  "2010-01-01" "2010-01-01" "2010-01-01" "2010-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  18 18 19 18 18 19 18 18 19 18 ...

# > summary(yr10)
# Date               Time           Sub_metering_1    Sub_metering_2   Sub_metering_3  
# Length:457394      Length:457394      Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000  
# Class :character   Class :character   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 1.000  
# Mode  :character   Mode  :character   Median : 0.0000   Median : 0.000   Median : 1.000  
# Mean   : 0.9875   Mean   : 1.102   Mean   : 7.244  
# 3rd Qu.: 0.0000   3rd Qu.: 1.000   3rd Qu.:18.000  
# Max.   :88.0000   Max.   :80.000   Max.   :31.000  

# > head(yr10)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2010-01-01 00:00:00              0              0             18
# 2 2010-01-01 00:01:00              0              0             18
# 3 2010-01-01 00:02:00              0              0             19
# 4 2010-01-01 00:03:00              0              0             18
# 5 2010-01-01 00:04:00              0              0             18
# 6 2010-01-01 00:05:00              0              0             19

# > tail(yr10)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 457389 2010-11-26 20:57:00              0              0              0
# 457390 2010-11-26 20:58:00              0              0              0
# 457391 2010-11-26 20:59:00              0              0              0
# 457392 2010-11-26 21:00:00              0              0              0
# 457393 2010-11-26 21:01:00              0              0              0
# 457394 2010-11-26 21:02:00              0              0              0

# > anyNA(yr10)
# [1] FALSE

# > anyDuplicated((yr10))
# [1] 0


#################
# Preprocessing #
#################

allyears <- bind_rows (yr06, yr07, yr08, yr09, yr10)

summary(allyears)
head(allyears)
tail(allyears)

# > summary(allyears)
# Date               Time               Sub_metering_1   Sub_metering_2   Sub_metering_3  
# Length:2049280     Length:2049280     Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
# Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000  
                                      # Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
                                      # 3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
                                      # Max.   :88.000   Max.   :80.000   Max.   :31.000  

# > head(allyears)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2006-12-16 17:24:00              0              1             17
# 2 2006-12-16 17:25:00              0              1             16
# 3 2006-12-16 17:26:00              0              2             17
# 4 2006-12-16 17:27:00              0              1             17
# 5 2006-12-16 17:28:00              0              1             17
# 6 2006-12-16 17:29:00              0              2             17

# > tail(allyears)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 2049275 2010-11-26 20:57:00              0              0              0
# 2049276 2010-11-26 20:58:00              0              0              0
# 2049277 2010-11-26 20:59:00              0              0              0
# 2049278 2010-11-26 21:00:00              0              0              0
# 2049279 2010-11-26 21:01:00              0              0              0
# 2049280 2010-11-26 21:02:00 

dtcomb <- cbind(allyears, paste(allyears$Date, allyears$Time), stringsAsFactors=FALSE)
head(dtcomb)

# > head(dtcomb)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3 paste(allyears$Date, allyears$Time)
# 1 2006-12-16 17:24:00              0              1             17                 2006-12-16 17:24:00
# 2 2006-12-16 17:25:00              0              1             16                 2006-12-16 17:25:00
# 3 2006-12-16 17:26:00              0              2             17                 2006-12-16 17:26:00
# 4 2006-12-16 17:27:00              0              1             17                 2006-12-16 17:27:00
# 5 2006-12-16 17:28:00              0              1             17                 2006-12-16 17:28:00
# 6 2006-12-16 17:29:00              0              2             17                 2006-12-16 17:29:00

colnames(dtcomb)[6] <-"DateTime"
head(dtcomb)

combdf <- dtcomb[,c(ncol(dtcomb), 1:(ncol(dtcomb)-1))]
head(combdf)

# > head(combdf)
#             DateTime       Date     Time      Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2006-12-16 17:24:00 2006-12-16 17:24:00              0              1             17
# 2 2006-12-16 17:25:00 2006-12-16 17:25:00              0              1             16
# 3 2006-12-16 17:26:00 2006-12-16 17:26:00              0              2             17
# 4 2006-12-16 17:27:00 2006-12-16 17:27:00              0              1             17
# 5 2006-12-16 17:28:00 2006-12-16 17:28:00              0              1             17
# 6 2006-12-16 17:29:00 2006-12-16 17:29:00              0              2             17

combdf$DateTime <- as.POSIXct(combdf$DateTime, "%Y/%m/%d %H:%M:%S")
attr(combdf$DateTime, "tzone") <- "Europe/Paris"
str(combdf)
head(combdf)

# > str(combdf)
# 'data.frame':	2049280 obs. of  6 variables:
#   $ DateTime      : POSIXct, format: "2006-12-16 18:24:00" "2006-12-16 18:25:00" "2006-12-16 18:26:00" "2006-12-16 18:27:00" ...
# $ Date          : chr  "2006-12-16" "2006-12-16" "2006-12-16" "2006-12-16" ...
# $ Time          : chr  "17:24:00" "17:25:00" "17:26:00" "17:27:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  1 1 2 1 1 2 1 1 1 2 ...
# $ Sub_metering_3: num  17 16 17 17 17 17 17 17 17 16 ...
# 
# > head(combdf)
#     DateTime              Date     Time       Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2006-12-16 18:24:00 2006-12-16 17:24:00              0              1             17
# 2 2006-12-16 18:25:00 2006-12-16 17:25:00              0              1             16
# 3 2006-12-16 18:26:00 2006-12-16 17:26:00              0              2             17
# 4 2006-12-16 18:27:00 2006-12-16 17:27:00              0              1             17
# 5 2006-12-16 18:28:00 2006-12-16 17:28:00              0              1             17
# 6 2006-12-16 18:29:00 2006-12-16 17:29:00              0              2             17

combdf$year <- year(combdf$DateTime)
combdf$quarter <- quarter(combdf$DateTime) 
combdf$month <- month(combdf$DateTime) 
combdf$week <- week(combdf$DateTime) 
combdf$day <- day(combdf$DateTime) 
combdf$hour <- hour(combdf$DateTime) 
combdf$minute <- minute(combdf$DateTime)
combdf$weekday <- wday(combdf$DateTime)

head(combdf)
#       DateTime       Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3 year quarter month week day
# 1 2006-12-16 18:24:00 2006-12-16 17:24:00              0              1             17 2006       4    12   50  16
# 2 2006-12-16 18:25:00 2006-12-16 17:25:00              0              1             16 2006       4    12   50  16
# 3 2006-12-16 18:26:00 2006-12-16 17:26:00              0              2             17 2006       4    12   50  16
# 4 2006-12-16 18:27:00 2006-12-16 17:27:00              0              1             17 2006       4    12   50  16
# 5 2006-12-16 18:28:00 2006-12-16 17:28:00              0              1             17 2006       4    12   50  16
# 6 2006-12-16 18:29:00 2006-12-16 17:29:00              0              2             17 2006       4    12   50  16

# hour minute weekday
# 1   18     24       7
# 2   18     25       7
# 3   18     26       7
# 4   18     27       7
# 5   18     28       7
# 6   18     29       7

tail(combdf)

colnames(combdf)[4] <-"Kitchen"
colnames(combdf)[5] <-"Laundry"
colnames(combdf)[6] <-"WHAC"
head(combdf)
summary(combdf)


Sub1Kit <- combdf[, -c(5:6)]
Sub2Lau <- combdf[, -c(4,6)]
Sub3whac <- combdf[, -c(4:5)]

summary(Sub1Kit)
summary(Sub2Lau)
summary(Sub3whac)

# > summary(Sub1Kit)
# DateTime                       Date               Time              Kitchen            year         quarter    
# Min.   :2006-12-16 18:24:00   Length:2049280     Length:2049280     Min.   : 0.000   Min.   :2006   Min.   :1.00  
# 1st Qu.:2007-12-10 06:37:45   Class :character   Class :character   1st Qu.: 0.000   1st Qu.:2007   1st Qu.:1.00  
# Median :2008-11-30 02:22:30   Mode  :character   Mode  :character   Median : 0.000   Median :2008   Median :2.00  
# Mean   :2008-12-02 01:59:44                                         Mean   : 1.122   Mean   :2008   Mean   :2.49  
# 3rd Qu.:2009-11-23 21:31:15                                         3rd Qu.: 0.000   3rd Qu.:2009   3rd Qu.:3.00  
# Max.   :2010-11-26 22:02:00                                         Max.   :88.000   Max.   :2010   Max.   :4.00  
# month             week            day             hour          minute        weekday 
# Min.   : 1.000   Min.   : 1.00   Min.   : 1.00   Min.   : 0.0   Min.   : 0.0   Min.   :1  
# 1st Qu.: 3.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:15.0   1st Qu.:2  
# Median : 6.000   Median :26.00   Median :16.00   Median :12.0   Median :30.0   Median :4  
# Mean   : 6.455   Mean   :26.29   Mean   :15.71   Mean   :11.5   Mean   :29.5   Mean   :4  
# 3rd Qu.: 9.000   3rd Qu.:39.00   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:45.0   3rd Qu.:6  
# Max.   :12.000   Max.   :53.00   Max.   :31.00   Max.   :23.0   Max.   :59.0   Max.   :7  
# > summary(Sub2Lau)
# DateTime                       Date               Time              Laundry            year         quarter    
# Min.   :2006-12-16 18:24:00   Length:2049280     Length:2049280     Min.   : 0.000   Min.   :2006   Min.   :1.00  
# 1st Qu.:2007-12-10 06:37:45   Class :character   Class :character   1st Qu.: 0.000   1st Qu.:2007   1st Qu.:1.00  
# Median :2008-11-30 02:22:30   Mode  :character   Mode  :character   Median : 0.000   Median :2008   Median :2.00  
# Mean   :2008-12-02 01:59:44                                         Mean   : 1.299   Mean   :2008   Mean   :2.49  
# 3rd Qu.:2009-11-23 21:31:15                                         3rd Qu.: 1.000   3rd Qu.:2009   3rd Qu.:3.00  
# Max.   :2010-11-26 22:02:00                                         Max.   :80.000   Max.   :2010   Max.   :4.00  
# month             week            day             hour          minute        weekday 
# Min.   : 1.000   Min.   : 1.00   Min.   : 1.00   Min.   : 0.0   Min.   : 0.0   Min.   :1  
# 1st Qu.: 3.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:15.0   1st Qu.:2  
# Median : 6.000   Median :26.00   Median :16.00   Median :12.0   Median :30.0   Median :4  
# Mean   : 6.455   Mean   :26.29   Mean   :15.71   Mean   :11.5   Mean   :29.5   Mean   :4  
# 3rd Qu.: 9.000   3rd Qu.:39.00   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:45.0   3rd Qu.:6  
# Max.   :12.000   Max.   :53.00   Max.   :31.00   Max.   :23.0   Max.   :59.0   Max.   :7  
# > summary(Sub3whac)
# DateTime                       Date               Time                WHAC             year         quarter    
# Min.   :2006-12-16 18:24:00   Length:2049280     Length:2049280     Min.   : 0.000   Min.   :2006   Min.   :1.00  
# 1st Qu.:2007-12-10 06:37:45   Class :character   Class :character   1st Qu.: 0.000   1st Qu.:2007   1st Qu.:1.00  
# Median :2008-11-30 02:22:30   Mode  :character   Mode  :character   Median : 1.000   Median :2008   Median :2.00  
# Mean   :2008-12-02 01:59:44                                         Mean   : 6.458   Mean   :2008   Mean   :2.49  
# 3rd Qu.:2009-11-23 21:31:15                                         3rd Qu.:17.000   3rd Qu.:2009   3rd Qu.:3.00  
# Max.   :2010-11-26 22:02:00                                         Max.   :31.000   Max.   :2010   Max.   :4.00  
# month             week            day             hour          minute        weekday 
# Min.   : 1.000   Min.   : 1.00   Min.   : 1.00   Min.   : 0.0   Min.   : 0.0   Min.   :1  
# 1st Qu.: 3.000   1st Qu.:13.00   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:15.0   1st Qu.:2  
# Median : 6.000   Median :26.00   Median :16.00   Median :12.0   Median :30.0   Median :4  
# Mean   : 6.455   Mean   :26.29   Mean   :15.71   Mean   :11.5   Mean   :29.5   Mean   :4  
# 3rd Qu.: 9.000   3rd Qu.:39.00   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:45.0   3rd Qu.:6  
# Max.   :12.000   Max.   :53.00   Max.   :31.00   Max.   :23.0   Max.   :59.0   Max.   :7  

combdf %>% filter(year == "2008")


#######
# EDA #
#######

summary(combdf)


hist(combdf$Laundry, main="Power Usage of Laundry Applicances", xlab="Usage in Wh")
hist(combdf$Kitchen, main="Power Usage of Kitchen Applicances", xlab="Usage in Wh")
hist(combdf$WHAC, main="Power Usage of Water Heater and AC", xlab="Usage in Wh")
plot(combdf$Laundry)
plot(combdf$Kitchen)
plot(combdf$WHAC)

plot(combdf$Laundry, combdf$DateTime)
plot(combdf$Kitchen, combdf$DateTime)
plot(combdf$WHAC, combdf$DateTime)

featurePlot(x = combdf [, 4:6], y = combdf$DateTime, plot = "scatter")

houseWeek <- filter(combdf, year == 2008 & week == 2)
plot(houseWeek$Kitchen)
plot(houseWeek$Laundry)
plot(houseWeek$WHAC)

houseDay <- filter(combdf, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Kitchen, type = 'scatter', mode = 'lines')
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Laundry, type = 'scatter', mode = 'lines')
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$WHAC, type = 'scatter', mode = 'lines')

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Laundry, name = 'Laundry', mode = 'lines') %>%
  add_trace(y = ~houseDay$WHAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# 10 minute frequency
houseDay10 <- filter(combdf, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Laundry, name = 'Laundry', mode = 'lines') %>%
  add_trace(y = ~houseDay10$WHAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

SumHol <- filter(combdf, year == 2007 & week == 28 & (hour == 1 | hour == 2 | hour == 3 |hour == 4 |hour == 5 |hour == 6 |hour == 7 | hour == 8 | hour == 9 | hour == 10 | hour == 11 | hour == 12 | hour == 14 | hour == 15 | hour == 16 | hour == 17 | hour == 18 | hour == 19 | hour == 20 | hour == 21 | hour == 22 | hour == 23 | hour == 24))
plot_ly(SumHol, x = ~SumHol$DateTime, y = ~SumHol$Kitchen, name = "Kitchen" , type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SumHol$Laundry, name = 'Laundry', mode = 'lines') %>%
  add_trace(y = ~SumHol$WHAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Second week of July 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) 

Fall <- filter(combdf, year == 2009 & month == 10 & (day == 1 | day == 2 | day == 3 |day == 4 |day == 5 |day == 6 |day == 7 | day == 8 | day == 9 | day == 10 | day == 11 | day == 12 | day == 14 | day == 15 | day == 16 | day == 17 | day == 18 | day == 19 | day == 20 | day == 21 | day == 22 | day == 23 | day == 24 | day == 25 | day == 26 | day == 27 | day == 28 | day == 29 | day == 30 | day == 31))
plot_ly(Fall, x = ~Fall$DateTime, y = ~Fall$Kitchen, name = "Kitchen" , type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Fall$Laundry, name = 'Laundry', mode = 'lines') %>%
  add_trace(y = ~Fall$WHAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) 

Fall1 <- filter(combdf, year == 2009 & month == 10 & (week == 40 | week == 41 | week == 42 | week == 43 | week == 44))
plot_ly(Fall1, x = ~Fall1$DateTime, y = ~Fall1$Kitchen, name = "Kitchen" , type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Fall1$Laundry, name = 'Laundry', mode = 'lines') %>%
  add_trace(y = ~Fall1$WHAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption October 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) 

# Consider creating pie chart of total usage by submeter.

#################
# Save datasets #
#################

write.csv(combdf, "combinedyeardata.csv", row.names = F)

###############
# Time Series #
###############

house070809weekly <- filter(combdf, weekday == 2 & hour == 20 & minute == 1)
house070809weekly

houseSunweekly <- filter(combdf, weekday == 7)
houseSunweekly

houseAnnual <- filter(combdf, hour == 16, minute == 1)
houseAnnual

tsSm3 <- ts(house070809weekly$WHAC, frequency=52, start=c(2007,1), end=c(2010))
autoplot(tsSm3, colour = 'turquoise4', xlab = "Time", ylab = "Watt Hours", main = "WHAC 2007-2009 Weekly on Mon at 2000")

tsSm2 <- ts(houseSunweekly$Laundry, frequency=52, start=c(2006,50), end=c(2010, 48))
autoplot(tsSm2, colour = 'blueviolet', xlab = "Time", ylab = "Watt Hours", main = "Laundry All Years Weekly on Sun")

tsSm1 <- ts(houseAnnual$Kitchen, frequency=52, start=c(2007,1), end=c(2009, 52))
autoplot(tsSm1, colour = 'deeppink4', xlab = "Time", ylab = "Watt Hours", main = "Kitchen at 1600 2007-2009")


# add monthly and quarterly data if time permits 


##############
#Forecasting #
##############

fitSM3 <- tslm(tsSm3 ~ trend + season) 
summary(fitSM3)
accuracy(fitSM3)

#                      ME     RMSE      MAE   MPE MAPE      MASE      ACF1
# Training set -3.39039e-16 5.701771 4.320098 NaN  Inf 0.7054593 0.1334294

# Residual standard error: 7.006 on 104 degrees of freedom
# Multiple R-squared:  0.3446,	Adjusted R-squared:  0.01688 
# F-statistic: 1.052 on 52 and 104 DF,  p-value: 0.4068


# Plot forecast

forecastfitSM3 <- forecast(fitSM3, h=20)
summary(forecastfitSM3)
plot(forecastfitSM3)

# Forecast with 80, 90 confidence

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time" , main = "WHAC")

# R2 = 0.119 / RMSE = 5.702

fitSM2 <- tslm(tsSm2 ~ trend + season) 
summary(fitSM2)
accuracy(fitSM2)
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(70,80))
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time" , main = "Laundry")

#                    ME    RMSE      MAE   MPE MAPE    MASE      ACF1
# Training set 4.4944e-16 5.76272 3.634279 NaN  Inf 1.09594 0.7900855

# Residual standard error: 6.681 on 154 degrees of freedom
# Multiple R-squared:  0.3099,	Adjusted R-squared:  0.07689 
# F-statistic:  1.33 on 52 and 154 DF,  p-value: 0.09349


# R2 =  0.096  / RMSE = 5.763

fitSM1 <- tslm(tsSm1 ~ trend + season) 
summary(fitSM1)
accuracy(fitSM1)
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(60,70))
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time" , main = "Kitchen")

#                       ME     RMSE      MAE MPE MAPE      MASE        ACF1
# Training set -3.520425e-17 3.752514 1.449704 NaN  Inf 0.8765653 -0.04289195
# Residual standard error: 5.359 on 51 degrees of freedom
# Multiple R-squared:  0.5085,	Adjusted R-squared:  0.00735 
# F-statistic: 1.015 on 52 and 51 DF,  p-value: 0.4796

# R2 =  0.259 / RMSE = 3.753

compchart <- data.frame (Submeter = c("Kitchen", "Laundry", "WHAC", "Kitchen", "Laundry", "WHAC"), Metric = c("R2", "R2", "R2", "RMSE", "RMSE", "RMSE"), Value = c(0.259, 0.096, 0.119, 3.753, 5.763, 5.702))                          
print(compchart)

chart <- ggplot(compchart, aes(x = Submeter, y = Value))+
  geom_col(aes(fill = Metric), width = 0.6)

chart

#################
# Decomposition #
#################

componentsSM3 <- decompose(tsSm3)
componentsSM2 <- decompose(tsSm2)
componentsSM1 <- decompose(tsSm1)
summary(componentsSM1)
summary(componentsSM3$seasonal)
summary(componentsSM2$seasonal)
summary(componentsSM1$seasonal)
summary(componentsSM3$trend)
summary(componentsSM2$trend)
summary(componentsSM1$trend)
summary(componentsSM3$random)
summary(componentsSM2$random)
summary(componentsSM1$random)
plot(componentsSM3)
plot(componentsSM2)
plot(componentsSM1)

compchart2 <- data.frame (Submeter = c("Kitchen", "Laundry", "WHAC", "Kitchen", "Laundry", "WHAC", "Kitchen", "Laundry", "WHAC"), Metric = c("Seasonal", "Seasonal", "Seasonal", "Trend", "Trend", "Trend", "Remainder", "Remainder", "Remainder"), Value = c(0.00, -0.022, 0.035, 0.559, 2.867, 3.377, -0.001, 0.281, -0.161))
print(compchart2)

chart2 <- ggplot(compchart2, aes(x = Submeter, y = Value))+
  geom_col(aes(fill = Metric), width = 0.6)

chart2

################
# Holt Winters #
################

#Remove seasonal components

tsSM3_Adjusted <- tsSm3 - componentsSM3$seasonal
autoplot(tsSM3_Adjusted)
plot(decompose(tsSM3_Adjusted))

#Apply HoltWinters Smoothing

tsSM3_HW <- HoltWinters(tsSM3_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW, ylim = c(0, 25))

#Forecast with smoothed data

tsSM3_HWfor <- forecast(tsSM3_HW, h=25)
plot(tsSM3_HWfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - WHAC")

#Forecast with reduced confidence levels

tsSM3_HWforC <- forecast(tsSM3_HW, h=25, level=c(10,25))

## Plot only the forecasted area

plot(tsSM3_HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - WHAC", start(2010))


tsSM2_Adjusted <- tsSm2 - componentsSM2$seasonal
autoplot(tsSM2_Adjusted)
plot(decompose(tsSM2_Adjusted))
tsSM2_HW <- HoltWinters(tsSM2_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW, ylim = c(0, 25))
tsSM2_HWfor <- forecast(tsSM2_HW, h=25)
plot(tsSM2_HWfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Laundry")
tsSM2_HWforC <- forecast(tsSM2_HW, h=25, level=c(10,25))
plot(tsSM2_HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Laundry", start(2010,49))

tsSM1_Adjusted <- tsSm1 - componentsSM1$seasonal
autoplot(tsSM1_Adjusted)
plot(decompose(tsSM1_Adjusted))
tsSM1_HW <- HoltWinters(tsSM1_Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW, ylim = c(0, 25))
tsSM1_HWfor <- forecast(tsSM1_HW, h=25)
plot(tsSM1_HWfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Kitchen")
tsSM1_HWforC <- forecast(tsSM1_HW, h=25, level=c(10,25))
plot(tsSM1_HWforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Kitchen", start(2010))
