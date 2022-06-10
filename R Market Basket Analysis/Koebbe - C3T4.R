# Title: Market Basket Analysis

# Last update: 23 Jan 2022

# File: Koebbe - C3T4.R

# Project name: UT DA 2021 / C3T4


#################
# Project Notes #
#################

# Summary:   

# discover any interesting relationships (or associations) between customer's transactions 
# and the item(s) they've purchased.


###############
# Housekeeping
###############

# Clear console: CTRL + L

rm(list = ls())
getwd()
setwd("C:\\Users\\klkoe\\Documents\\School\\UT DA 2021\\C3T4")
dir()

#################
# Load packages #
#################

install.packages("arules")
install.packages("arulesViz")
install.packages("caret")

library(arules)
library(arulesViz)
library(caret)


###############
# Import data #
###############

## Load existing dataset ##

?read.transactions  #used for baskets instead of datasets

MBAdf <- read.transactions("ElectronidexTransactions2017.csv" , sep = "," , format = "basket")


#################
# Evaluate data #
#################

str(MBAdf)
summary(MBAdf)  
head(MBAdf)
tail(MBAdf)

# transactions as itemMatrix in sparse format with
# 9835 rows (elements/itemsets/transactions) and
# 125 columns (items) and a density of 0.03506172 
# 
# most frequent items:
#   iMac                HP Laptop CYBERPOWER Gamer Desktop            Apple Earpods        Apple MacBook Air 
# 2519                     1909                     1809                     1715                     1530 
# (Other) 
# 33622 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   25   26   27 
# 2 2163 1647 1294 1021  856  646  540  439  353  247  171  119   77   72   56   41   26   20   10   10   10    5    3    1    1    3 
# 29   30 
# 1    1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   4.383   6.000  30.000 
# 
# includes extended item information - examples:
#   labels
# 1 1TB Portable External Hard Drive
# 2 2TB Portable External Hard Drive
# 3                   3-Button Mouse


anyNA(MBAdf) #No#
anyDuplicated((MBAdf)) #Error: Only applies to vectors. 
#Warning message upon loading DS: In asMethod(object) : removing duplicated items in transactions"

inspect(MBAdf[1:20])
length (MBAdf) # Number of transactions
size (MBAdf[1:20]) # Number of items per transaction
LIST(MBAdf[1:20]) # Lists the transactions by conversion
itemLabels(MBAdf)# To see the item labels


######################
# EDA/Visualizations #
######################

?itemFrequencyPlot
itemFrequencyPlot(MBAdf, topN=10, horiz=TRUE)

?image()
image(MBAdf, 1:10)
image(sample(MBAdf, 98))
image(sample(MBAdf, 983))


######################
#  Apriori Algorithm #
######################

Rules1 <- apriori(MBAdf, parameter=list(supp = 0.2, conf=0.2, minlen=2))
inspect(Rules1)

Rules2<- apriori(MBAdf, parameter=list(supp = 0.1, conf=0.1, minlen=2))
inspect(Rules2)

Rules3<- apriori(MBAdf, parameter=list(supp = 0.05, conf=0.1, minlen=2))
inspect(Rules3)

# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
# 0.1    0.1    1 none FALSE            TRUE       5    0.05      2     10  rules TRUE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 491 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[125 item(s), 9835 transaction(s)] done [0.00s].
# sorting and recoding items ... [28 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 done [0.00s].
# writing ... [8 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].
# > inspect(Rules3)
# lhs                           rhs                        support    confidence coverage  lift     count
# [1] {Dell Desktop}             => {iMac}                     0.05460092 0.4074355  0.1340112 1.590762 537  
# [2] {iMac}                     => {Dell Desktop}             0.05460092 0.2131798  0.2561261 1.590762 537  
# [3] {CYBERPOWER Gamer Desktop} => {iMac}                     0.05673615 0.3084577  0.1839349 1.204320 558  
# [4] {iMac}                     => {CYBERPOWER Gamer Desktop} 0.05673615 0.2215165  0.2561261 1.204320 558  
# [5] {Lenovo Desktop Computer}  => {iMac}                     0.05876970 0.3969780  0.1480427 1.549932 578  
# [6] {iMac}                     => {Lenovo Desktop Computer}  0.05876970 0.2294561  0.2561261 1.549932 578  
# [7] {HP Laptop}                => {iMac}                     0.07554652 0.3892090  0.1941027 1.519599 743  
# [8] {iMac}                     => {HP Laptop}                0.07554652 0.2949583  0.2561261 1.519599 743  

Rules4<- apriori(MBAdf, parameter=list(supp = 0.05, conf=0.05, minlen=2))
inspect(Rules4)

# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
# 0.05    0.1    1 none FALSE            TRUE       5    0.05      2     10  rules TRUE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 491 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[125 item(s), 9835 transaction(s)] done [0.00s].
# sorting and recoding items ... [28 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 done [0.00s].
# writing ... [8 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].
# > inspect(Rules4)
# lhs                           rhs                        support    confidence coverage  lift     count
# [1] {Dell Desktop}             => {iMac}                     0.05460092 0.4074355  0.1340112 1.590762 537  
# [2] {iMac}                     => {Dell Desktop}             0.05460092 0.2131798  0.2561261 1.590762 537  
# [3] {CYBERPOWER Gamer Desktop} => {iMac}                     0.05673615 0.3084577  0.1839349 1.204320 558  
# [4] {iMac}                     => {CYBERPOWER Gamer Desktop} 0.05673615 0.2215165  0.2561261 1.204320 558  
# [5] {Lenovo Desktop Computer}  => {iMac}                     0.05876970 0.3969780  0.1480427 1.549932 578  
# [6] {iMac}                     => {Lenovo Desktop Computer}  0.05876970 0.2294561  0.2561261 1.549932 578  
# [7] {HP Laptop}                => {iMac}                     0.07554652 0.3892090  0.1941027 1.519599 743  
# [8] {iMac}                     => {HP Laptop}                0.07554652 0.2949583  0.2561261 1.519599 743  

##############
# Adjustments #
##############

summary(Rules3) # highest output #

SortRule3 <-sort(Rules3, by = "confidence")
inspect(SortRule3)

DellRules <- subset(Rules3, items %in% "Dell Desktop")
inspect(DellRules)

is.redundant(Rules3) #NO#

#######################
# Rules Visualization #
#######################

?plot

plot(Rules3, method="graph", shading="confidence", control=list(type=items), measureLabels=TRUE)
plot(Rules3, method="paracoord", reorder=TRUE, aplha=1, shading="confidence")
