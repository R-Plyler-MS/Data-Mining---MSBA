##############################
## Chapter 11 -  guides Neural Nets ## 
##############################

#NOTE: based on R version 3.6.0

#set the working directory to appropriate folder on your machine, so as to access the data
#files.

#load the required libraries/packages for this chapter
#Install the package(s) below once on your machine. To do so, uncomment the 
#install.packages line(s) below.

#install.packages("caret")
#install.packages("neuralnet")
#install.packages("forecast")
#install.packages("gains")
library(caret)
library(neuralnet)
library(forecast)
library(gains)


## Problem 11.1 Credit Card Use. Consider the hypothetical bank data in 
##Table 11.7 on consumers' use of credit card credit facilities. Create a small
##worksheet in Excel to illustrate one pass through a simple neural network.

##TABLE 11.7 DATA FOR CREDIT CARD EXAMPLE AND VARIABLE DESCRIPTIONS
##   Years      Salary     Used Credit
##       4          43               0
##      18          65               1
##       1          53               0
##       3          95               0
##      15          88               1
##       6         112               1
##        Years: number of years the customer has been with the bank
##       Salary: customer's salary (in thousands of dollars)
##  Used Credit: 1 = customer has left an unpaid credit card balance at the 
##                   end of at least one month in the prior year, 
##               0 = balance was paid off at the end of each month

#An R solution follows, but the point of building a small spreadsheet is largely
#lost.
#first set up the spreadsheet and normalize the values
#determine the initial weights for the two nodes and theta
#the output from the node will be the sum of the bias plus the weights times the inputs
# in the sample spreadsheet provided weights are 0.05, 0.01, and theta value of -0.03
# the values for the first record (normalised) are -0.550057 and -1.24022 for years and salary
#inserting these into the formula for the logistic activation function
#=1/(1+EXP(-(-0.03+(0.05*0.55057+-1.24022*0.01))))
#= 0.4825
#now we need to determine the new weights
#first determine the error - from the text (page)
#errk = ^yk(1 􀀀 ^yk)(yk 􀀀 ^yk):
#the error in this case is =(0-0.4825)*0.4825*(1-0.4825) = -0.1205
#the new weights are calclulated from the error and previous weights
#so we get new weight =old-weight + decay-factor * error
# for year we get 0.0380 for salary we get -0.0020 and theta = 0.0420
#these weights are then used for the next record and this process continues
#until we have gone through the entire data set
#the spreadsheet 11-1.xlsx provides the first epoch
#Calculate the second epoch

##create a data frame from given data
data.df <- data.frame(Years=c(4,18,1,3,15,6), Salary=c(45,65,53,95,88,112),
                      Used_credit=c(0,1,0,0,1,1))
#has two outputs
data.df$Used_creditY <- ifelse(data.df$Used_credit == 1, 1, 0)
data.df$Used_creditN <- ifelse(data.df$Used_credit == 0, 1, 0)
head(data.df)
library(neuralnet)
#note data not normalized 
nn1 <- neuralnet(Used_creditY + Used_creditN ~ Years + Salary, 
                 data = data.df, linear.output = F, hidden = 3)
plot(nn1)


#############################

##Problem 11.3 Car Sales. Consider the data on used cars (ToyotaCorolla.csv) with
##1436 records and details on 38 attributes, including Price, Age, KM, HP, and
##other specifications. The goal is to predict the price of a used Toyota 
##Corolla based on its specifications. 

##11.3.a Fit a neural network model to the data. Use a single hidden layer with
##2 nodes.

#load the data and preprocess
toyota.df <- read.csv("C:/Users/sergi/Documents/dmclass/DMBAdata/ToyotaCorolla.csv")
head(toyota.df)
# preprocess
# convert categorical to numeric
# create two dummy variables
toyota.df$Fuel_Type_CNG <- 1* (toyota.df$Fuel_Type == "CNG")
toyota.df$Fuel_Type_Diesel <- 1* (toyota.df$Fuel_Type == "Diesel")
## partition  to create the training and validation

set.seed(1)
train.index <- sample(row.names(toyota.df), 0.6*dim(toyota.df)[1])  
valid.index <- setdiff(row.names(toyota.df), train.index)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[valid.index, ]

# normalize
library(caret)
norm.values <- preProcess(train.df, method="range")
# generate the normalized training and validation 
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

# run nn with a single hidden layer with 2 nodes
nn <- neuralnet(Price ~ Age_08_04+ 
                  KM+               
                  Fuel_Type_CNG+   
                  Fuel_Type_Diesel+
                  HP+     
                  Automatic+ 
                  Doors+
                  Quarterly_Tax+ 
                  Mfr_Guarantee+
                  Guarantee_Period+ 
                  Airco+            
                  Automatic_airco+ 
                  CD_Player+  
                  Powered_Windows+ 
                  Sport_Model+ 
                  Tow_Bar, 
                data = train.norm.df, linear.output = T, 
                hidden = 2)
plot(nn)
#use compute function for nn to get the prediction
training.prediction <- compute(nn, train.norm.df)
#examine the result matrix
nn$result.matrix

#predictions on validation data
validation.prediction=compute(nn, valid.norm.df)

table(unlist(validation.prediction))
plot.new()
hist(unlist(validation.prediction))


library(forecast)
#prediction error of training and validation data
accuracy(unlist(training.prediction), train.norm.df$Price)

#> accuracy(unlist(training.prediction), train.norm.df$Price)
#            ME  RMSE  MAE  MPE MAPE
#Test set -0.77 0.782 0.77 -Inf  Inf
accuracy(unlist(validation.prediction), valid.norm.df$Price)
#> accuracy(unlist(validation.prediction), valid.norm.df$Price)
#             ME  RMSE   MAE  MPE MAPE
#Test set -0.778 0.787 0.778 -475  475

# run the nn with a single hidden layer with 5 nodes
# replace the hidden = 2 to hidden =5
#carry out the same analysis as above
nn <- neuralnet(Price ~ Age_08_04+ 
                  KM+               
                  Fuel_Type_CNG+   
                  Fuel_Type_Diesel+
                  HP+     
                  Automatic+ 
                  Doors+
                  Quarterly_Tax+ 
                  Mfr_Guarantee+
                  Guarantee_Period+ 
                  Airco+            
                  Automatic_airco+ 
                  CD_Player+  
                  Powered_Windows+ 
                  Sport_Model+ 
                  Tow_Bar, 
                data = train.norm.df, linear.output = T, 
                hidden = 2) ##change to 5

#> accuracy with train
#            ME  RMSE  MAE  MPE MAPE
#Test set -0.77 0.782 0.77 -Inf  Inf
#> accuracy with validation
#             ME  RMSE   MAE  MPE MAPE
#Test set -0.778 0.787 0.778 -475  475

# run nn with two hidden layers with 5 nodes in each
# use hidden = c(5,5)


#> accuracy with training set
#            ME  RMSE  MAE  MPE MAPE
#Test set -0.77 0.782 0.77 -Inf  Inf
#> accuracy with validation
#             ME  RMSE   MAE  MPE MAPE
#Test set -0.778 0.787 0.778 -475  475

#i, ii, iii
#discuss thee outcomes
#############################
