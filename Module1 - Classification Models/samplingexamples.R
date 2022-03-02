
library(forecast)


housing.df <- read.csv("C:/Users/randa/Dropbox/Masters/Winter/TBANLT 560 Data Mining/Module1/WestRoxbury.csv", header = TRUE)  # load data

dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation

selected.var <- c(1, 3:8, 11:13)
names(housing.df)
#different ways of creating the sample partitions
train.index <- sample(c(1:dim(housing.df)[1]), dim(housing.df)[1]*0.6)  
train.df <- housing.df[train.index, selected.var]
valid.df <- housing.df[-train.index, selected.var]

# Sample into 3 sets.
idx <- sample(seq(1, 3), size = nrow(housing.df), replace = TRUE, prob = c(.6, .2, .2))
housing.df$indx<-idx
head(housing.df)
train <- housing.df[housing.df$indx == 1,]
valid <- housing.df[housing.df$indx == 2,]
test <- housing.df[housing.df$indx== 3,]
head(train)
idxTrain <- sample(nrow(housing.df),as.integer(nrow(housing.df)*0.6))
idxNotTrain <- which(! 1:nrow(housing.df) %in% idxTrain )
idxVal <- sample(idxNotTrain,as.integer(length(idxNotTrain)*0.333))
idxTest <- idxNotTrain[which(! idxNotTrain %in% idxVal)]
# create the 3 data frames by collecting all columns from the appropriate rows 
train.idx.data <- housing.df[idxTrain, ]
valid.idx.data <- housing.df[idxVal, ]
test.idx.data <- housing.df[idxTest, ]
head(test.idx.data)
head(train.index)
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
head(train.data)
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(housing.df), train.rows) 

valid.data <- housing.df[valid.rows, ]
itrain.rows<-as.integer(train.rows)

# alternative code for validation (works only when row names are numeric): 
# collect all the columns without training row ID into validation set 
valid.data <- housing.df[-itrain.rows, ] # does not work in this case

## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), 
                     dim(housing.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows 
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]
names(train.data)
head(train.data[,-15])
