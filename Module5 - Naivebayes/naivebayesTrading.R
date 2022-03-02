#### Table 8.4

library(e1071)

trading.df <- read.csv("trading2.csv")
head(trading.df)

str(trading.df)

# change numerical variables to categorical first
trading.df$past.trend <- factor(trading.df$past.trend )

trading.df$open.interest <- factor(trading.df$open.interest )

trading.df$trading.volume <- factor(trading.df$trading.volume )
trading.df$trading.return <- factor(trading.df$trading.return)


train.index <- sample(c(1:dim(trading.df)[1]), dim(trading.df)[1]*0.6)  
train.df <- trading.df[train.index, ]
valid.df <- trading.df[-train.index, ]
# run naive bayes
trading.nb <- naiveBayes(trading.return ~ ., data = trading.df)
trading.nb
print(trading.nb)

