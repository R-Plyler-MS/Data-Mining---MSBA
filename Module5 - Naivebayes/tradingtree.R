#### Figure 9.7

library(rpart)
library(rpart.plot)
trading.df <- read.csv("trading2.csv")

head(trading.df)
# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
tree <- rpart(trading.return~., trading.df, method = "class", minsplit=2, minbucket=1)
tree
class.tree <- rpart(trading.return ~ ., data = trading.df, 
                    #control = rpart.control(maxdepth = 2), 
                    method = "class")
rpart.rules(tree)
## plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

class.tree
