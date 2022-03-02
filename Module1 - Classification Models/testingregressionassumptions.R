#now do
#
getwd()
library(ggplot2)
library(dplyr)

heart.data <- read.csv("C:/Users/randa/Dropbox/Masters/Winter/TBANLT 560 Data Mining/Module1/heart.data.csv", header = TRUE)  # load data

cor(heart.data$biking, heart.data$smoking)

hist(heart.data$heart.disease)


plot(heart.disease ~ biking, data=heart.data)

plot(heart.disease ~ smoking, data=heart.data)

heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart.data)

summary(heart.disease.lm)


income.data <- read.csv("C:/Users/randa/Dropbox/Masters/Winter/TBANLT 560 Data Mining/Module1/income.data.csv", header = TRUE)  # load data
income.happiness.lm <- lm(happiness ~ income, data = income.data)

summary(income.happiness.lm)
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))


income.graph<-ggplot(income.data, aes(x=income, y=happiness))+
  geom_point()
income.graph

#add linear regression line to plotted data
income.graph <- income.graph + geom_smooth(method="lm", col="black")

income.graph

#add equation to regression line
income.graph <- income.graph +
  stat_regline_equation(label.x = 3, label.y = 7)

income.graph

#add some style parameters using theme_bw() and making custom labels using labs().

income.graph +
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")

