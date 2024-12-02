# set working directory
setwd("C:/Users/HRowan1111/Desktop/stat215")

#install "haven" package
install.packages("haven")

#load "haven" package
library("readr")

# Load the data
stfinal <- read_delim("stfinal.csv.csv")

#create table for qaul1 and qual2
table(stfinal$anger, stfinal$crying)

#create a box plot for qual and quant
boxplot(stfinal$music, stfinal$humor)

#run anova test
anova <- aov(humor ~ music, data = stfinal)
summary(anova)

#create a scatter plot for quant1 and quant2
plot(stfinal$creature, stfinal$flashbacks)
linear_relationship <- lm(stfinal$creature ~ stfinal$flashbacks, stfinal = stfinal)
summary(linear_relationship)
abline(linear_relationship)

#add lines for means of x and y
meany <- mean(stfinal$creature)
meanx <- mean(stfinal$flashbacks)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")

#create residual plot for scatter plot
plot(stfinal$flashbacks, residuals(linear_relationship))
abline(h=0, col = "black")

#table 1 descriptive statistics
mean(stfinal$humor)
summary(stfinal$humor)
sd(stfinal$humor)

mean(stfinal$creature)
sd(stfinal$creature)
summary(stfinal$creature)

mean(stfinal$flashbacks)
sd(stfinal$flashbacks)
summary(stfinal$flashbacks)

table(stfinal$crying)
table(stfinal$anger)
table(stfinal$music)
