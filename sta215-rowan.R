# set working directory #
setwd("C:/Users/HRowan1111/Desktop/stat215")

# install "haven" package #
install.packages("haven")

# load "haven" package #
library("readr")

# load raw data #
stfinal <- read_delim("stfinal.csv.csv")

########## table 1 descriptive statistics ##########
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

########## create a contingency table for QUAL1 and QUAL2 ##########
table(stfinal$anger, stfinal$crying)

# run chi-squared test for independence #
chisq.test(stfinal$anger, stfinal$crying)

########## create a box plot for QUAL and QUANT ##########
boxplot(stfinal$music, stfinal$humor)

# run anova test #
anova <- aov(humor ~ music, data = stfinal)
summary(anova)

########## create a scatter plot for QUANT1 and QUANT2 ##########
plot(stfinal$creature, stfinal$flashbacks)

# create linear relationship line between QUANT1 and QUANT2 #
linear_relationship <- lm(stfinal$creature ~ stfinal$flashbacks, stfinal = stfinal)
summary(linear_relationship)
abline(linear_relationship)

# add lines for means of x and y to confirm linear relationship line #
meany <- mean(stfinal$creature)
meanx <- mean(stfinal$flashbacks)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")

# create residual plot for scatter plot #
plot(stfinal$flashbacks, residuals(linear_relationship))
abline(h=0, col = "black")

# highlighting outliers on residual plot #
  #18
fitted_value_18 <- fitted(linear_relationship)[18]
residual_value_18 <- residuals(linear_relationship)[18]
print(paste("Fitted Value (x) for point 18: ", fitted_value_18))
print(paste("Residual Value (y) for point 18: ", residual_value_18))
points(0.218474895795501, 9.7815251042045, 
       col = "red", pch = 17, cex = 1.5)
  #52
fitted_value_52 <- fitted(linear_relationship)[52]
residual_value_52 <- residuals(linear_relationship)[52]
print(paste("Fitted Value (x) for point 52: ", fitted_value_52))
print(paste("Residual Value (y) for point 52: ", residual_value_52))
points(0.2184748957955, 2.7815251042045, 
       col = "red", pch = 17, cex = 1.5)
  #56
fitted_value_56 <- fitted(linear_relationship)[56]
residual_value_56 <- residuals(linear_relationship)[56]
print(paste("Fitted Value (x) for point 56: ", fitted_value_56))
print(paste("Residual Value (y) for point 56: ", residual_value_56))
points(1.67250384821598, 2.32749615178402, 
       col = "red", pch = 17, cex = 1.5)
  #77
fitted_value_77 <- fitted(linear_relationship)[77]
residual_value_77 <- residuals(linear_relationship)[77]
print(paste("Fitted Value (x) for point 77: ", fitted_value_77))
print(paste("Residual Value (y) for point 77: ", residual_value_77))
points(0.2184748957955, 4.7815251042045, 
       col = "red", pch = 17, cex = 1.5)
  #92
fitted_value_92 <- fitted(linear_relationship)[92]
residual_value_92 <- residuals(linear_relationship)[92]
print(paste("Fitted Value (x) for point 92: ", fitted_value_92))
print(paste("Residual Value (y) for point 92: ", residual_value_92))
points(0.2184748957955, 5.7815251042045, 
       col = "red", pch = 17, cex = 1.5)
  #108
fitted_value_108 <- fitted(linear_relationship)[108]
residual_value_108 <- residuals(linear_relationship)[108]
print(paste("Fitted Value (x) for point 108: ", fitted_value_108))
print(paste("Residual Value (y) for point 108: ", residual_value_108))
points(2.64185648316297, 3.35814351683703, 
       col = "red", pch = 17, cex = 1.5)
  #116
fitted_value_116 <- fitted(linear_relationship)[116]
residual_value_116 <- residuals(linear_relationship)[116]
print(paste("Fitted Value (x) for point 116: ", fitted_value_116))
print(paste("Residual Value (y) for point 116: ", residual_value_116))
points(0.218474895795501, 12.7815251042045, 
       col = "red", pch = 17, cex = 1.5)
  #137
fitted_value_137 <- fitted(linear_relationship)[137]
residual_value_137 <- residuals(linear_relationship)[137]
print(paste("Fitted Value (x) for point 137: ", fitted_value_137))
print(paste("Residual Value (y) for point 137: ", residual_value_137))
points(4.58056175305695, -3.58056175305695, 
       col = "red", pch = 17, cex = 1.5)
  #140
fitted_value_140 <- fitted(linear_relationship)[140]
residual_value_140 <- residuals(linear_relationship)[140]
print(paste("Fitted Value (x) for point 140: ", fitted_value_140))
print(paste("Residual Value (y) for point 140: ", residual_value_140))
points(7.00394334042443, 2.99605665957557, 
       col = "red", pch = 17, cex = 1.5)
