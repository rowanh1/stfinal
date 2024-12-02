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
table(stfinal$music)
boxplot(stfinal$music, stfinal$humor)

#create a scatter plot for quant1 and quant2
plot(stfinal$creature, stfinal$flashbacks)
abline(stfinal$creature, stfinal$flashbacks)
resid(stfinal$creature, stfinal$flashbacks)



plot(stfinal$humor, stfinal$creature)
residuals(stfinal$humor, stfinal$creature)