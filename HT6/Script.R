install.packages("e1071")
install.packages("dummies")
install.packages("caret")

library(caret)
library(dummies)
#library(dummy)
library(e1071)


datos <- read.csv('train.csv')

set.seed(123)

dum <- dummy(datos$SaleCondition)


datos <- cbind(datos, dummy(datos$SaleCondition, verbose = T))

corte <- sample(nrow(datos), nrow(datos)*0.7)











