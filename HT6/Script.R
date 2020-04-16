install.packages("e1071")
install.packages("dummies")
install.packages("caret")
install.packages("")

library(caret)
library(dummies)
#library(dummy)
library(e1071)


datos <- read.csv('train.csv')

set.seed(123)

dum <- dummy(datos$SaleCondition)


datos <- cbind(datos, dummy(datos$SaleCondition, verbose = T))

corte <- sample(nrow(datos), nrow(datos)*0.7)

train <- datos[corte,]
test <- datos[-corte,]

#------------------DESDE ACA TENGO DUDA----------------
modelo <- glm(datosNormal~., data = train[,c(44, 47, 81, 86)], family = binomial(), maxit=100)



pred <- predict(modelo, newdata = test[,c(44, 47, 81)], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datosNormal),as.factor(prediccion))
#------------------HASTA ACA------------------------------






