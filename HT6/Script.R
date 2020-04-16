install.packages("e1071")
install.packages("dummies")
install.packages("caret")
install.packages("nortest")
install.packages("corrplot")

library(caret)
library(dummies)
#library(dummy)
library(e1071)
library(nortest)
library(corrplot)


datos <- read.csv('train.csv')

set.seed(123)

datos <- cbind(datos, dummy(datos$SaleCondition, verbose = T))

corte <- sample(nrow(datos), nrow(datos)*0.7)

train <- datos[corte,]
test <- datos[-corte,]

#-------------Normalidad-------
matriz <- cor(train[,c(44, 47, 81)])
corrplot(matriz)

plot(datos$SaleCondition)

hist(datos$SalePrice)
hist(datos$X1stFlrSF)
hist(datos$GrLivArea)

shapiro.test(datos$SalePrice)
shapiro.test(datos$X1stFlrSF)
shapiro.test(datos$GrLivArea)

plot(density(datos$SalePrice))
plot(density(datos$X1stFlrSF))
plot(density(datos$GrLivArea))

#opcional
lillie.test(datos$SalePrice)
lillie.test(datos$X1stFlrSF)
lillie.test(datos$GrLivArea)





#------------------DESDE ACA TENGO DUDA----------------
modelo <- glm(datosNormal~., data = train[,c(44, 47, 81, 86)], family = binomial(), maxit=100)

plot(modelo)



pred <- predict(modelo, newdata = test[,c(44, 47, 81)], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datosNormal),as.factor(prediccion))
#------------------HASTA ACA------------------------------






