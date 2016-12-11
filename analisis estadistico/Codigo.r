# Importar las librerias a utilizar
library(MASS)
library(caTools)

# Directorio de trabajo
setwd("C:/users/Sergio_2/Desktop/master/ANALISIS ESTADISTICO/entregas/entrega 1")
# Importar CSV
house_train=read.csv("house_train.csv")

# Revision básica del dataset

str(house_train)
head(house_train)
summary(house_train)
# Vision gráfica de las variables precio y tamaño de la vivienda
hist(house_train$price)
boxplot(house_train$price)
points(mean(house_train$price), pch=3)
hist(house_train$sqft_living)
boxplot(house_train$sqft_living)
points(mean(house_train$sqft_living), pch=3)
# Cruce de las variables
plot(house_train$sqft_living,house_train$price)
abline(lm(house_train$price ~ house_train$sqft_living), col='red')
#modelo lineal
model_lm=lm(price~sqft_living, data=house_train)
summary(model_lm)
#residuos del modelo lm
par(mfrow=c(2,2))
plot(model_lm$residuals)
smoothScatter(model_lm$residuals)
hist(model_lm$residuals)
qqnorm(model_lm$residuals); qqline(model_lm$residuals,col=2)

# modelo lineal con transformacion logaritmica sobre el precio
model_lm_logPrice=lm(log(price)~sqft_living, data=house_train)
summary(model_lm_logPrice)
#residuos del modelo lm_logPrice
par(mfrow=c(2,2))
plot(model_lm_logPrice$residuals)
smoothScatter(model_lm_logPrice$residuals)
hist(model_lm_logPrice$residuals)
qqnorm(model_lm_logPrice$residuals); qqline(model_lm_logPrice$residuals,col=2)

# modelo lineal robusto con transformacion logaritmica sobre el precio
model_rlm_logPrice=rlm(log(price)~sqft_living, data=house_train)
summary(model_rlm_logPrice)
#residuos del modelo lm_logPrice
par(mfrow=c(2,2))
plot(model_rlm_logPrice$residuals)
smoothScatter(model_rlm_logPrice$residuals)
hist(model_rlm_logPrice$residuals)
qqnorm(model_rlm_logPrice$residuals); qqline(model_rlm_logPrice$residuals,col=2)

#intervalo de confianza al 95% para el model lm_logPrice
confint(model_lm_logPrice,level=0.95)

#####Parte 2#######
### Modelo predictivo ###
house_train$date= as.numeric(substr(house_train$date, 0, 8))

#Nos quedamos con las variables que conocemos su significado.
var_conocidas<-c("id","date","price","bedrooms", "bathrooms","sqft_living", "sqft_lot", "floors","waterfront", "view", "yr_built", "yr_renovated")
house_train<-house_train[var_conocidas]
#dividimos entre train y test

SAMPLE = sample.split(house_train$id, SplitRatio = 3/4)
Train = subset(house_train, SAMPLE == TRUE)

Test = subset(house_train, SAMPLE == FALSE)
#Calculamos la matriz de covarianzas
cor(Train)

#modelo lineal
model_lm=lm(price~sqft_living+bathrooms+view+bedrooms+waterfront+floors, data=Train)
summary (model_lm)
AIC(model_lm)
BIC(model_lm)
#modelo lineal con transformación logarítmica del precio
model_lm_log=lm(log(price)~sqft_living+bathrooms+view+bedrooms+waterfront+floors, data=Train)
summary (model_lm_log)
AIC(model_lm_log)
BIC(model_lm_log)

#modelo robusto
model_rlm=rlm(price~sqft_living+bathrooms+view+bedrooms+waterfront+floors, data=Train)
summary (model_rlm)
AIC(model_rlm)
BIC(model_rlm)

#modelo robusto con transformación logaritmica del precio
model_rlm_log=rlm(log(price)~sqft_living+bathrooms+view+bedrooms+waterfront+floors, data=Train)
summary (model_rlm_log)
AIC(model_rlm_log)
BIC(model_rlm_log)

#comparacion de modelos
tabla<-rbind(cbind("AIC",AIC(model_lm),AIC(model_lm_log),AIC(model_rlm),AIC(model_rlm_log)), cbind("BIC",BIC(model_lm),BIC(model_lm_log),BIC(model_rlm),BIC(model_rlm_log)))
colnames(tabla)<-c("Metrica", "model_lm","model_lm_log", "model_rlm","model_rlm_log")
tabla

#comparacion de los residuos de los modelos
par(mfrow=c(2,2))
plot(model_lm$residuals, main = "modelo lm")
plot(model_lm_log$residuals, main = "modelo lm_log")
plot(model_rlm$residuals, main = "modelo rlm")
plot(model_rlm_log$residuals, main = "modelo rlm_log")

par(mfrow=c(2,2))
hist(model_lm$residuals, main = "modelo lm")
hist(model_lm_log$residuals, main = "modelo lm_log")
hist(model_rlm$residuals, main = "modelo rlm")
hist(model_rlm_log$residuals,main = "modelo rlm_log")

par(mfrow=c(2,2))
qqnorm(model_lm$residuals,main = "modelo lm"); qqline(model_lm$residuals,col=2); 
qqnorm(model_lm_log$residuals,main = "modelo lm_log"); qqline(model_lm_log$residuals,col=2); 
qqnorm(model_rlm$residuals,main = "modelo rlm"); qqline(model_rlm$residuals,col=2); 
qqnorm(model_rlm_log$residuals,main = "modelo rlm_log"); qqline(model_rlm_log$residuals,col=2)

## -------------------------------------------------------------------------
# Evaluación de la prediccion
Train$prediccion_lmLog=predict(model_lm_log,type="response")
Test$prediccion_lmLog=predict(model_lm_log,newdata=Test,type="response")


Train$prediccion_rlmLog=predict(model_rlm_log,type="response")
Test$prediccion_rlmLog=predict(model_rlm_log,newdata=Test,type="response")


par(mfrow=c(2,2))
plot(Train$prediccion_lmLog,log(Train$price), main="Prediccion Vs observado en train; modelo lmLog"); abline(a=0,b=1)
plot(Test$prediccion_lmLog,log(Test$price), main="Prediccion Vs observado en test; modelo lmLog"); abline(a=0,b=1)
plot(Train$prediccion_rlmLog,log(Train$price),, main="Prediccion Vs observado en train; modelo rlmLog"); abline(a=0,b=1)
plot(Test$prediccion_rlmLog,log(Test$price),, main="Prediccion Vs observado en test; modelo rlmLog"); abline(a=0,b=1)

#Obtener los precios de venta de las viviendas sin precio

house_test=read.csv("house_test.csv")
house_test$price=exp(predict(model_lm_log,newdata=house_test,type="response"))
write.csv(house_test, "house_test_price.csv")