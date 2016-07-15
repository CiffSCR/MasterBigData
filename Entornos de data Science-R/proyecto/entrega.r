library(ggplot2)
#cargamos el fichero de datos. Tomamos con NA los valores -9999.0
diabetes<-read.delim("C:\\Users\\Sergio_2\\Desktop\\master\\Entornos de data Science-R\\entrega\\proyecto\\diabetes.data", na.strings="-9999.0")
diabetes<-diabetes[complete.cases(diabetes),]
str(diabetes)
summary(diabetes)
boxplot(diabetes, col=c("red","green","blue","white","yellow","pink","orange","grey","purple","violet"),main="Representacion variables diabetes",ylab="valores")
tapply(diabetes[,1],diabetes$SEX, mean)

cor(diabetes[,-2],diabetes$Y)
plot(diabetes$Y, diabetes$BMI, type="p") #maxima correlacion positiva
plot(diabetes$Y, diabetes$S3, type="p") #maxima correlacion negativa
plot(diabetes$Y, diabetes$S2, type="p") #minima correlacion
plot(diabetes$Y, diabetes$Y, type="p") # correlacion =1
diabetes$SEX <- as.numeric(diabetes$SEX)
medianas<-lapply(diabetes,median)
mads<-lapply(diabetes, mad)
vec_out_inf<-c() #vamos a guardar los límites inferiores para considerar outliers
vec_out_sup<-c() #vamos a guardar los límites superiores para considerar outliers
for (i in 1:length(medianas)){
   vec_out_sup[i]<-medianas[[i]]+ (3*(mads[[i]]))
   vec_out_inf[i]<-medianas[[i]]- (3*(mads[[i]]))
   }
# Eliminamos los registros que tienen alguna variable con valor superior o inferior a los valores tomados como límite de outliers
diabetes_sin_out<-diabetes # data frame donde vamos a guardar los datos filtrados, sin ningún outlier
for (i in 1:length(diabetes_sin_out[1,])){
   if(mads[[i]]>0){  #Si el mad es 0, se eliminan todos los registros (excepto si vale 0)
   diabetes_sin_out<-subset(diabetes_sin_out, diabetes_sin_out[,i]>=vec_out_inf[i] & diabetes_sin_out[,i]<=vec_out_sup[i])
   }
}

porc_training<-floor(((dim(diabetes)[1])*70)/100)
posicion_training<-sample(c(1:dim(diabetes)[1]), porc_training)
training<-diabetes[posicion_training,]
test<-diabetes[-posicion_training,]
medias<-sapply(training,mean)
desv_tipica<-sapply(training,sd)
test_escala<-NULL
for(i in 1:dim(test)[2]){
   test_escala<-cbind(test_escala,(test[,i] - medias[[i]])/desv_tipica[[i]])
  }
colnames(test_escala)<-colnames(diabetes)
l_regr<-lm(Y~AGE+SEX+BMI+BP+S1+S2+S3+S4+S5+S6, data=training)

Y_esperado_training<- predict(l_regr)
error_cuad_medio_training<-(sum((training$Y-Y_esperado_training)^2))/dim(training)[1]

Y_esperado_test<-predict(l_regr, test)
error_cuad_medio_test<-(sum((test$Y-Y_esperado_test)^2))/dim(test)[1]
