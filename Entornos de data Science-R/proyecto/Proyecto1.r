#cargamos el fichero de datos. Tomamos con NA los valores -9999.0
diabetes<-read.delim("C:\\Users\\Sergio.Castro\\Desktop\\master\\master_casa\\Entornos de data Science-R\\entrega\\proyecto\\diabetes.data", na.strings="-9999.0")
diabetes<-diabetes[complete.cases(diabetes), ]
str(diabetes)
summary(diabetes)
boxplot(diabetes)
tapply(diabetes[,1],diabetes$SEX, mean)
