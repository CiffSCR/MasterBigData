# Cargamos los datos
library(ggplot2) # los datos se encuentran en la librería ggplot2
library(nortest)
library(Rcmdr)
dt<-as.data.frame(diamonds) #guardamos en un data frame los datos
head(dt) # comprobamos que se han cargado
attach(dt) # para poder utilizar las variables directamente
######Muestra representativa: Selecciona una muestra representativa para "cut"######
#Realizamos un muestreo estratificado tomando como estratos los distintos valores de la variable cut
#Sacamos los distintos valores de cut
niveles<-levels(dt$cut)
#realizamos un muestreo aleatorio simple en cada uno de los estratos de un 30% de la población
muestra<-c()
for (i in 1:length(niveles)){
   tam= round((dim(dt[cut==niveles[i],])[1])*0.3) # tamaño del estrato
   DStemp=dt[cut==niveles[i],] # data set temporal con un solo valor de Cut
   muestra<-rbind(muestra,DStemp[sample(dim(DStemp)[1],tam),])
}

# En la variable muestra tengo la muestra representativa de diamonds

######Análisis de las variables######
#### Análisis descriptivo de las variables: Tipo de variable, distribucion representacion
str(dt) # hace una descripcion de la variable en cuanto a su tipo
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':       53940 obs. of  10 variables:
# $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...          Variable numérica 
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...    Variable categorica con 5 categorias
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...  Variable categorica con 7 categorias
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ... Variable categorica con 8 categorias
# $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...          Variable numérica 
# $ table  : num  55 61 65 58 58 57 57 55 61 61 ...                              Variable numérica 
# $ price  : int  326 326 327 334 335 336 336 337 337 338 ...                    Variable numérica 
# $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...              Variable numérica 
# $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...          Variable numérica 
# $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...          Variable numérica 

#Vemos como se distribuyen los valores de cada variable.
summary(dt$carat)#por la funcion summary podemos suponer que hay muchos valores concentrados a la izquierda (el q1 y la mediana están relativamente cerca) y existe una cola hacia la derecha (la media es superior a la mediana)
hist(dt$carat)# en el histograma confirmamos la suposicion de la agrupación de datos a la izquierda y la cola a la derecha
boxplot(dt$carat) # en el boxplot vemos la concentración de datos con valores bajos y que hay muchos valores atípicos

summary(dt$cut) # se muestra el conteo de cada nivel de cut. Vemos que, segun el corte sea mejor, tenemos más individuos
counts <- table(dt$cut)
barplot(counts) # con el grafico de barras vemos visualmente la conclusion anterior. cuanto mejor sea el corte, mas individuos de ese corte tenemos


summary(dt$color) # la cantidad de individuos de cada color es similar excepto en los colores "D" "I" y "J". Sobre todo en este ultimo color, la diferencia es notable
counts <- table(dt$color)
barplot(counts) # confirmamos de forma visual la conclusión anterior

summary(dt$clarity) # vemos una distribucion desigual de individuos
counts <- table(dt$clarity)
barplot(counts) # observamos que los colores centrales son los que tienen más individuos

summary(dt$depth) # concentracion de valores en torno a la mediana( los cuartiles 1 y 3 estan muy proximos a ella) y con una cola a la izquierda y otra a la derecha
hist(dt$depth) 
boxplot(dt$depth) # con los gráficos confirmamos lo anterior, viendo que hay muchos valores atípicos a ambos lados. En el histograma vemos que la cola derecha es mas larga 

summary(dt$table) # observamos una distribución similar a la anterior. Concentracion de valores en el centro y dos colas
hist(dt$table)
boxplot(dt$table) # confirmamos que la distribucion de los valores se asemeja a la de la varia depth, aunque en este caso no hay tanta diferencia entre sus colas

summary(dt$price) #vemos que la mediana es inferior a la media, lo que indica una acumulación de valores a la izquierda. Tambien es destavable el valor máximo. mu lejano a la media. Esto idica una cola hacia la derecha
hist(dt$price)
boxplot(dt$price) # En los gráficos comfimmamos que hay mucho datos con valores bajos y se va disminuyendo la frecuencia según va creciendo el precio.

summary(dt$x) # vemos que hay datos con valor 0. Estos datos no tienen interés, puesto que no se ha medido la variable. 
dt2<-dt[dt$x>0,]#Eliminamos los datos con valor 0
summary(dt2$x) #Sin estos valores, vemos que la distribución de valores es, mas o menos, uniforme, aunque vemos un valor máximo bastante alejado
hist(dt2$x) 
boxplot(dt2$x) # los graficos muestran que los datos tienen dos colas, con la derecha más larga. Tambien se ven algunos valores atípicos en la cola derecha

summary(dt$y) # vemos que hay datos con valor 0. Estos datos no tienen interés, puesto que no se ha medido la variable. 
dt2<-dt[dt$y>0,]#Eliminamos los datos con valor 0
summary(dt2$y) #Sin estos valores, vemos que la distribución de valores es, mas o menos, uniforme, aunque vemos un valor máximo bastante alejado
hist(dt2$y)
boxplot(dt2$y)# los graficos muestran que los datos tienen dos colas, con la derecha más larga. Tambien se ven algunos valores atípicos en la cola derecha

summary(dt$z) # vemos que hay datos con valor 0. Estos datos no tienen interés, puesto que no se ha medido la variable. 
dt2<-dt[muestra$z>0,] #Eliminamos los datos con valor 0
summary(dt2$z) #Sin estos valores, vemos que la distribución de valores es, mas o menos, uniforme, aunque vemos un valor máximo bastante alejado
hist(dt2$z)
boxplot(dt2$z) # los graficos muestran que los datos tienen dos colas, con la derecha más larga. Tambien se ven algunos valores atípicos en la cola derecha

#### Detección de casos atípicos y su tratamiento
#Hemos visto que todas las variables poseen valores atípicos. Una posible solución es eliminarlos para que no influyan en los resultados de los diferentes análisis. Por otra parte, como son muchos, datos los que se salen de los valores normales, quizás es recomendable dejarlos, puesto que, aunque sean un porcentaje pequeño, son valores que nos vamos a encontrar.
#Voy a eliminar, al menos los que tienen valores nulos (0) en las variables "X", "Y" y "Z"
dt<-dt[dt$x>0 & dt$y>0 & dt$z>0,]
# Comprobamos la cantidad de outliers:
out_carat=c(quantile(dt$carat,.25)-1.5*IQR(dt$carat), quantile(dt$carat,.75)+1.5*IQR(dt$carat))
out_depth= c(quantile(dt$depth,.25)-1.5*IQR(dt$depth), quantile(dt$depth,.75)+1.5*IQR(dt$depth))
out_table= c(quantile(dt$table,.25)-1.5*IQR(dt$table), quantile(dt$table,.75)+1.5*IQR(dt$table))
out_price= c(quantile(dt$price,.25)-1.5*IQR(dt$price), quantile(dt$price,.75)+1.5*IQR(dt$price))
out_x = c(quantile(dt$x,.25)-1.5*IQR(dt$x), quantile(dt$x,.75)+1.5*IQR(dt$x))
out_y = c(quantile(dt$y,.25)-1.5*IQR(dt$y), quantile(dt$y,.75)+1.5*IQR(dt$y))
out_z = c(quantile(dt$z,.25)-1.5*IQR(dt$z), quantile(dt$z,.75)+1.5*IQR(dt$z))
out<- rbind(out_carat, out_depth, out_table, out_price, out_x, out_y, out_z)

dt_sinOut<-dt[dt$carat>out[1,1] & dt$carat<out[1,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$depth>out[2,1] & dt_sinOut$depth<out[2,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$table>out[3,1] & dt_sinOut$table<out[3,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$price>out[4,1] & dt_sinOut$price<out[4,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$x>out[5,1] & dt_sinOut$x<out[5,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$y>out[6,1] & dt_sinOut$y<out[6,2],]
dt_sinOut<-dt_sinOut[dt_sinOut$z>out[7,1] & dt_sinOut$z<out[7,2],]
#comprobamos el porcentaje de datos que hemos eliminado
porcent=(dim(dt_sinOut)[1]*100)/dim(dt)[1] # eliminando los outliers mantenemos el 90.23% de la poblacion. Seguiremos los análisis con este nuevo data set.
dt<-dt_sinOut
######Inferencia######
####  Calcula un intervalo de confianza para la media de "carat" y "depth"
#con la funcion t.test para una sola muestra, nos devuelve el intervalo de confianza. Por defecto el intervalo es del 95%
t.test(dt$carat)
#data:  muestra$carat
#t = 424.08, df = 48670, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
# 0.7073269 0.7138956
#sample estimates:
#mean of x 
#0.7106112 
#Vemos que el intervalor de confianza es de [0.7947883 0.8093220]

t.test(dt$depth)
#data:  muestra$depth
#t = 10845, df = 48670, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
# 61.68094 61.70324
#sample estimates:
#mean of x 
# 61.69209 

#### Formula un test de hipótesisis
tapply(dt$price, dt$cut,mean)
#     Fair      Good Very Good   Premium     Ideal 
# 3603.584  3216.468  3217.418  3497.122  2795.426 
# Vemos que la media del precio de la muestra que tenemos es muy parecida para los cortes "Good" y "Very Good". Vamos a hacer un contraste de hipótesis para ver si podemos afirmar esta igualdad. En teoría, "Very Good" al ser un corte mejor, debería tener un precio mayor. Vamos a hacer un contrate en el que la hipótesis nula sea la igualdad de medias y vermos si se puede rechazar o no esta hipótesis.
precioGood<-dt$price[dt$cut=="Good"]
precioVeryGood<-dt$price[dt$cut=="Very Good"]
# con un histograma vemos que no sigue una distribución normal
hist(precioGood)
hist(precioVeryGood)
# Ahora utilizamos el t.test para hacer el contrase entre las dos medias. El t.test es para datos que sigan una distribución normal, pero si el tamaño de la muestra es grande (mayor de 30) no es necesario que sigan dicha distribución:
t.test(precioGood, precioVeryGood,  alternative='two.sided')

#        Welch Two Sample t-test
#
#data:  precioGood and precioVeryGood
#t = -0.0203, df = 8043.546, p-value = 0.9838
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -92.59992  90.69902
#sample estimates:
#mean of x mean of y 
# 3216.468  3217.418 

# A la vista del P-valor obtenido, no podemos rechazar la hipótesis nula de igualdad de medias

######Relaciones entre las variables######

# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlación

#tenemos que pasar las variables categóricas a numéricas.
#Obtenemos los distintos niveles de cada una de ellas y los reemplazamos por números. Obtenemos los niveles ordenados de menor a mayor (de peor a mejor)
dt_recode<-dt
dt_recode[,2]<-sapply(dt_recode[,2],switch,"Fair"=1,"Good"=2,"Very Good"=3,"Premium"=4,"Ideal"=5)
dt_recode[,3]<-sapply(dt_recode[,3],switch,"J"=1,"I"=2,"H"=3,"G"=4,"'F'"=5,"E"=6,"D"=7)
dt_recode[,4]<-sapply(dt_recode[,4],switch,"IF"=1,"VVS1"=2,"VVS2"=3,"VS1"=4,"VS2"=5,"SI1"=6,"SI2"=7,"I1"=8)

head(dt_recode) #comprobamos que se han cambiado los valores
plot(dt_recode) #vemos un scatterplot entre todas las variables, dos a dos.
cor(dt_recode) # nos muestra el coeficiente de correlación entre todas las variables.

#              carat         cut       color     clarity        depth       table        price           x           y          z
#carat    1.00000000 -0.12725901  0.25565398 -0.35830341  0.035242073  0.18682902  0.922784405  0.98653913  0.98574891  0.9853223
#cut     -0.12725901  1.00000000 -0.01081142  0.16576169 -0.274081960 -0.40653369 -0.066394836 -0.10781531 -0.11172268 -0.1377845
#color    0.25565398 -0.01081142  1.00000000  0.05947226  0.044450708  0.01751371  0.138672543  0.23356707  0.23371027  0.2369094
#clarity -0.35830341  0.16576169  0.05947226  1.00000000 -0.066795105 -0.15740964 -0.144054306 -0.37264625 -0.36763744 -0.3749860
#depth    0.03524207 -0.27408196  0.04445071 -0.06679510  1.000000000 -0.24059716  0.005624437 -0.01112742 -0.01277283  0.0922298
#table    0.18682902 -0.40653369  0.01751371 -0.15740964 -0.240597157  1.00000000  0.136240038  0.18593231  0.18050787  0.1586113
#price    0.92278441 -0.06639484  0.13867254 -0.14405431  0.005624437  0.13624004  1.000000000  0.89942474  0.90057949  0.8961437
#x        0.98653913 -0.10781531  0.23356707 -0.37264625 -0.011127423  0.18593231  0.899424739  1.00000000  0.99853019  0.9933311
#y        0.98574891 -0.11172268  0.23371027 -0.36763744 -0.012772834  0.18050787  0.900579487  0.99853019  1.00000000  0.9931567
#z        0.98532227 -0.13778449  0.23690940 -0.37498598  0.092229796  0.15861132  0.896143738  0.99333108  0.99315669  1.0000000


#vamos a considerar que una correlación mayor de 0.7 (positiva o negativa) es una correlación alta.
# A la vista de la matriz de correlaciones, vemos que hay una fuerte correlacion positiva entre los pares de variables
# (carat,price), (carat, x), (carat,y), (carat,z), (price, x), (price, y), (price,z), (x, y), (x,z), (y,z)
# es lógico pensar que las variables que indican el tamaño (x, y, z) están relacionadas con la variable peso (carat)
# lo que observamos es que no hay corelación entre la calidad del corte, el color o la claridad y el precio.

# Ahora, mediante un análisis de la varianza (ANOVA) vamos a ver si el precio depende de las distintas categorias de Color.
# Para hacer un anova necesitamos que la variable respuesta (price) siga una distribución normal en cada categoría o bien, el cual es nuestro caso, que el tamaño de los datos sea grande. Tambien se necesita homogeniedad de la varainza en cada categoría. Esta última condicion la vamos a suponer para poder hacer el análisis.

an_o_va <-aov(dt_recode$price~dt_recode$cut)

summary(an_o_va)
#                 Df    Sum Sq   Mean Sq F value Pr(>F)    
#dt_recode$cut     1 1.574e+09 1.574e+09   210.2 <2e-16 ***
#Residuals     47481 3.556e+11 7.489e+06                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# El análisis de la varianza nos dice que hay diferencias significativas entre las medias de cada categoría.

######Análisis de regresión######

#Analizamos los gráficos de correlacion y los coeficientes de corelación
#tomamos el dataframe con lo valores recodificados para poder utilizar las variables categóricas como variables numéricas discretas. Esto se puede hacer porque todas las variables categóricas se pueden ordenar. Otra opción sería la de crear variables dummies de las distintas categorías. En nuestro caso se crearían demasiadas variables, puesto que hay muchas categorías.

plot(dt_recode)
partial.cor(dt_recode)

#Sacamos la matriz de correlaciones parciales. Estas correlaciones entre variables no tienen en cuenta la interrelacion con el resto de variables:
# como sólo nos interesa la relación con price, sólo pego esa fila:

#              carat         cut       color     clarity        depth       table        price           x           y           z
#price    0.60622390  0.06196948 -0.43503931  0.56725573  0.005237836 -0.04709215  1.000000000 -0.06873170  0.05150151 -0.04346383



# Nos interesa la correlacion de la variable precio con el resto de variables, ya que va a ser nuestra variable respuesta.

#Vemos que los valores más altos se encuentran en las variables carat, clarity (asociaciones positivas) y color (negativa)

#Vamos a ir creando el model de forma manual con las variables, es decir, vamos a seguir el método stepwise de forma manual.

# La máxima asociación con precio la vemos con la variable carat. Estudiamos ese modelo:

model1 <- lm(dt_recode$price~dt_recode$carat) 
summary(model1)
resm1<-model1$residuals
plot(resm1) # En los residuos vemos que no se distribuyen de forma aleatoria en torno al cero, por lo que el modelo no es bueno. Se ven dos zonas muy diferenciadas (alrededor del index 2100)
summary(resm1)
sd(resm1) # 1056.77

#vamos a añadir la siguiente variable con más correlacion parcial, clarity
model2 <- lm(dt_recode$price~dt_recode$carat+dt_recode$clarity) 
summary(model2)
resm2<-model2$residuals
plot(resm2) #seguimos viendo las dos zonas en los residuos, pero en la sgunda parte ya se ve un comportamiento más aleatorio y centrado en el cero
summary(resm2)
sd(resm2) #903.5127
#Con este model mejoramos el ajuste y disminuimos la varianza.

#Vamos a añadir la variable color que es la siguiente con una mayor correlacion parcial:
model3 <- lm(dt_recode$price~dt_recode$carat+dt_recode$clarity+dt_recode$color) 
summary(model3)
resm3<-model3$residuals
plot(resm3) #seguimos viendo las dos zonas en los residuos, de la misma forma que en el caso anterior.
summary(resm3)
sd(resm3) 
#disminuimos la varianza y mejoramos el ajuste, pero se sigue manteniendo la no aleatoriedad de los residuos.

#Vamos a probar con una transformación logarítmica del precio para ver si de esta forma conseguimos un modelo mejor.

caratLog<-log(dt_recode$carat)
priceLog<-log(dt_recode$price)

#Volvemos a hacer los modelos pero esta vez con la nueva variable como variable respuesta

model1_log <- lm(priceLog~caratLog) 
summary(model1_log)
resm1_log<-model1_log$residuals
plot(resm1_log) # El gráfico con los residuos muestra cierta falta de aleatoriedad pero menos que antes
summary(resm1_log)
sd(resm1_log) # 0.2483734

#Este modelo con una sola variable tiene un ajuste bastante bueno (Adjusted R-squared:  0.9279)
#Vamos a probar introduciendo una variable más para ver si mejoramos la distribución de los residuos
clarityLog<-log(dt_recode$clarity)
model2_log <- lm(priceLog~caratLog+clarityLog) 
summary(model2_log)
resm2_log<-model2_log$residuals
plot(resm2_log) #seguimos viendo las dos zonas en los residuos, pero en la sgunda parte ya se ve un comportamiento más aleatorio y centrado en el cero
summary(resm2_log)
sd(resm2_log) #0.1839627

#Este modelo tiene un ajuste muy bueno (Adjusted R-squared:  0.9605) y por el plot de residuos podemos suponer que éstos son aleatorios en torno al cero.
 