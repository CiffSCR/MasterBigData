# Cargamos los datos
library(ggplot2) # los datos se encuentran en la librería ggplot2
library(nortest)
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

#### Detecci󮠤e casos atcos y su tratamiento
#Hemos visto que todas las variables poseen valores atípicos. Una posible solución es eliminarlos para que no influyan en los resultados de los diferentes análisis. Por otra parte, como son muchos, datos los que se salen de los valores normales, quizás es recomendable dejarlos, puesto que, aunque sean un porcentaje pequeño, son valores que nos vamos a encontrar.
#Voy a eliminar, al menos los que tienen valores nulos (0) en las variables "X", "Y" y "Z"
dt<-dt[dt$x>0 & dt$y>0 & dt$z>0,]
# Comprobamos la cantidad de outliers:
out_carat=c(quantile(dt$carat,.25)-1.5*IQR(dt$carat), quantile(dt$carat,.75)+1.5*IQR(dt$carat))
out_depth= c(quantile(dt$carat,.25)-1.5*IQR(dt$depth), quantile(dt$depth,.75)+1.5*IQR(dt$depth))
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
> t.test(precioGood, precioVeryGood,  alternative='two.sided')

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
dt_recode<-dt[c('carat', 'depth', 'table', 'price', 'x', 'y', 'z')]
dt_recode['numCut']<-as.numeric(dt$cut)
dt_recode['numColor']<-as.numeric(dt$color)
dt_recode['numClarity']<-as.numeric(dt$clarity)

head(dt_recode) #comprobamos que se han cambiado los valores
plot(dt_recode) #vemos un scatterplot entre todas las variables, dos a dos.
corrplot(cor(dt_recode), method="number") # nos muestra el coeficiente de correlación entre todas las variables.
#                  carat        depth       table       price           x           y           z       numCut     numColor  numClarity
#carat       1.000000000  0.006071982  0.18870386  0.92222112  0.98610576  0.98539472  0.98472104 -0.129388900  0.255570212 -0.35962317
#depth       0.006071982  1.000000000 -0.30791702 -0.00738645 -0.04751424 -0.04799363  0.07047293 -0.133285953  0.043863218 -0.03556990
#table       0.188703857 -0.307917021  1.00000000  0.13485058  0.19385046  0.18841622  0.15497986 -0.425495930  0.014545049 -0.16135977
#price       0.922221117 -0.007386450  0.13485058  1.00000000  0.89780230  0.89910124  0.89574793 -0.066105007  0.139162243 -0.14359991
#x           0.986105761 -0.047514241  0.19385046  0.89780230  1.00000000  0.99847548  0.99151621 -0.116509777  0.232836668 -0.37496155
#y           0.985394719 -0.047993625  0.18841622  0.89910124  0.99847548  1.00000000  0.99150725 -0.120358894  0.232997879 -0.36986935
#z           0.984721044  0.070472929  0.15497986  0.89574793  0.99151621  0.99150725  1.00000000 -0.133770990  0.237474806 -0.37539492
#numCut     -0.129388900 -0.133285953 -0.42549593 -0.06610501 -0.11650978 -0.12035889 -0.13377099  1.000000000 -0.006563392  0.16607216
#numColor    0.255570212  0.043863218  0.01454505  0.13916224  0.23283667  0.23299788  0.23747481 -0.006563392  1.000000000  0.05903221
#numClarity -0.359623170 -0.035569899 -0.16135977 -0.14359991 -0.37496155 -0.36986935 -0.37539492  0.166072160  0.059032209  1.00000000

#vamos a considerar que una correlación mayor de 0.7 (positiva o negativa) es una correlación alta.
# A la vista de la matriz de correlaciones, vemos que hay una fuerte correlacion positiva entre los pares de variables
# (carat,price), (carat, x), (carat,y), (carat,z), (price, x), (price, y), (price,z), (x, y), (x,z), (y,z)
# es lógico pensar que las variables que indican el tamaño (x, y, z) están relacionadas con la variable peso (carat)
# lo que observamos es que no hay corelación entre la calidad del corte, el color o la claridad y el precio.


######Análisis de regresión######
