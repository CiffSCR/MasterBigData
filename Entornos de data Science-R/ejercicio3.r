library(ggplot2)
library(dplyr)
### Reproducir plot


### Ejercicio dplyr
filter(diamonds, cut=="Ideal")
select(diamonds, contains("c"))
mutate(diamonds, precio_quilate=price/carat)
group_by(diamonds,color)
diamonds%>%mutate(precio_quilate=price/carat)%>%group_by(color)%>%summarise(avg = mean(precio_quilate))
diamonds%>%mutate(precio_quilate=price/carat)%>%arrange(desc(precio_quilate))