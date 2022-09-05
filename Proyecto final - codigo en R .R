###########################  PROYECTO FINAL #####################################
#################### Kevin Jose Caceres Contreras  ##############################
####################  Wilfran Ballesteros Diaz   ################################
#################################################################################


#instalamos y llamamos a las diferentes librerias que necesitamos 
install.packages("ggplot2")
install.packages("readxl")
install.packages("corrplot")
library(ggplot2)
library(readxl)
library(corrplot)

#importamos el docuento de excel, le indicamos que hoja se necesita y que valores
#debe tomar y esto lo guardamos en una variable
calendario <- read_excel("LENGUAJES_ESTADISTICOS/Precios-área-y-producción-de-café-3.xlsx", 
                         sheet = "10. Valor cosecha", 
                         range = "C6:D27")

#cambiamos el nombre de las columnas para llevar un mejor manejo
colnames (calendario)<- c("año","valor")
View(calendario)


#filtramos todos los datos, en este caso dos decadas, se guarda en variables
dat <- filter(calendario,año >= "2000",año <="2020")
View(dat)

#calculamos el promedio de esta primera tabla se guarda en variable
media<- mean(calendario$valor)
media

#calculamos la desviacion estandar de la primera tabla, se guarda en variable

de <- sd(calendario$valor)
de

#aqui debimos importar el mismo excel, pero en este caso, escogimos la misma hoja
#pero indicado que datos en especial para la segunda tabla y lo guardamos en una
#nueva variable
cafetero <- read_excel("LENGUAJES_ESTADISTICOS/Precios-área-y-producción-de-café-3.xlsx",
                       sheet = "10. Valor cosecha", 
                       range = "F6:G27")


#cambiamos el nombre de las columnas
colnames (cafetero)<- c("año","valor")
View(cafetero) #para visualizar la base de datos

#filtramos los datos de esta tabala tambien y se guarda en una variable
dat1 <- filter(cafetero, año>="2000/1",año<="2020/21")
View(dat1)

#calculamos el promedio de esta tabala y se guarda en una variable
media1 <- mean(cafetero$valor)
media1

#calculamos la desviacion estandar, se guarda en una variable

de1 <- sd(cafetero$valor)
de1

#########################################################################

##2##

#creamos un diagrama para los datos del calendario normal
options(scipen = 999)
plot(calendario$año, calendario$valor, xlab="Años", ylab="Valor de la cosecha", 
     type="l", main="Valor de cosecha a traves de los años (Calendario)")

#creamos un diagrama para los datos del calendario cafetero
options(scipen = 999)

plot(cafetero$año, cafetero$valor, xlab="Años", ylab="Valor de la cosecha", 
     type="l", main="Valor de cosecha a traves de los años (Calendario Cafetero)")

###########################################################################



#diagrama de dispersion del calendario normal, guaramos en varible
grafica1 <- ggplot(calendario, aes(año,valor))
grafica1

grafica1 + geom_point() + geom_smooth(method = "lm", colour="Red")+ labs (title="Diagrama de dispersion(calendario normal)")

#diagrama de dispersion del calendario cafetero, guardamos en variable

grafica2 <- ggplot(cafetero, aes(año,valor))
grafica2

grafica2 + geom_point() + geom_smooth(method = "lm", colour="Blue")+labs(title="Diagrama de dispersion del calendario cafetero")


M<-cor(calendario,cafetero)
################################################################

#aqui aplicamos en t.test para realizar un analisis de ambas tablas y poder 
#sacar conclusiones

t.test(calendario$valor,cafetero$valor) #se le indica las bases de datos y sus respectivas columnas

##############################################################

#diagrama de correlacion entre ambas tablas

M<-cor(calendario,cafetero)# se guarda en una variable, con la palabra cor, se calcula la 
                           # la correlacion  y dentro se le indican las dos bases de datos

corrplot(M,method='number') #en esta parte lo que hacemos es graficar el diagrama anterior

