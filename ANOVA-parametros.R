
#########################################################
###                                                   ###
###     Análisis de Varianza de Una Vía - ANOVA       ###
###                                                   ###                                              
###     Script elaborado por:  @wak4n                 ### 
###     Dudas y comentarios: wak4n@riseup.net         ###
###                                                   ###
#########################################################

# Si solo quieres ver los resultados pasa a la línea: 558



# En el siguiente ejercicio se analizará la variación de algunos 
# parámetros que fueron tomados en dos sitios en una sección de un 
# arroyo; la base de datos también incluye variables categóricas para analizar
# la variación de los parámetros en relacion a estas.

# Para saber a que se refiere cada cabezal de la base de datos y las unidades de cada valor
# ver la siguiente descripción:


### VARIABLES CATEGÓRICAS ### Contienen un número finito de categorías o grupos distintos: tratamientos, 
                          ### sitios, temporadas, tipo de material, etc.

# SITIO           (solo hace referencia a una de las dos secciones del río donde se tomaron los parámetros)
# UBICACIÓN       (si los parámetros fueron tomados río arriba o río abajo)
# TEMPORADA       (parámetros durante la temporada de secas o lluvias)


### VARIABLES CONTÍNUAS ### Son medidas en una escala contínua, son variables numéricas que tienen un número
                        ### infinito de valores: 

# ID              (solo es un número consecutivo que sirve para control de los datos en campo pero no para analizar)
# PROF2           (profundidad del agua medida en cm en el centro del arroyo)
# TEMP1           (temperatura del agua en °C medido con un oxímetro)
# FLUJO           (flujo del agua medido en m/s)


### VARIABLES EXPLICATIVAS  ###  También llamadas variables INDEPENDIENTES, factor o tratamiento; 
                            ###   es la variable que se manipula en un experimento para observar 
                            ###   su efecto en la variable DEPENDIENTE (respuesta)

## La variable EXPLICATIVA es la causa y la variable RESPUESTA es el efecto.


# EL ANOVA SE USA PARA EVALUAR LA VARIACIÓN DE UNA VARIABLE DEPENDIENTE CONTÍNUA (Y) A TRAVÉS DE NIVELES
# DE UNA O MÁS VARIABLES CATEGÓRICAS INDEPENDIENTES (X).


# Antes de comenzar vamos a instalar algunos paquetes que iremos utilizando:

install.packages("rapportools")
install.packages("nortest")
install.packages("ggplot2")
install.packages("car")
install.packages("tidyverse")

#####################
####              ###
####    INICIO    ###
####              ###
#####################


# Nuestra base de datos está almacenada en un archivo .csv que nombramos "parametros.csv"
# es importante que pongamos todos nuestros archivos (incluyendo este script) en una carpeta
# de la cual conozcamos la ruta donde está almacenada. Ojo: en R las rutas usan este slash: / y no este:\


# Establecemos nuestro directorio de trabajo insertando la ruta de nuestra carpeta 
# dentro de las comillas "C:/aqui/tu/ruta":

setwd("D:/02-BIBLIOTECA/Ciencia/Estadística/Scripts R/ANOVA")

# Enlistamos los archivos de nuestro directorio de trabajo:

list.files()

# Cargamos nuestra base de datos como un data frame que de ahora en adelante se llamará "parametros":

parametros <- read.csv("parametros.csv")

# Usando la función attach podemos acceder a los nombres de las variables (columnas) de un data frame
# sin tener que repetir el nombre del data frame:

attach(parametros)

# La función "names" nos devuelve los nombres de las variables contenidas en el data frame "parametros":

names(parametros)

# Si queremos conocer la clase de nuestras variables introducimos la función "class" y el nombre de la variable:


class(SITIO)
class(UBICACION)
class(TEMPORADA)


class(PROF2)
class(TEMP1)
class(FLUJO)


# Si vemos el resultado en la consola para SITIO, UBICACION y TEMPORADA tenemos "character" 
# mientras que para PROF2, TEMP1 y FLUJO "numeric"; 
# esto significa que las tres primeras variables son categóricas, y las restantes contínuas o numéricas.

# También podemos conocer los niveles de nuestra(s) variable(s) con la función "factor":

factor(SITIO)
factor(UBICACION)
factor(TEMPORADA)


# En este caso podemos ver que tenemos 2 niveles: SITIO 1 y SITIO 2; RIO ARRIBA y RIO ABAJO; LLUVIAS y SECAS.
# Podríamos hacerlo con las variables numéricas pero carece de sentido.

# Para iniciar a explorar nuestros datos podemos obtener un resumen de cada una de las variables:

summary(PROF2)
summary(TEMP1)
summary(FLUJO)

# De un conjunto en particular:

summary(PROF2, TEMP1, FLUJO)

# O de todas las variables contenidas en nuestra base de datos que hemos convertido en un dataframe y 
# nombramos "parmetros":

summary(parametros)

# Con esta funcion "summary" podemos tener estadísticas básicas para cada variable tales como el mínimo,
# primer cuartil, mediana, media, tercer cuartil y el máximo si son variables contínuas o la clase y 
# longitud si son categóricas.

# Podríamos calcular algunos estadísticos individualmente para cada variable:

# Como la media:

mean(PROF2)
mean(TEMP1)
mean(FLUJO)

###############################################################

# Incluso podemos calcular la media bajo condiciones con el paquete "dplyr" que viene en el "tidyverse", 
# por ejemplo:

library(tidyverse)


# Vamos a calcular la media de la temperatura por cada temporada:

temp1 = select(parametros, TEMPORADA, TEMP1)

temp1secas = filter(temp1, TEMPORADA == "SECAS") 

temp1lluvias = filter(temp1, TEMPORADA == "LLUVIAS")


# La media de la temperatura de la temporada de Secas es:

mean(temp1secas$TEMP1)

# La media de la temperatura de la temporada de lluvias es:

mean(temp1lluvias$TEMP1)



# De igual forma con el flujo:

flujo = select(parametros, TEMPORADA, FLUJO)

flujosecas = filter(flujo, TEMPORADA == "SECAS")

flujolluvias = filter(flujo, TEMPORADA == "LLUVIAS")

# La media de la temperatura de la temporada de secas es:

mean(flujosecas$FLUJO)

# La media de la temperatura de la temporada de lluvias es:

mean(flujolluvias$FLUJO)

###############################################################


# Podemos conocer la desviación estandar

sd(PROF2)
sd(TEMP1)
sd(FLUJO)

# O la varianza; para darnos cuenta de como se comportan nuestros datos:

var(PROF2)
var(TEMP1)
var(FLUJO)

# Si queremos leer la documentación de una función solo tenemos que poner el nombre de la función
# precedido del signo ? 

?mean

# o escribiendo la palabra help("nombre de la función")

help("mean")


# Así podemos ber en la ventana de la derecha que se nos despliega la ayuda y nos enseña en que paquete(s)
# está esa función. Los paquetes aparecen como hipervínculos que podemos seguir y leer sobre ellos e incluso
# se nos muestra el directorio donde está guardado ese paquete en nuestra computadora.


# Ahora vamos a cargar el paquete "rapportools":

library(rapportools)


# Con este paquete, vamos a construir una grafica de caja y bigote para la Profundidad del centro del arroyo
# PROF2 de acuerdo a la TEMPORADA.

boxplot(PROF2~TEMPORADA, ylab = "PROFUNDIDAD 2", xlab = "TEMPORADA", main="ANOVA", notch=F, outline=F, col="paleturquoise4", axes=T)

# También podemos modificar los parámetros de las etiquetas, el título, la muesca, los valores atípicos, el color y los ejes)

# Si observamos en el caso de la muesca/notch; los valores atípicos/outline o los ejes/axes: están seguidas de un signo = 
# y la letra T de true/verdadero o F false/falso para activar o desactivar esta característica.
# En el caso de los colores, se pueden poner los nombres entre comillas o los números, sin comillas.
# Aquí hay un PDF http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 
# con nombres de los colores, pero siempre se pueden buscar en google esos códigos.

# Recuerda: toda la documentación de la función "boxplot" se puede consultar 
# para usarla según nuestras necesidades:

?boxplot


# También podemos dejar el estilo por defecto de la función "boxplot" 

boxplot(TEMP1~TEMPORADA)
boxplot(FLUJO~TEMPORADA)


# A continuación, vamos a realizar el ANOVA propiamente y que se construya un dataframe que nombraremos
# "anovap2t" (Anova de la profundidad 2 por temporada) ; otro que se llame "anovatt" (Anova de la temperatura 1 por temporada)
# y otro que nombraremos "anovaft" (Anova del flujo por temporada):

anovap2t <- aov(PROF2~TEMPORADA)

anovatt <- aov(TEMP1~TEMPORADA)

anovaft <- aov(FLUJO~TEMPORADA)

# Vamos a pedir que se nos muestre la significancia para cada ANOVA:

summary(anovap2t)

summary(anovatt)

summary(anovaft)


############################################################### 
##                                                           ##
##                                                           ##
##   HIPOTESIS NULA                                          ##
##                                                           ##
##                 H0: Las medias son iguales      p > 0.05  ##
##                                                           ##
##   HIPÓTESIS ALTERNATIVA                                   ##
##                                                           ##
##                 H1: Las medias son diferentes   p < 0.05  ##
##                                                           ##
###############################################################



# PERO ..... PARA QUE NUESTRO ANÁLISIS SEA CORRECTO, DEBEMOS ATENDER ALGUNOS DETALLES:



#########################
####                  ###
####       ANOVA      ###
####                  ###
#########################


# Para poder realizar un análisis de varianza (ANOVA) es necesario que se cumplan algunos supuestos:

          # (1) La variable dependiente debe medirse al menos a nivel de intervalo.
          # (2) Independencia de las observaciones.
          # (3) La distribución de los residuales debe ser normal.
          # (4) Homocedasticidad: homogeneidad de las varianzas.


# Dado que los dos primeros supuestos deben cumplirse desde la toma de los datos,
# vamos a asumir que estos ya se cumpllen por lo que vamos a probar los dos restantes:


######
##################   LA RUTA DE TRABAJO ES LA SIGUIENTE:
#####


# (A) Por medio de la prueba de Shapiro-Wilk evaluamos si hay normalidad 
    
#    (a1) SI hay normalidad, evaluamos homocedasticidad (B)


#    (a2) NO hay normalidad, evaluamos homocedasticidad (B)  

# (B) La homocedasticidad es la homogeneidad de las varianzas

#     (b1) + SI hay normalidad y evaluamos homocedasticidad ("igualdad" de varianzas)  
#           + por Levene (mismo tamaño de grupos; sensible a la normalidad)
#           + o por Bartlet (Distintos tamaños de grupos; sensible a la normalidad)
#                           + Si hay normalidad y homocedastcidad entonces procedemos a (C)
#                           + SI no hay homocedasticidad procedemos a (D)

#     (b2) + NO hay normalidad y evaluamos homocedasticidad ("igualdad" de varianzas) 
#           + por Fligner (mismo tamaño de grupos-diseño balanceado NO sensible a la normalidad) 
#                 + si hay homocedasticidad (Fligner) entonces procedemos a (C)
#           
#               + NO hay normalidad ni homocedasticidad procedemos a (E)


# (C) ANOVA

# (D) WELCH

# (E) KRUSKAL-WALLIS: Es el análisis a realizar en caso de no haber normalidad ni homocedasticidad.


####################
#                  #
# ¡¡¡ ATENCIÓN !!! #
#                  #
####################

# Es muy importante que conozcamos si hay o no igualdad en el tamaño de grupos, porque esto determinará la mejor
# prueba a elegir. En este ejercicio, la variable TEMPORADA si tiene igualdad de grupos, 
# sin embargo SITIO y UBICACION no. Sin embargo se dice que la prueba de Flinger también tiene un buen desempeño,
# incluso cuando los diseños están desbalanceados.



######
##################   
#####


# AHORA COMPROBAREMOS LOS SUPUESTOS Y VEREMOS QUE TIPO DE ANÁLISIS SE AJUSTA MEJOR A NUESTROS DATOS:


#########################
####                  ###
####    NORMALIDAD    ###
####                  ###
#########################



## (3) para probar la normalidad de los datos vamos a hacerlo con el paquete "nortest" 
# que ya instalamos y vamos a llamar:

library(nortest)

######################################################################################
##                                                                                  ##
##   Normalidad:                                                                    ##
##                                                                                  ##
##   HIPOTESIS NULA                                                                 ##
##                                                                                  ##
##                 H0: Los datos se distribuyen de manera normal     p > 0.05       ##
##                                                                                  ##
##   HIPÓTESIS ALTERNATIVA                                                          ##
##                                                                                  ##
##                 H1: Los datos no se distribuyen de manera normal   p < 0.05      ##
##                                                                                  ##
######################################################################################


# En general, se plantea que la prueba de Shapiro-Wilk para evaluar la normalidad es
# es de las mas potentes y usadas:
# Para ver mas detalles sobre esta prueba se puede consultar el artículo https://doi.org/10.1093/biomet/52.3-4.591

# Ahora vamos a hacer esta prueba a todas nuestras variables continuas:


shapiro.test(PROF2)

shapiro.test(TEMP1)

shapiro.test(FLUJO)


# Así podemos ver que en ninguna de las variables el valor de p es mayor a 0.05 por lo que H0 se rechaza.

# Si quisieramos podríamos hacer un gráfico de densidad y ver como se comportan nuestras variables:

# Cargamos el paquete "ggplot2"

library(ggplot2)

# Y graficamos la variable FLUJO:

ggplot(parametros, aes(x=PROF2))+geom_density(color=3, lwd=1.5, linetype=8)

ggplot(parametros, aes(x=TEMP1))+geom_density(color=2, lwd=1.5, linetype=8)

ggplot(parametros, aes(x=FLUJO))+geom_density(color=4, lwd=1.5, linetype=8)

# Con lo que podemos observar que ninguno presenta la forma típica de la distribución normal.

# Sin embargo debemos probar si existe homocedasticidad o no para decidir si podemos 
# realizar un ANOVA o en su caso la prueba de KRUSKALL-WALLIS.



###############################
####                        ###
####    HOMOCEDASTICIDAD    ###
####                        ###
###############################


######################################################################################
##                                                                                  ##
##   Homocedasticidad:                                                              ##
##                                                                                  ##
##   HIPOTESIS NULA                                                                 ##
##                                                                                  ##
##                 H0: Las varianzas son homogenas     p > 0.05                     ##
##                                                                                  ##
##   HIPÓTESIS ALTERNATIVA                                                          ##
##                                                                                  ##
##                 H1: Las varianzas son heterogéneas   p < 0.05                    ##
##                                                                                  ##
######################################################################################

# Cargamos el paquete "car" 

library(car)

# Dependiendo de cada caso podemos realizar la prueba de:

#Levene

leveneTest(anovap2t)

leveneTest(anovatt)

leveneTest(anovaft)


#Bartlett

bartlett.test(parametros$PROF2, parametros$TEMPORADA)

bartlett.test(parametros$TEMP1, parametros$TEMPORADA)

bartlett.test(parametros$FLUJO, parametros$TEMPORADA)


#Fligner:

fligner.test(parametros$PROF2, parametros$TEMPORADA)

fligner.test(parametros$TEMP1, parametros$TEMPORADA)

fligner.test(parametros$FLUJO, parametros$TEMPORADA)



# EN NUESTRO CASO ELEGIMOS FLINGER DEBIDO A QUE NINGUNA DE LAS VARIABLES PRESENTA NORMALDIAD
# Y ENCONTRAMOS QUE LOS VALORES DE p SON:

# Profundidad por temporada: p = 0.8989

# Temperatura por temporada: p = 0.5667

# Flujo por temporada: p = 0.0002885

# Por lo tanto Profundidad y Temperatura presentan Homocedasticidad
# mientras que el Flujo presenta Heterocedasticidad.
# De ahí que para las dos primeras variables haremos la prueba de WELCH
# y para la tercera KRUSKALL-WALLIS.


###############################
####                        ###
####    WELCH               ###
####                        ###
###############################


oneway.test(parametros$PROF2 ~ parametros$TEMPORADA)

oneway.test(parametros$TEMP1 ~ parametros$TEMPORADA)



###############################
####                        ###
####    KRUSKAL-WALLIS      ###
####                        ###
###############################


############################################################### 
##                                                           ##
##                                                           ##
##   HIPOTESIS NULA                                          ##
##                                                           ##
##                 H0: Las medias son iguales      p > 0.05  ##
##                                                           ##
##   HIPÓTESIS ALTERNATIVA                                   ##
##                                                           ##
##                 H1: Las medias son diferentes   p < 0.05  ##
##                                                           ##
###############################################################



# Para esta prueba, que se aplica en caso de que los datos no sean normales, ni homocesdásticos
# utilizamos la función "kruskal.test" en nuestra variable numérica ~ y nuestra variable categórica:


kruskal.test(parametros$FLUJO ~ parametros$TEMPORADA)



#######################################################
####                                                ###
####          PRUEBA POST-HOC TUKEY                 ###
####                                                ###
#######################################################

# En el supuesto de que tuvieramos más de dos niveles dentro de nuestra variable
# podemos hacer la prueba de Tukey con el fin de  determinar qué medias difieren. 
# La prueba de rango post hoc, identifica subconjuntos homogéneos de medias que no se diferencian entre sí.


TukeyHSD(anovap2t)
TukeyHSD(anovatt)
TukeyHSD(anovaft)


####################################
####                             ###
####         CONCLUSIONES        ###
####                             ###
####################################


# Al comprar la profundiad del arroyo encontramos no hay diferencias entre lluvias y secas (F=2.0548, df=57, p=0.15)
# sin embargo la temperatura del agua fue diferente (F=5.9609, df=52, p=0.18) siendo mas caliente durante las secas (x=16.96) 
# que en lluvias (x=15.15). Finalmente al comparar el flujo, encontramos que existen diferencias significativas (p=7.174e-07)
# siendo mayor el flujo en lluvias (x=0.27) que en secas (x=0.05).


#x= al valor de la media, solo que no supe como poner el símbolo adecuado jejeje



#####################
####              ###
####    FIN       ###
####              ###
#####################

# SI QUIERES CONOCER MÁS DETALLES SOBRE EL ANÁLISIS DE VARIANZA Y/O R TE RECOMIENDO:

# The R book / Michael J. Crawley (2012)
# https://bit.ly/31qrI2j 



#________________________________________________________________________________________
#________________________________________________________________________________________
#________________________________________________________________________________________
#________________________________________████__████______________________________________
#______________________________________██▒▒░░██░░██______________________________________
#____________________________________██▒▒░░░░██░░░░██____________________________________
#____________________________________██▒▒░░██▒▒██░░██____________________________________
#____________________________________██▒▒░░██▒▒██░░██____________________________________
#__________________________________██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██__________________________________
#________________________________██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██__________________________________
#________________________________██▒▒▒▒▒▒▒▒▒▒▒▒██████▒▒██________________________________
#______________________________██▒▒▒▒▒▒▒▒▒▒████░░░░░░▒▒██________________________________
#______________________________██▒▒▒▒▒▒▒▒██░░░░░░░░░░██__________________________________
#______________________________██▒▒▒▒▒▒▒▒██░░░░░░██░░██__________________________________
#______________________________██▒▒▒▒▒▒████░░░░░░██░░░░████______________________________
#__________________________████▒▒██▒▒▒▒████░░░░░░░░░░░░░░░░██____________________________
#________________________██▒▒▒▒▒▒▒▒██▒▒▒▒██░░░░██░░░░░░░░░░██____________________________
#______________________██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░██████░░░░░░██______________________________
#____________________██▒▒▒▒▒▒██▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░██████________________________________
#____________________██▒▒▒▒██▒▒██▒▒▒▒▒▒████░░░░░░░░░░██__________________________________
#____________________██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒████████████____________________________________
#____________________██▒▒▒▒▒▒▒▒▒▒██▒▒▒▒▒▒██______________________________________________
#____________________██▒▒▒▒▒▒▒▒▒▒██▒▒▒▒▒▒██______________________________________________
#______________________██▒▒▒▒▒▒██████▒▒██████____________________________________________
#________________________██▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░██__________________________________________
#______________________██▒▒▒▒▒▒▒▒▒▒▒▒██░░░░░░██__________________________________________
#______________________████████████████████████__________________________________________
#________________________________________________________________________________________
#________________________________________________________________________________________

