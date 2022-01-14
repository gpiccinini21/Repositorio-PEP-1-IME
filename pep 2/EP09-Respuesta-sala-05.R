#Integrantes
#Nombre: Sofia Castro; RUT: 20.055.286-5
#Nombre: Felipe Cornejo; RUT: 20.427.782-6
#Nombre: Gianfranco Piccinini; Rut: 20.237.081-0
library(dplyr)
library ( tidyverse )
library ( ggpubr )
library (ez)
#PREGUNTA 1
texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 26 36.4 35.7 34.8 33
'brock400_4' 30 46.2 43.7 42.6 41
'C2000.9' 77 99.8 97.4 94.6 91
'c-fat500-10' 123 124 124 124 123
'hamming10-2' 509 677.8 601.9 598.6 509
'johnson32-2-4' 13 13 13 13 13
'keller6' 56 80.5 72 69.5 66.8
'MANN_a81' 1097 1114.8 1114.8 1114.8 1101.1
'p-hat1500-1' 9 14.1 12.9 12.1 11
'p-hat1500-3' 91 109.2 107.3 100.4 99
'san1000' 12 19.4 19.4 19.3 17
'san400_0.7_1' 37 57.4 56.5 56.5 56
'san400_0.9_1' 97 152.9 142.6 140.6 105
'frb100-40' 97 130.6 120.3 116.5 115
'frb59-26-1' 56 75.8 69.1 66.7 67
'1et.2048' 313 396.6 360.6 348.1 336.6
'1zc.4096' 376 481.2 461.8 447.6 426.5
'2dc.2048' 21 29.4 26.3 25.1 24
")
alfa <- 0.01
datos <- read.table(textConnection(texto), header = TRUE)
#Se  normalizan los datos, con el fin de implementar una distribución normal.
datos$Optimo <- scale(datos$Optimo)
datos$R <- scale(datos$R)
datos$R2 <- scale(datos$R2)
datos$R3 <- scale(datos$R3)
datos$G <- scale(datos$G)
datos2 <- datos %>% select(Instancia,R, R2,R3,G)
datosL <- datos %>% pivot_longer (c( "Optimo","R", "R2",
                                   "R3","G"),
                                 names_to = "algoritmo", values_to ="tiempo")
#Como solicitan verificar si existe un algoritmo eficiente en comparación a los otros dados,
#se debe aplicar la prueba de Anova para muestras correlacionadas. Primero se verifican las condiciones
#de esta prueba:
#Condiciones:
#1.Se verifica, puesto que el tiempo, como toda magnitud física, 
#tiene una escala de intervalos iguales.
#2.Como las mediciones dependen de cada algoritmo y de cada entrada de datos que se dan, 
#se confirma la independencia entre al interior de cada grupo
#3.Se implementa la prueba de normalidad para verificar el supuesto:
g <- ggqqplot (datosL, x ="tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap (~ algoritmo )
print(g)
#Como es visible en el gráfico, las muestras tienden a seguir una distribución normal, sin embargo,
#poseen valores atípicos. Por lo que se procede a implementar la prueba de shapiro para cada algoritmo.
shapiro1 <- shapiro.test(datos[["Optimo"]])
print(shapiro1)
shapiro2 <- shapiro.test(datos[["R"]])
print(shapiro2)
shapiro3 <- shapiro.test(datos[["R2"]])
print(shapiro3)
shapiro4 <- shapiro.test(datos[["R3"]])
print(shapiro4)
shapiro5 <- shapiro.test(datos[["G"]])
print(shapiro5)
#En las tres muestras se obtiene un p<alfa, por lo que no cumplen con una distribución
#normal, y por lo tanto, no proceden de una población normal. Sin embargo, dado que la prueba
#ANOVA es robusta, permite seguir con el análisis de las muestras.
#4. Se procede aplicar la prueba de ANOVA, ya que es una prueba robusta para muestras del mismo
#tamaño.
#H0: Ningún algoritmo es más eficiente que otro.
#H1: Existe al menos un algoritmo más eficiente que otro

pruebaANOV <- ezANOVA ( data = datosL , dv = tiempo , within = algoritmo ,
                       wid = Instancia , return_aov = TRUE )
print(pruebaANOV)
#La prueba de ANOVA entrega el valor de p<alfa, por lo que se rechaza la hipótesis nula
#en favor de la hipótesis alternativa, concluyendo que existe al menos un algoritmo más eficiente que otro.


#PREGUNTA 2
alfa<-0.05
texto_P2 <-(" words colors interfer
15 26 42
12 5 44
16 15 32
9 25 38
21 19 44
18 19 44
21 19 32
9 14 42
21 17 35
17 21 31
16 15 29
21 20 38
13 22 47
19 15 31
19 21 34
23 17 37")
datos_P2<- read.table(textConnection(texto_P2), header = TRUE)
instancia <- factor(1:16)
datos_P2<- datos_P2 %>%
  add_column(instancia = instancia,
             .before = "words") 
datosL_P2 <- datos_P2 %>% pivot_longer (c( "words","colors", "interfer"),
                                  names_to = "tareas", values_to ="tiempo")
#Como solicitan verificar si existen diferencias entres las tareas implementadas en el estudio,
#de debe aplicar la prueba de Anova para muestras correlacionadas. Primero se verifican las condiciones
#de esta prueba:
#Condiciones:
#1.Se verifica, puesto que el tiempo, como toda magnitud física, 
#tiene una escala de intervalos iguales.
#2.Como las mediciones dependen de cada tarea y de cada estudiante que realiza la tarea, 
#se confirma la independencia al interior de cada grupo
#3.Se implementa la prueba de normalidad para verificar el supuesto:
g_P2 <- ggqqplot (datosL_P2, x ="tiempo", y = "tareas", color = "tareas")
g_P2 <- g_P2 + facet_wrap (~ tareas )
print(g_P2)
#El  gráfico qq nos permite confirmar que las muestras poseen una distribución normal.
#4. Se procede aplicar la prueba de ANOVA, ya que es una prueba robusta para muestras del mismo
#tamaño.
#H0: No hay diferencias entre tareas.
#H1: Existe al menos una diferencia entre tareas.

pruebaANOV_P2 <- ezANOVA ( data = datosL_P2 , dv = tiempo , within = tareas ,
                        wid = instancia , return_aov = TRUE )
print(pruebaANOV_P2)
#La prueba de ANOVA entrega un valor de p>alfa, por lo que se puede puede rechazar la 
#hipótesis alternativa en favor de la hipótesis nula, concluyendo que no hay
#diferencias entre los tiempos de las tareas del estudio implementado.

