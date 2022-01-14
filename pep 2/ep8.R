library(dplyr)
library ( tidyverse )
library ( ggpubr )
library (ez)
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
datos2 <- datos %>% select(Instancia,R, R2,R3,G)
datosL <- datos %>% pivot_longer (c("Optimo", "R", "R2",
                                   "R3","G"),
                                 names_to = "algoritmo", values_to = "tiempo")
#PREGUNTA 1
#Como solicitan verificar si existe un algoritmo eficiente en comparacion a los otros dados,
#de debe aplicar la prueba de Anova para muestras correlacionadas. Primero se verifican las condiciones
#de esta prueba:
#Condiciones:
#1.Se verifica, puesto que el tiempo, como toda magnitud física, 
#tiene una escala de intervalos iguales (de hecho tiene escala de razón).
#2.Como las mediciones dependen de cada algortimo y de cada entrada de datos que se dan, 
#se confirma la independencia entre al interior de cada grupo
#3.Se implementa la prueba de normalidad para verificar el supuesto:
g <- ggqqplot (datosL , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap (~ algoritmo )
print (g)
#Como es visible en el grafico, las muestras tienden a seguir una distribucion normal, sin embargo,
#poseen valores atipicos. Por lo que se prodece a implementa la prueba de shapiro para cada algoritmo.
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
#En las tres muestras se obtiene un p<alfa, por lo que no cumplen con una distribucion
#normal, y por lo tanto, no proceden de una poblacion normal. Sin embargo, dado que la prueba
#ANOVA es robusta, se permite seguir con el analisis de las muestras.
#4. Se procede aplicar la prueba de ANOVA, ya que es una prueba robusta para muestras del mismo
#tamaño.
#H0: Ningun algoritmo es mas eficiente que otro.
#H1: Existe al menos un algoritmo mas eficiente que otro
pruebaANOVa <- ezANOVA ( data = datosL , dv = tiempo , within = algoritmo ,
                       wid = Instancia , return_aov = TRUE )
print(summary(pruebaANOVa$aov))

#PREGUNTA 2
texto2 <- ("
words colors interfer
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
23 17 37
")
datos2 <- read.table(textConnection(texto2), header = TRUE)

datosL2 <- datos2 %>% pivot_longer (c("words", "colors", "interfer"),
                                  names_to = "tarea", values_to = "tiempo")

g2 <- ggqqplot (datosL2 , x = "tiempo", y = "tarea", color = "tarea")
g2 <- g2 + facet_wrap (~ tarea )
print (g2)

#H0: Si existen diferencias en los tiempos entre tareas
#HA: No existen diferencias en los tiempos entre tareas

pruebaANOVa2 <- ezANOVA ( data = datosL2 , dv = tiempo , within = tarea ,
                         wid = tarea , return_aov = TRUE )
print(summary(pruebaANOVa2$aov))
