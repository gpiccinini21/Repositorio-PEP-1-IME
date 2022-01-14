library(dplyr)
library ( tidyverse )
library ( ggpubr )
library(ggplot2)
#PREGUNTA 1
texto <- ("Instancia 'Tpo A6' 'Tpo B12'
'rat783' 273271 211192
'd1291' 788789 605288
'fl1400' 6088487 7888789
'nrw1379' 2766181 2928081
'pr2392' 58568 69171
'u1060' 304179 387673
'rat575' 2911117 2873000
'u1432' 30014 29200
'rl1889' 308089 208129
'pcb1173' 358788 425337
'rl1304' 122636 146527
'u1817' 302047 353892
'pr1002' 242068 302009
'fl1577' 219311 119389
'u574' 39017 47426
'u2152' 101093 111110
'd657' 49523 57617
'u724' 3107858 389588
'rl1323' 372305 488894
'vm1084' 1688789 768792
")
datos <- read.table(textConnection(texto), header = TRUE)
#Como el experimento emplea dos muestras a ser comparadas y adem?s solicitan verificar 
#que algoritmo gen?tico es m?s eficiente, se considera adecuada la implementaci?n de 
#la prueba de rangos Wilcoxon-Mann-Whitney. A continuaci?n se comprueban las condiciones para
#esta prueba:
#1. Dado que pertenece a un estudio realizado para un trabajo de t?tulo, se asume
#que las observaciones de las muestras son independientes.
#2. La escala de medici?n de las observaciones es continua.
#3. En vista de que nos piden determinar si un algoritmo m?s eficiente que otro, la
#escala empleada de ambas muestras es ordinal.

alfa <- 0.05
#Se prueba si ambas muestras se distribuyen de manera normal.
datosL <- datos %>% pivot_longer(c( "Tpo.A6","Tpo.B12"),
                                 names_to = "algoritmo", values_to ="tiempo")
g <- ggqqplot(datosL, x ="tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo )
print(g)
#Se definen las hip?tesis:
#H0: No hay diferencia en los tiempos de cada algoritmo (misma eficiencia)
#HA: El algoritmo A es m?s eficiente que el algoritmo B.
#El gr?fico demuestra que ambas muestras no poseen una distribuci?n normal, pero dada
#la naturaleza de la prueba, no es necesario.

prueba_P1 <- wilcox.test(datos[["Tpo.A6"]], datos[["Tpo.B12"]], alternative = "greater", paired =FALSE ,
                         conf.level = 1 - alfa )
print(prueba_P1)
#Por lo tanto, el resultado de la prueba Wilcoxon indica un p>alfa, concluyendo que no se 
#puede rechazar la hip?tesis nula en favor de la hip?tesis alternativa, por lo que
#no hay diferencias en los tiempos de ejecuci?n de cada algoritmo y ambos poseen la misma
#velocidad.
#PREGUNTA 2
#Prueba de suma de rangos de Wilcoxon
#Comparar las tasas de interés de 2 bancos por mes dentro de un periodo de un año
#Ver si las tasas se distribuyen de igual manera
#H0: Las tasas de ambos bancos se distribuyen de igual forma
#HA: Las tasas de ambos bancos de distribuyen de manera distinta

#PREGUNTA 3
#Una compañía de cosméticos hizo una prueba preliminar de su nueva crema quitamanchas, en que 30
#personas fueron separadas aleatoriamente en tres grupos de 10 voluntarios/as: uno de control, a quienes
#se les entregó una crema placebo (humectante solamente); otro que usaron la crema quitamanchas que la
#compañía comercializa actualmente; y el último que usaron el nuevo producto. A todos se les dijo que
#usaban la crema nueva de última generación. Dos personas del grupo de control y una del grupo con la
#La crema existente abandonó el estudio. Para el resto, se reportaron los siguientes números de manchas
#removidas al finalizar el tiempo de prueba: *diferencia entre media de manchas* 


texto <- ("
 Nueva Actual Control
 40 41 41
 35 8 42
 34 15 26
 74 18 10
 42 23 15
 30 24 11
 25 25 17
 31 33 12
 37 23 --
 55 -- --
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

Cantidad <- c(c(datos[["Nueva"]]),c(datos[["Actual"]]),c(datos[["Control"]]))

Manchas <- c(rep("Nueva", length(datos[["Nueva"]])),
             rep("Actual", length(datos[["Actual"]])),
             rep("Control", length(datos[["Control"]])))

Manchas <- factor(Manchas)

data2 <- data.frame(Cantidad,Manchas)

alfa <- 0.05

#Prueba de Kruskal - Wallis

prueba <- kruskal.test(Cantidad  ~  Manchas, data=data2)
print(prueba)

# Como valor p es 0.01228 mayor que nuestro nivel de significación 0.05, se rechaza la hipótesis nula a favor de la alternativa, es decir que con un 95%
# confianza, existe al menos una muestra la cual su media es distinta de las demás.

# Para ello se realiza? un procedimiento post - hoc el cual nos indican las diferencias significativas.

if(prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(data2$Cantidad ,
                                   data2$Manchas ,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  print(post_hoc)
}

#PREGUNTA 4
# La pandemia ha generado varias consecuencias y entre una de ella es el aumento de la 
# tasa de inter?s, se quiere saber visualizar el efecto que genera, mediante los precios
# de productos, para verificar si hay un aumento equitativo en los valores en porcentajes
#de cada banco seg?n el servicio que disponen a sus clientes.
#H0: todos los servicios aumentaron su precio en cantidades similares.
#HA: Al menos un banco increment? el precio en su servicios de manera exorbitante.

#Servicio       BancoA    BancoB   BancoC
#Credito de      30         100      20
#consumo         
#Tarjeta
#de creditos     50          2       30
#Credito
#hipotecario     60          4       25
#Servicio n      ...        ...      ...

