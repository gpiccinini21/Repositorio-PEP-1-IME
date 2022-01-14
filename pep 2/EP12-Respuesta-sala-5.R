#Integrantes
#Nombre: Sofia Castro; RUT: 20.055.286-5
#Nombre: Felipe Cornejo; RUT: 20.427.782-6
#Nombre: Gianfranco Piccinini; Rut: 20.237.081-0
library(scatterplot3d)
library(dplyr)
library(ggpubr)
library(leaps)
library(car)
#PASO 1
set.seed(7782)

#PASO 2
datos <- read.csv("/Users/macbookair/Downloads/body.csv", sep="")
filtro_w <- datos %>% filter(Gender == 0)
muestra <- filtro_w[sample(nrow(filtro_w), 50),]

#PASO 3

header <- colnames(muestra)
#Se eliminan del header el cual se utilizar? para escoger 8 variables aleatorias
#Ya que de la muestra son todas mujeres y no se puede utilizar el peso para predecir el peso
header <- header[header != "Gender"];
header <- header[header != "Weight"];
header <- sample(header,8)

#PASO 4
#Se hará uso de la variable Wrists.diameter debido a multiples alusiones a sobre determinar obesidad
#en base a estas:
#https://www.biobiochile.cl/noticias/2014/11/28/como-calcular-el-peso-ideal-utilizando-la-medida-de-la-muneca.shtml
#https://vanguardia.com.mx/vida/cuanto-mide-tu-muneca-el-resultado-hablara-de-la-composicion-de-tu-cuerpo-KPVG3341029

#Junto con la observación de que gente muy flaca tiene las muñecas huesudas y gente sobre peso las tiene hinchadas.

#PASO 5
modelo_simple <- lm(Weight ~ Wrists.diameter, data=muestra)
print(summary(modelo_simple))

#Los datos deben presentar una relacion lineal
cat("\ nPrueba de normalidad para los residuos :\n")
print( shapiro.test(modelo_simple$residuals))
#Los residuos siguen una distribuci?n normal

#La variabilidad de los residuos debe ser aproximadamente constante.
cat(" Prueba de homocedasticidad para los residuos :\n")
print(ncvTest( modelo_simple ))

#Los residuos deben ser independientes entre sí.
cat("Prueba de Durbin - Watson para autocorrelaciones ")
cat("entre errores :\n")
print( durbinWatsonTest( modelo_simple ))

#Cada variable se relaciona linealmente con la respuesta.
p <- ggscatter( muestra , x = "Wrists.diameter", y = "Weight", color = "blue", fill = "blue",
                 xlab = "Diametro de la muñeca [cm]", ylab = "Peso [kg]")

p <- p + geom_smooth( method = lm , se = FALSE , colour = "red")
print(p)

coef_corre <- cor(muestra$Wrists.diameter, muestra$Weight)

#PASO 6
pre_extras <- sample(header,3)
# Incorporar 3 predictores obtenidos al azar.
m_1=muestra[pre_extras[1]]
m_2=muestra[pre_extras[2]]
m_3=muestra[pre_extras[3]]
modelo_6 <- update(modelo_simple , . ~ . + m_1[,1] +m_2[,1]+m_3[,1])
cat("=== Modelo con predictores agregados ===\ n")
print( modelo_6 )

#PASO 7
#Se evaluan los modelos
datos_7<- muestra[, c( "Shoulder.Girth", "Waist.Girth","Hip.Girth","Wrists.diameter" ,"Weight")][-1]
resultados <- data.frame( respuesta_predicha = fitted(modelo_6))
resultados[["residuos_estandarizados"]] <- rstandard(modelo_6)
resultados[["residuos_estudiantizados"]] <- rstudent(modelo_6)
resultados[["distancia_Cook"]] <- cooks.distance(modelo_6)
resultados[["dfbeta"]] <- dfbeta( modelo_6 )
resultados[["dffit"]] <- dffits( modelo_6 )
resultados[["apalancamiento"]] <- hatvalues( modelo_6 )
resultados[["covratio"]] <- covratio( modelo_6 )
sospechosos1 <- which(abs(resultados[["residuos_estandarizados"]]) > 1.96)
sospechosos2 <- which( resultados[[" cooks.distance "]] > 1)
apal_medio <- (ncol( datos_7 ) + 1) / nrow ( datos_7 )
sospechosos3 <- which( resultados[["apalancamiento"]] > 2*apal_medio )
sospechosos4 <- which(apply( resultados[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
inferior <- 1 - 3 * apal_medio
superior <- 1 + 3* apal_medio
sospechosos5 <- which(resultados[["covratio"]] < inferior |
                              resultados[["covratio"]] > superior )
sospechosos <- c( sospechosos1 , sospechosos2 , sospechosos3 ,
                 sospechosos4 , sospechosos5 )
sospechosos <- sort(unique ( sospechosos ))

cat("- Residuos estandarizados fuera del 95 % esperado :",
    sospechosos1 , "\n")
cat("- Residuos con una distancia de Cook alta :",
    sospechosos2 , "\n")
cat("- Residuos con apalancamiento fuera de rango :",
    sospechosos3 , "\n")
cat ("- Residuos con DFBeta >= 1:",sospechosos4 , "\n")
cat("- Residuos con razón de covarianza fuera de rango :",
    sospechosos5 , "\n")
cat(" Apalancamiento promedio :", apal_medio , "\n")
cat(" Intervalo razón de covarianza : [", inferior , "; ",
       superior , "]\n\n", sep = "")
print(round( resultados[ sospechosos , c("distancia_Cook", "apalancamiento",
                                            "covratio")], 3))

#PASO 8
#Se implementa validación cruzada.
n <- nrow(datos_7)
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos[ muestra , ]
prueba <- datos[-muestra,]
modelo_7 <- lm(Weight ~ Shoulder.Girth+Waist.Girth+Hip.Girth +Wrists.diameter , data = entrenamiento )
print( summary( modelo_7 ))
mse_entrenamiento <- mean(modelo_7$residuals ** 2)
cat("MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")            
predicciones <- predict(modelo_7, prueba )
# Calcular error cuadrado promedio para el conjunto de prueba .
error <- prueba[["Weight"]] - predicciones
mse_prueba <- mean( error ** 2)
cat("MSE para el conjunto de prueba :", mse_prueba )



                      