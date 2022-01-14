# NOMBRES :
#Nombre: Sofia Castro; RUT: 20.055.286-5
#Nombre: Felipe Cornejo; RUT: 20.427.782-6
#Nombre: Gianfranco Piccinini; Rut: 20.237.081-0
library(pROC)
library(caret)
library(dplyr)
library(ggpubr)
library(scatterplot3d)
library(dplyr)
library(leaps)
library(car)
datos <- read.csv(choose.files(),sep = ";")
#Se propone utilizar la misma semilla que en la experiencia anterior, los ultimos 4 digitos del rut del menor del grupo T-T
set.seed(7782)
#Se calcula el IMC
imc <- datos[["Weight"]] / ((datos[["Height"]]/100)**2)
EN<-imc
EN[EN < 25.0] <- 0
EN[EN >= 25.0] <- 1
datos$EN <- EN
datos$EN <- factor(datos$EN)
datos$imc <- imc
datos$imc <- factor(datos$imc)

#Ocho predictores seleccionados anteriormente.
#filtro_w <- datos %>% filter(Gender == 0)
muestra <- datos[sample(nrow(datos), 50),]
header <- colnames(muestra)

# Se sacan las variables Weight, Height y Gender para que no aparezcan en la muestra. las primeras dos son dependientes de IMC y la tercera no aporta ninguna informaciÃ³n.
header <- header[header != "Gender"];
header <- header[header != "Weight"];
header <- header[header != "Height"];

header <- sample(header,8)
#Se escoge la variable Navel.Girth para predecir la clase EN, pues mediante su valor se puede suponer 
#las condiciones fisicas de una persona.

#Revisar condiciones para Navel.Girth

modelo_lineal <- glm(EN ~ Navel.Girth, family =binomial(link = "logit"), data=muestra)
print(modelo_lineal)
probs_e <- predict(modelo_lineal , muestra , type = "response")
ROC_p <- roc(muestra[["EN"]], probs_e )
plot(ROC_p)
#Se obtiene un coeficiente de correlación de 0.5648
modelo_ajustado <- train(EN ~ Navel.Girth, data = muestra , method = "glm",
                         family = binomial(link ="logit"),
                         trControl=trainControl( method="cv", number = 5,savePredictions =TRUE))      
print(summary(modelo_ajustado))

matriz <- confusionMatrix( modelo_ajustado$pred$pred , modelo_ajustado$pred$obs)
print(matriz)

pre_extras <- sample(header,3)
# Incorporar 3 predictores obtenidos al azar.
m_1=muestra[pre_extras[1]]
m_2=muestra[pre_extras[2]]
m_3=muestra[pre_extras[3]]
modelo_logistico <- update(modelo_lineal, . ~ . + m_1[,1] +m_2[,1]+m_3[,1])
cat("=== Modelo con predictores agregados ===\ n")
print(modelo_logistico)

# Verificación de multicolinealidad .
cat(" Verificación de colinealidad \n")
cat(" --------------------------------------\n")
cat("\ nVIF :\n")
vifs <- vif(modelo_logistico)
print( vifs )
cat("\ nPromedio VIF: ")
print( mean ( vifs ))
# Independencia de los residuos .
cat(" Verificación de independencia de los residuos \n")
cat(" --------------------------------------\n")
print(durbinWatsonTest( modelo_logistico , max.lag = 5))
# Detectar posibles valores atípicos .
cat("Identificación de posibles valores atípicos \n")
cat("--------------------------------------\n")
plot(modelo_logistico)
# Obtener los residuos y las estadísticas.
output <- data.frame( predicted.probabilities = fitted(modelo_logistico))
output[["standardized.residuals"]] <- rstandard(modelo_logistico)
output[["studentized.residuals"]] <- rstudent(modelo_logistico)
output[["cooks.distance"]] <- cooks.distance(modelo_logistico)
output[["dfbeta"]] <- dfbeta(modelo_logistico)
output[["dffit"]] <- dffits(modelo_logistico)
output[["leverage"]] <- hatvalues(modelo_logistico)
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat("Residuos estandarizados fuera del 95 % esperado \n")
cat("------------------------------------------------\n")
print(rownames(muestra[sospechosos1,]))
# Revisar casos con distancia de Cook mayor a uno .
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat(" Residuales con una distancia de Cook alta \n")
cat(" -----------------------------------------\n")
print(rownames(muestra[sospechosos2,]))
# Revisar casos cuyo apalancamiento sea mÃ¡s del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(muestra)/nrow(datos)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio )
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat(" Residuales con levarage fuera de rango (> ")
cat(round(leverage.promedio,3),")", "\n", sep = "")
cat(" --------------------------------------\n")
print(rownames(muestra[sospechosos3,]))

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]]>= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat(" Residuales con DFBeta sobre 1\n")
cat(" -----------------------------\n")
print(rownames(muestra[sospechosos4,]))
# Detalle de las observaciones posiblemente atípicas .
sospechosos <- c(sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4 )
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat(" Casos sospechosos \n")
cat(" -----------------\n")
print(muestra[sospechosos,])
cat("\n\n")
print(output[ sospechosos,])
#Validación cruzada

datos_7<- muestra[,c("Biiliac.diameter","Navel.Girth","Ankle.Minimum.Girth","Age","EN")][-1]
n <- nrow(datos_7)
n_entrenamiento <- floor(0.7*n)
muestra2 <- sample.int(n = n, size=n_entrenamiento,replace = FALSE )
entrenamiento <- datos[muestra2,]
prueba <- datos[-muestra2,]
modelo_7 <- lm(EN ~ Biiliac.diameter+Navel.Girth+Ankle.Minimum.Girth+Age, data =entrenamiento )
print(modelo_7)
mse_entrenamiento <- c(modelo_7$residuals) ** 2
mse_entrenamiento <- mean(mse_entrenamiento)
cat("MSE para el conjunto de entrenamiento :", mse_entrenamiento , "\n")            
predicciones <- predict(modelo_7,prueba)
# Calcular error cuadrado promedio para el conjunto de prueba .
extra <- as.numeric(as.character(prueba$EN))
error <- extra - predicciones
mse_prueba <- mean( error ** 2)
cat("MSE para el conjunto de prueba :", mse_prueba)

#En los graficos existen valores atípicos, los cuales resaltan reiteradamente, influyendo en los resultados visualizados en
#Los graficos, estas observaciones son la 80, 88 y 491, la cual esta última en el grafico de distancia de Cook se puede ver que
#realiza un apalancamiento mayor en la gráfica, llevando la distancia de Cook a casi 1.

#Realizando la comparación entre el modelo de regresión logística de una variable, se puede ver que que la presición es de 0.88
#junto con una sensitividad de 0,9375 y especifidad de 0,7778. considerando que la variable escogida para el modelo de regresion logistica
#simple, es de 0.54 > 0.5 se considerará levemente fuerte, y una relación correcta.

