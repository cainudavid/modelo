# FUENTE https://rpubs.com/learsi_96/628980
# Objetivo ----------------------------------------------------------------

# Realizar un análisis de datos identificando coeficientes de correlación 
# entre variables así como un modelo de regresión lineal múltiple para 
# determianr predicciones con nuevos datos.

library(dplyr)
library(readr)   # Leer datos
library(knitr)   # tablas amigables
library(ggplot2) # gráficos más amigables y potentes

datos <- as.data.frame(state.x77)
str(datos)
summary(datos)

# Limpiar datos -----------------------------------------------------------
datos <- rename(habitantes = Population, 
                analfabetismo = Illiteracy,
                ingresos = Income, 
                esperanza_vida = `Life Exp`, 
                asesinatos = Murder,
                universitarios = `HS Grad`, 
                heladas = Frost, 
                area = Area,
                .data = datos)

datos <- mutate(.data = datos, densidad_pob = habitantes * 1000 / area)
kable(head(datos)) # Primeros
kable(tail(datos)) # Últimos

# Coeficiente de Correlación ----------------------------------------------
r <- cor(datos)
r <- as.data.frame(r)
kable(r)

# Interpretación del Coeficiente de Correlación ---------------------------

modelo <- lm(esperanza_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area + densidad_pob, data = datos )
summary(modelo)

# Determinar mejores predictores ------------------------------------------

# función step() para obtener como resultado el mejor modelo, se busca identifica
# cuales variabes representan variabilidad con respecto a la esperanza de vida, 
# o lo que es lo mismo cuáles SI son importantes y cuáles NO con respecto a la 
# variable dependiente esperanza de vida

step(object = modelo, direction = "both", trace = 1)

# Generando el mejor modelo basado en Akaike (AIK) ------------------------

modelo <- (lm(formula = esperanza_vida ~ habitantes + asesinatos + universitarios +
                heladas, data = datos))
summary(modelo)

# Interpretación del Modelo -----------------------------------------------

# Esperanza−de−vida=7.103e01+5.014e05∗habitantes−3.001e01∗aesinatos+
# 4.658e02∗universitarios−5.943e03∗heladas

# El 73,6% de variabilidad es explicada por esas variables
# El test F muestra que es significativo (p-value: 1.696e-12). Se satisfacen
# todas las condiciones para este tipo de regresión múltiple.

# Predicciones ------------------------------------------------------------

# Cuál sería la esperanza de vida para tres ciudades que tienen los siguientes valores: 
# habitantes=500,2000,10000
# asesnatos=4,12,15
# universitarios=60,40,20
# heladas=150,100,50
nuevos.Valores <- data.frame(habitantes = c(500, 2000, 10000),
                             asesinatos = c(4, 12, 15),
                             universitarios = c(60,40,20),
                             heladas = c(150,100,50))
nuevos.Valores

prediccion <- predict(modelo, newdata = nuevos.Valores)
prediccion

# Comprobar la predicción conforme a la fórmula
r <- cor(nuevos.Valores)
r <- as.data.frame(r)
kable(r)
