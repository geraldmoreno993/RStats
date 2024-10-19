setwd("/home/gerald/Documentos/Clases_propias/curso_2/data_final")

############################################################
# Nombre del Script: regresiones_flyteek_parte2.R
# Descripción: "Bioestadística con R parte II"
# Autor: Gerald Moreno Morales
# Fecha: 19/08/2024
# Referencias: ver paquetes
############################################################


install.packages("tidyverse")
install.packages("mlbench")
install.packages("GGally")
install.packages("broom")
install.packages("jtools")

library(tidyverse)
library(mlbench)
library(GGally)
library(broom)
library(jtools)


################################################## 
#                Regresion lineal                #
##################################################
#Supuestos
#1) Relación Lineal entre las dos variables
#2) No Presencia de Outliers
#3) Homocedasticidad de residuos 
#4) Normalidad de los Errores
#5) Independencia de los Errores
#Pregunta: ¿Cuánto es el efecto de variable independiente () con una 
#variable dependiente (esperanza de vida)?

#Ho : β1 = 0
#Ha : β1 != 0

# Cargar los datos de state.x77
#Convertir los datos en un data frame para facilitar su manejo
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

# Ver las primeras filas del data frame para confirmar
head(datos)
View(datos)

# Realizar la regresión lineal
modelo <- lm(esp_vida ~ ingresos, data = datos)
summary(modelo)

# Graficar la relación entre las dos variables
plot(datos$esp_vida, datos$ingresos, main="Relación entre ingresos y esp. de vida", xlab="Ingresos", ylab="Esp. de vida(años)")

#Errores vs valores ajustados (linealidad y homocedasticidad)
plot(modelo$fitted.values, residuals(modelo))
abline(h = 0, col = "red")

# Gráfico Q-Q de los residuos (normalidad de errores)
qqnorm(residuals(modelo))
qqline(residuals(modelo), col = "red")
shapiro.test(modelo$residuals)

# Instalar y cargar el paquete car
install.packages("car")
library(car)

## Independencia de residuos: Prueba de Durbin-Watson
durbinWatsonTest(modelo) #(si p<0.05 hay codependencia)

# Resumen del modelo
summary(modelo) 

# Crear el gráfico de dispersión con la línea de regresión
ggplot(datos, aes(x = ingresos, y = esp_vida)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresión Lineal: Esperanza de Vida vs Ingreso",
       x = "Ingreso",
       y = "Esperanza de Vida")

#Y = β0 + β1X + ε
#beta1= 0.0007
#espvida = 67 + 7.4*(10^(-4)).ingresos + ε

#el valor p nos indica si existe una evidencia suficiente para 
#rechazar la hipótesis nula de que el coeficiente de una variable 
#es igual a cero (es decir, que la variable no tiene un efecto significativo 
#sobre la variable dependiente).





############################################### 
#             Regresión logística             #
###############################################

#Se desea evaluar la asociación de diversos genes de resistencia con el fenotipo
#resistencia a la meticilina en genomas de Staphylococcus aureus

#Supuestos
#1) Variable dependiente binaria
#2) Independencia de las observaciones
#4) Relación lineal entre el logit de la prob. de Y, y la variable independiente
#(solo aplicar a variables numericas)

library(tidyverse)
staphy <- read_csv("staphy.csv")
str(staphy)

# Convertir todas las columnas character a factor
staphy<- staphy %>%
  mutate(across(75:ncol(.), as.factor))

staphy_mini <- staphy[,75:ncol(staphy)]
levels(staphy_mini$MRSA)

#Lo que se desea hacer es evaluar las asociación entre la presencia de diversos genes
#vs el fenotipo meticilinorresistente

#describiendo

tab <- table(staphy_mini$mecA,staphy_mini$MRSA)  
print(tab)
fisher.test(tab)

# Modelo de regresión logística
modelo0 <- glm(MRSA ~  mecA  , data = staphy_mini, family = binomial)
modelo1 <- glm(MRSA ~  merB  , data = staphy_mini, family = binomial)

# Resumen del modelo
summary(modelo0)
summary(modelo1)

# Extraer los coeficientes del modelo
coeficiente0 <- coef(modelo0)
coeficiente1 <- coef(modelo1)

# Convertir los coeficientes a odds ratios
OR0 <- exp(coeficiente0)
OR1 <- exp(coeficiente1)
OR2 <- exp(coeficiente2)

# Mostrar los OR
print(OR0)
print(OR1)

#Regresión logística múltiple

modelo3 <- glm(MRSA ~  merB + mecA, data = staphy_mini, family = binomial)

summary(modelo3)
coeficiente3 <- coef(modelo3)
OR3 <- exp(coeficiente3)
# Calcular los intervalos de confianza
IC <- exp(confint(modelo3))
print(OR3)
print(IC)


################################################## 
#    Regresión de Poisson y binomial negativa    #
##################################################

#ln(λi)=β0+ β1*Xi

#Depende de un solo parámetro (λ): que es la tasa
#promedio de eventos por unidad de tiempo/espacio/incidencia/prevalencia
#Ej. numero de insectos muertos por unidad de tiempo

#Supuestos
#1) Los eventos ocurren independientemente entre si. 
#2) La ocurrencia de un evento no afecta la probabilidad de ocurrencia de otro evento.
#3) λ es constante a través del tiempo (enfermedades de rapida propagacion no cumplen).
#4) Media = varianza (dispersión) de Y.
#En la vida real, la mayoría de situaciones violan el
#supuesto de dispersión de la variable respuesta: (media=varianza).

#Cargar el dataset InsectSprays
data("InsectSprays")

##Objetivo: Se ajustará un modelo de regresión para explicar el número de 
##insectos muertos (count) en función del tipo de spray usado
##DF: Unidades experimentales tratadas con diferentes insecticidas

# Verificar la estructura del dataset
str(InsectSprays)
View(InsectSprays)

library("psych")
describeBy(InsectSprays$count, InsectSprays$spray)

# Ajustar un modelo de regresión de Poisson
poisson_model <- glm(count ~ spray, data = InsectSprays, family = poisson)

# Resumen del modelo
summary(poisson_model)

# Calcular la razón de devianza
dev_ratio <- sum(residuals(poisson_model, type = "deviance")^2) / poisson_model$df.residual
print(dev_ratio)   ##Devianza/grados de libertad debe ser = 1 para que se cumpla el supuesto


#Coeficientes: Cada coeficiente representa el cambio logarítmico esperado 
#en el número de conteos para un incremento unitario en la variable independiente.
### Dificil de interpretar

# Obtener y transformar los coeficientes del modelo
coef_poisson_model <- coef(poisson_model)
exp_coef_poisson <- exp(coef_poisson_model)
exp_coef_poisson 

#Interpretación:
##Esto significa que la tasa(o conteo) esperada de eventos es ... veces mayor cuando
#se usa el insecticida x con respecto al insecticida de referencia.

#Regresion binomial negativa
#############################

#log(µ) = β0 + β1*X1

#La regresión binomial negativa es usada
#para estimar variables de conteo de un
#evento que tiene sobre dispersión.

# Instalar y cargar el paquete MASS
install.packages("MASS")
library(MASS)

# Ajustar un modelo de regresión binomial negativa
neg_binomial_model <- glm.nb(count ~ spray, data = InsectSprays)

# Resumen del modelo
summary(neg_binomial_model)

# Obtener y transformar los coeficientes del modelo
coef_neg_binomial <- coef(neg_binomial_model)
exp_coef_neg_binomial <- exp(coef_neg_binomial)
exp_coef_neg_binomial

#Se interpreta exactamente igual que Poisson

################################################## 
#                 Muchas gracias                 #
##################################################