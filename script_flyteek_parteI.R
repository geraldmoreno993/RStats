###########################################################################
# Nombre del Script: bioestadisica_flyteek_parte1.R
# Descripción: "Bioestadística con RStudio aplicada a biociencias parte I"
# Autor: Gerald Moreno Morales
# Fecha: 21/10/2024
###########################################################################

#Remover objetos de la memoria de R
rm(list =ls())


################################################## 
#                Comandos básicos                #
################################################## 

#Este es un comentario
#Todo código debe ser comentado

#Ejecutar código de calculadora
3+4
10-6
4*4
5^2
sqrt(36)
(4*4)*(4-5)

#Ejercicio
sqrt(100)*(5^2)

#Estableciendo carpeta de trabajo
setwd("/home/gerald/Documentos/Clases_propias/bioestadistica_version2.0")
getwd()


#Este es un vector
notas <- c(14,15,13,17,20,18) 
class(notas)

#Esta es una matriz 6x1 
matriz <- matrix(notas, ncol=1)
matriz
class(matriz)

#Esta es una matriz
matriz <- matrix(notas, ncol=1)
matriz
class(matriz)

#Estos son dataframe
iris
cars
df <- iris

#Describiendo data
str(df)
summary(df)
table(df$Species)
t <- table(df$Species)

prop.table(t)

#Usando paquetes
install.packages("psych")
library("psych")
describe(df)

#Filtrando datos de dataframes
df
df_set_ver <- subset(df, Species == "setosa" | Species == "versicolor")
df_set_ver


################################################## 
#      T-test para muestras independientes       #
################################################## 

# Supuestos

#1# Muestra aleatoria
#2# Independencia de las observaciones
#3# Variable distribuida normalmente (solo cuando n < 30)
#4# Homocedasticidad

# Pregunta: ¿Las medias de longitud de sépalo (Sepal.length) poblacionales de
#las especies setosa y versicolor son iguales?

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La medias de long poblacionales de sépalo son iguales (μ1=μ2).
# Hipótesis alternativa (Ha): La medias de long de sépalo poblacionales son diferentes (μ1≠μ2)

df_set_ver #Este es mi dataframe para las pruebas iniciales

#Revisar dataset
colnames(df_set_ver)
dim(df_set_ver)

#Generamos dos subsets más
df_set <- subset(df, Species == "setosa")
df_ver <- subset(df, Species == "versicolor")

#Supuestos
#Normalidad
qqnorm(df_set$Sepal.Length)
qqline(df_set$Sepal.Length)

qqnorm(df_ver$Sepal.Length)
qqline(df_ver$Sepal.Length)

#Homocedasticidad
bartlett.test(Sepal.Length ~ Species, data = df_set_ver)
boxplot(df_set$Sepal.Length, df_ver$Sepal.Length, names = c("setosa" , "virginica"))

#Welch Two Sample t-test
t.test(Sepal.Length ~ Species, data=df_set_ver, var.equal = FALSE)


################################################## 
#            Prueba U de Mann-Whitney            #
##################################################

#Supuestos:
#1) Independencia
#2) Escala Ordinal o Intervalo
#3) Forma de la Distribución similar
#4) Mismo Número de Observaciones
##Usarla cuando mis poblaciones estudiadas no siguen una dist. normal

#H0: Las distribuciones de ambas muestras son iguales (medianas poblacionales iguales).
#Ha: Las distribuciones de las dos muestras no son iguales (medianas poblacionales diferentes).

wilcox.test(Sepal.Length ~ Species, data = df_set_ver, correct = TRUE)



################################################## 
#                      ANOVA                     #
##################################################

#Supuestos

#1. k muestras aleatorias de sus respectivas poblaciones
#2. Independencia de las Observaciones
#3. Normalidad de los Errores
#4. Homogeneidad de Varianzas (Homocedasticidad)

#Pregunta: Habrá diferencias de la longitud promedio de sépalo entre las 3 especies de iris?

#Ho: Las medias de las 3 poblaciones (3 especies) son iguales 
#Ha: Al menos dos medias poblacionales son distintas

library(psych)

#Descripcion completa por catergoria (no te olvides library(psych))
describeBy(iris$Sepal.Length, iris$Species)
# Ajustar el modelo ANOVA
modelo <- aov(Sepal.Length ~ Species, data = iris)
summary(modelo)

# Evaluación de la normalidad de los residuos
residuos <- residuals(modelo)
qqnorm(residuos)
qqline(residuos)
shapiro.test(residuos)

# Evaluación de la homogeneidad de varianzas
bartlett.test(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Length ~ Species, data = iris)

# Realizar la prueba de Welch de ANOVA
oneway.test(Sepal.Length ~ Species, data = iris, var.equal = FALSE)


################################################## 
#            Prueba de Kruskall-Wallis           #
################################################## 

#Supuestos:

#1. Independencia de las Observaciones
#2. Variables deben estar en escala al menos ordinal
#3. Distribuciones con Forma Similar

#Pregunta: ¿Las distribuciones de longitud de sepalo de las 3 
#especies (poblaciones) son iguales?

#Ho: Las distribuciones de las 3 poblaciones son iguales
#Ha: Al menos dos distribuciones poblacionales son diferentes entre si

#Medianas
library("psych")
describeBy(iris$Sepal.Length, iris$Species)

#Supuesto de distribuciones iguales
hist(iris$Sepal.Length[iris$Species == "setosa"])
hist(iris$Sepal.Length[iris$Species == "virginica"])
hist(iris$Sepal.Length[iris$Species == "versicolor"])

# Realizar la prueba de Kruskal-Wallis
kruskal.test(Sepal.Length ~ Species, data = iris)
kruskal_test <- kruskal.test(Sepal.Length ~ Species, data = iris)



################################################## 
#          T-test para muestras pareadas         #
##################################################
# Supuestos

#1# Muestra aleatoria
#2# Independencia de las observaciones
#3# Cada pareja debe obtenerse del mismo sujeto
#4# Las diferencias (antes y después) se distribuyen de manera normal

# Pregunta: ¿Las medias de peso (weight) poblacionales de los tiempos 20 y 21
#del experimento son iguales?

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La medias de pesos poblacionales antes y despues son iguales (μ1=μ2).
# Hipótesis alternativa (Ha): La medias de los pesos poblacionales antes y despues son diferentes (μ1≠μ2)

#Leer archivos .csv externo
chickweight_reshaped <- read.csv("chickweight_reshaped.csv")

# Explorar los datos reestructurados
View(chickweight_reshaped)

#Describir
str(chickweight_reshaped)
summary(chickweight_reshaped)

# Calcular las diferencias entre los pesos en los tiempos 20 y 21
differences <- chickweight_reshaped$weight.21 - chickweight_reshaped$weight.20

# Test de normalidad sobre las diferencias
hist(differences, breaks = 10, main = "Histograma de las diferencias", xlab = "Diferencias en el peso")
shapiro.test(differences)  # Para verificar si las diferencias siguen una distribución normal

# Observar los datos antes de realizar el test
boxplot(chickweight_reshaped$weight.20, chickweight_reshaped$weight.21,
        names = c("tiempo 20", "tiempo 21"),
        main = "Peso del pollito en los tiempos 20 y 21")

# Realizar la prueba t pareada
t.test(chickweight_reshaped$weight.20, chickweight_reshaped$weight.21, paired = TRUE)

nrow(chickweight_reshaped)

################################################## 
#            Wilcoxon Signed-Rank Test           #
##################################################

#Supuestos:

#Los datos son n pares de diferencias.
#Cada par es tomado de un sujeto, o de sujetos pareados por una o más variables
#Los pares de observaciones provienen de una muestra aleatoria
#Las diferencias representan observaciones de variables numericas continuas


# Hipotesis:
# Ho: la mediana de las diferencias poblacionales es igual cero 
# Ha: la mediana de las diferencias poblacionales es diferente de cero 

# Pregunta: ¿Habra diferencias entre las medianas de peso antes y después?

wilcox.test(chickweight_reshaped$weight.21, chickweight_reshaped$weight.20, paired = TRUE, exact = FALSE)

###################################################### 
#  Coeficiente de correlación de Pearson y Spearman  #
###################################################### 

#1) Relación Lineal de las variables
#2) Distribucioń normal de las variables
#3) Homogeneidad de Varianzas (Homocedasticidad)
#4) Independencia de observaciones

#Pregunta: ¿Existe relación lineal entre las variables de estudio (peso de auto y consumo de comb.) ?
#Ho: No hay una relación lineal entre las dos variables en la población (ρ = 0)
#Ha: Existe relación lineal entre las dos variables en la población (ρ ≠ 0)

# Cargar el dataset
data(mtcars)

# Ver las primeras filas del dataset
head(mtcars)

# Graficar la relación entre mpg y wt
plot(mtcars$wt, mtcars$mpg, main="Relación entre el peso del auto y el consumo de combustible", xlab="Peso del auto (1000 lbs)", ylab="Consumo de combustible (mpg)")
abline(lm(mpg ~ wt, data=mtcars), col="red")

# Q-Q plot para mpg
qqnorm(mtcars$mpg)
qqline(mtcars$mpg)

# Q-Q plot para wt
qqnorm(mtcars$wt)
qqline(mtcars$wt)


# Prueba de correlación de Pearson
##################################
#Supuestos
#Variables al menos ordinales
#Observaciones independientes
#Relación monotónica (se observa alguna tendencia a la linealidad)

cor.test(mtcars$wt, mtcars$mpg, method="pearson")

# Prueba de correlación de Spearman
#####################################

#minimo las variables deben estar en escala ordinal

cor.test(mtcars$wt, mtcars$mpg, method="spearman")




###################################################### 
#  Prueba de Chi-cuadrado y prueba exacta de Fisher  #
######################################################

#Supuestos

#1) Variables categóricas
#2) Tamaño de muestra mayor a 20 (ntotal > 20)
#3) No más del 20% de los valoes esperados debe ser < 5 y ninguno es = 0
#Si no se cumple, realizar la prueba exacta de Fisher 


####Pregunta: "¿Existe una asociación significativa entre la especie de pingüino y la isla en la que habitan?"

#Ho: Las variables especie e isla son independientes (No hay asociación)
#Ha: Las variables especie e isla estan asociadas

penguin <- read.csv("penguins.csv")
head(penguin)

# Eliminar filas con valores NA
penguin <- na.omit(penguin)

#Describir
str(penguin)
summary(penguin)

# Convertir columnas character a factor
library(dplyr)
penguin <- penguin %>%
  mutate_if(is.character, as.factor)

# Crear una tabla de contingencia entre 'species' e 'island'
tabla <- table(penguin$species, penguin$island)
print(tabla)

# Realizar la prueba de Chi-cuadrado
chi2 <- chisq.test(tabla)
print(chi2)

#Supuesto de valores esperados:
chi2$expected

#En caso no se cumplan los supuestos:

###Prueba exacta de Fisher
#############################

# Realizar la prueba exacta de Fisher
fisher.test(tabla, workspace = 2e8)

#Simulación de Monte Carlo:
fisher.test(tabla, simulate.p.value = TRUE, B = 10000)

library("ggplot2")
# Crear el gráfico de barras apiladas
ggplot(penguin, aes(x = island, fill = species)) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Especies de Pingüinos por Isla",
       x = "Isla", y = "Número de Pingüinos", fill = "Especie") +
  theme_minimal() +
  theme(text = element_text(size = 14))




