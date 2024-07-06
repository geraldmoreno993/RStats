############################################################
# Nombre del Script: bioestadisica_flyteek_parte1.R
# Descripción: "Bioestadística con R parte I"
# Autor: Gerald Moreno Morales
# Fecha: 04/07/2024
############################################################

rm(list =ls())
getwd()
setwd("/home/gerald/Documentos/Clases_propias")
getwd()

################################################## 
#                Comandos básicos                #
################################################## 

#Calculadora
4+4
5-7
5*3
2**4

#Extrayendo columnas de un datafrane
cars
df0 <- cars 
speed <- cars$speed
class(speed)
summary(speed)
summary(df0)

install.packages("psych")
library(psych)

#Describiendo df o variables
describe(df0)
describe(speed)
hist(speed)
hist(speed, breaks=20)
boxplot(speed)

PlantGrowth #En este dataframe tengo una variable tratamiento con 3 grupos
describe(PlantGrowth)
plantg <- PlantGrowth

plantg$group  #extraigo la variable categórica
grupos <- plantg$group
table(grupos)
tab <- table(grupos)
prop.table(tab)  #*100

#Gráficos para variables categóricas
barplot(tab)
pie(tab)

#Como filtrar datos de la tabla
head(plantg)
filt_ctrl <- subset(PlantGrowth, group == "ctrl")
summary(filt_ctrl)

filt_ctrl2 <- subset(PlantGrowth, group == "ctrl"|group == "trt1")

View(filt_ctrl2)
summary(filt_ctrl2)

################################################## 
#                    Prueba Z                    #
##################################################

# Instalar y cargar el paquete psych
install.packages("psych")
library(psych)

# Obtener resumen detallado del dataset iris
describe(iris)
head(iris)   #Observamos que hay una variable categórica
table(iris$Species) 

#############################
##Prueba Z para una muestra##
#############################

# Instalar y cargar el paquete psych
install.packages("BSDA")
library(BSDA)

#Cargar el dataset
data(iris)
View(iris)

#Filtrar datos para la especie setosa
setosa_data <- subset(iris, Species == "setosa")

# Pregunta: ¿La media poblacional actual de long. de sepalo de la especie 
#Iris setosa sera igual a la media historica (μ=5)?

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La media de la longitud del sépalo de la especie 
#setosa es igual a 5 cm (μ=5).
# Hipótesis alternativa (Ha): La media de la longitud del sépalo de la 
#especie setosa es diferente de 5 cm (μ≠5).

#Supuestos

#1# Muestra aleatoria
#2# Independencia de las observaciones
#3# Variable distribuida normalmente (generalmente cuando n < 30)
#4# Varianza poblacional conocida (en este caso definimos sd_pob = 0.4)
#(si no se conoce se puede usar un T.test de una muestra)

#NORMALIDAD
# Hipótesis nula (Ho): La distribucion de la variable en la pob. es normal
# Hipótesis ALTERNA (HA): La distribucion de la variable en la pob. no es normal

describe(setosa_data)
describe(setosa_data$Sepal.Length)
#########
##P=0.05#
#########

#Evalúo la normalidad de manera opcional
shapiro <- shapiro.test(setosa_data$Sepal.Length)
print(shapiro)

# Definimos elementos
media_hipotetica <- 5
sd_pob <- 0.4
summary(setosa_data$Sepal.Length)
media_muestra <- mean(setosa_data$Sepal.Length)
print(media_muestra)

# Calcular el p valor de la prueba Z
resultado <- z.test(setosa_data$Sepal.Length, mu = media_hipotetica, sigma.x = sd_pob)
resultado

# Calcular el p valor de la prueba T (cuando la sd_pob es desconocida)
resultado_t <- t.test(setosa_data$Sepal.Length, mu = media_hipotetica)
print(resultado_t)

################################################## 
#      T-test para muestras independientes       #
################################################## 

# Supuestos

#1# Muestra aleatoria
#2# Independencia de las observaciones
#3# Variable distribuida normalmente (generalmente solo cuando n < 30)
#4# Homocedasticidad

# Pregunta: ¿Las medias de longitud de petalo (Petal.length) poblacionales de
#las especies virginica y setosa son iguales?

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La medias de long poblacionales de petalo son iguales (μ1=μ2).
# Hipótesis alternativa (Ha): La medias de long poblacionales son diferentes (μ1≠μ2)

library(psych)
describe(iris) #cuales son los n?
data2 <- subset(iris, Species == "virginica" | Species == "setosa")
vir <- subset(iris, Species== "virginica")
set <- subset(iris, Species== "setosa")

#Normalidad
hist(vir$Petal.Length)
hist(set$Petal.Length)

shapiro.test(vir$Petal.Length)
shapiro.test(set$Petal.Length)

#Homocedasticidad
#Ho = homocedasticidad
#Ha = heterocedasticidad
bartlett.test(Petal.Length ~ Species, data = data2)
#p-value =1.922e-13

#Welch Two Sample t-test
t.test(vir$Petal.Length, set$Petal.Length, var.equal = FALSE)

#p-value < 2.2e-16
describe(vir) #5.55
describe(set) #1.46 
#Existe evidencia estadistica para rechazar la Ho que las longitudes de 
#petalo de setosa y virginica sean iguales (p-value < 2.2e-16)

################################################## 
#            Prueba U de Mann-Whitney            #
##################################################
##Supuestos
#1) Independencia
#2) Escala Ordinal o Intervalo
#3) Forma de la Distribución similar
#4) Mismo Número de Observaciones
##Usarla cuando mis poblaciones estudiadas no siguen una dist. normal

#H0: Las distribuciones de ambas muestras son iguales (medianas poblacionales iguales).
#Ha: Las distribuciones de las dos muestras no son iguales (medianas poblacionales diferentes).

resultado_mann_whitney <- wilcox.test(vir$Petal.Length, set$Petal.Length, paired = FALSE, correct = TRUE)
print(resultado_mann_whitney)

################################################## 
#          T-test para muestras pareadas         #
##################################################
# Supuestos

#1# Muestra aleatoria
#2# Independencia de las observaciones
#3# Cada pareja debe obtenerse del mismo sujeto
#4# Las diferencias (antes y despues) se distribuyen de manera normal

data(ChickWeight)
data <- subset(ChickWeight, Time == 0 | Time == 12)
data$Diet <- NULL
View(data)
# Reestructurar los datos utilizando la función reshape
chickweight_reshaped <- reshape(data, 
                                idvar = "Chick", 
                                timevar = "Time", 
                                direction = "wide")

View(chickweight_reshaped)   ##Explorar esto

# Pregunta: ¿Las medias de peso (weight) poblacionales de los tiempos 1 y 12 
#del experimento son iguales?

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La medias de pesos poblacionales antes y despues son iguales (μ1=μ2).
# Hipótesis alternativa (Ha): La medias de los pesos poblacionales antes y despues son diferentes (μ1≠μ2)

View(data)


# Calcular las diferencias
differences <- chickweight_reshaped$weight.12 - chickweight_reshaped$weight.0

#Test de Normalidad
hist(differences)
shapiro.test(differences)

#Observo antes de hacer el test
boxplot(chickweight_reshaped)

# Realizar la prueba t pareada
t_test_paired <- t.test(chickweight_reshaped$weight.0, chickweight_reshaped$weight.12, paired = TRUE)
print(t_test_paired)

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


wilcox.test(chickweight_reshaped$weight.0, chickweight_reshaped$weight.12, paired = TRUE, exact = FALSE)


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

data(iris) # Cargar el dataset iris
tapply(iris$Sepal.Length, iris$Species, mean) # medias de Sepal.Length por especie 

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

# Realizar la prueba de Welch de ANOVA
welch_anova <- oneway.test(Sepal.Length ~ Species, data = iris, var.equal = FALSE)
print(welch_anova)

#Un valor de p pequeño < 0.05 indica que al menos dos categorías serán 
#diferentes

#Posthoc de Games-Howell (robusto para heterocedasticidad)
install.packages("rstatix")
library(rstatix)
gmt <- games_howell_test(iris, Sepal.Length ~ Species, conf.level = 0.95, detailed = FALSE)
print(gmt) 

tapply(iris$Sepal.Length, iris$Species, mean) ##Para ver las medias

#En caso de que no puedas instalar el paquete
## Prueba post hoc con ajuste de Holm
#posthoc_result <- pairwise.t.test(iris$Sepal.Length, iris$Species, p.adjust.method = "holm")
#print(posthoc_result)

################################################## 
#            Prueba de Kruskall-Wallis           #
################################################## 

#Supuestos
#Independencia de las Observaciones
#Variables deben estar en escala al menos ordinal
#Distribuciones con Forma Similar

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

#posthoc test
install.packages("FSA")
install.packages("dunn.test")
library(FSA)
library(dunn.test)

# Prueba de Dunn para análisis post-hoc
dunn_test <- dunnTest(Sepal.Length ~ Species, data = iris, method = "bonferroni")
print(dunn_test)


tapply(iris$Sepal.Length, iris$Species, median) ##Para ver las medias

####################################################### 
#  ANOVA para medidas repetidas y Prueba de Friedman  #
#######################################################

# Instalar y cargar el paquete nlme
install.packages("nlme")
library(nlme)

# Cargar la base de datos Orthodont
data("Orthodont")

# Mostrar las primeras filas de la base de datos
head(Orthodont)
View(Orthodont)
# Resumen estadístico de la base de datos
summary(Orthodont)
num_sujetos <- length(unique(Orthodont$Subject))  # Contar el número de sujetos únicos
num_sujetos

table(sort(Orthodont$Subject), Orthodont$age)

#Contiene medidas repetidas del crecimiento dental en niños y niñas a 
#lo largo del tiempo, lo que la hace ideal para análisis de modelos de 
#efectos mixtos y ANOVA de medidas repetidas.

# ANOVA de Medidas Repetidas

#Supuestos:
#1.-Evaluación de la Normalidad de los residuos
#2.-Evaluación de la Esfericidad
#3.-Independencia de los individuos

#Pregunta: ¿Aumenta la distancia entre la pituitaria y la fisura pterigomaxilar de manera uniforme a medida que los niños crecen de 8 a 14 años?

#Hipótesis Nula (H0): No hay diferencias significativas en las medias de los diferentes tiempos.
#Hipótesis alterna (Ha): Hay al menos una diferencia significativa en las medias de los diferentes tiempos.

# Instalar y cargar los paquetes necesarios
install.packages("ez")
install.packages("nlme")
install.packages("car")
library(ez)
library(nlme)
library(car)

# Cargar el conjunto de datos Orthodont
data("Orthodont", package = "nlme")

# Convertir los sujetos y la variable de edad en factores
Orthodont$Subject <- factor(Orthodont$Subject)
Orthodont$age <- factor(Orthodont$age)

# Realizar el ANOVA de medidas repetidas usando ezANOVA
model <- ezANOVA(
  data = Orthodont,
  dv = distance,
  wid = Subject,
  within = age,
  type = 3,
  detailed = TRUE
)

# Mostrar el resumen del modelo (incluye evaluación de esfericidad)
print(model)

# Test de Mauchly p<.05 quiere decir que no se viola la esfericidad para el efecto de la edad
# Realizar el ANOVA de medidas repetidas usando aov
model_aov <- aov(distance ~ age + Error(Subject/age), data = Orthodont)

# Extraer los residuos del modelo aov
residuals <- residuals(model_aov$`Subject:age`) # Verificar los residuos print(head(residuals))

# Evaluacionn de normalidad de residuos
shapiro_test_result <- shapiro.test(residuals)
print(shapiro_test_result)
qqPlot(residuals, main = "Q-Q Plot of Residuals")
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", breaks = 10, col = "lightblue")


# Instalar y cargar ggplot2 si no está ya instalado
install.packages("ggplot2")
library(ggplot2)

# Gráfico de líneas de distancia vs edad por sujeto, coloreado por sexo
#Spaghetti plot
ggplot(Orthodont, aes(x = age, y = distance, group = Subject, color = "pacientes")) +
  geom_line() +
  labs(title = "Crecimiento Dental en Niños y Niñas",
       x = "Edad (años)",
       y = "Distancia (mm)") +
  theme_minimal()

###################################################################
#1. Medidas repetidas en el tiempo (más de 3 tiempos)
#2. Medidas en diferentes condiciones (más de 3 sujetos por )
#3. Mediciones ordinales o continuas

#Sujeto (Subject): 1, 2, 3, 4 son los bloques.
#Edad (Age): 8, 10, 12 años son los diferentes 
#"tratamientos" o condiciones bajo las cuales se mide la distancia.

#Pregunta: ¿Hay diferencias significativas en el crecimiento maxilar de los sujetos durante el periodo de crecimiento?

##Prueba de Friedman
#Hipótesis Nula (H0): Las distribuciones de los rangos entre los diferentes tratamientos (o condiciones) son idénticas.
#Hipótesis Alternativa (H1): Al menos una de las distribuciones de los rangos es diferente de las demás.

# Instalar y cargar el paquete necesario (aumentar esto a los alumnos)
install.packages("nlme")
install.packages("dplyr")
install.packages("tidr")
# Cargar el paquete y los datos (aumentar esto a los alumnos)
library(nlme)
library(dplyr)
library(tidyr)

data("Orthodont")

# Mirar los primeros registros del dataframe
head(Orthodont)

# Transformar los datos a un formato adecuado para el test de Friedman
# Pivotar los datos para que cada sujeto tenga sus medidas en columnas separadas
orthodont_wide <- Orthodont %>%
  pivot_wider(names_from = age, values_from = distance)

# Convertir el dataframe a una matriz
orthodont_matrix <- as.matrix(orthodont_wide[ , -1])

# Realizar el test de Friedman
friedman_test <- friedman.test(orthodont_matrix)
print(friedman_test)

#Si este valor es menor que 0.05, se rechaza la hipótesis nula de que todas 
#las edades tienen la misma distribución de distancia.



###################################################### 
#  Prueba de Chi-cuadrado y prueba exacta de Fisher  #
######################################################

#Supuestos

#1) Variables categóricas
#2) Tamaño de muestra mayor a 20 (ntotal > 20)
#3) Las frecuencias esperadas de al menos el 20% 
#de cada casilla debe ser al menos 5... Si no se cumple, realizar la prueba exacta de
#Fisher 


####Pregunta: Un tratamiento nuevo estará asociado a la mejora de síntomas
#en una población con artitris??

#Ho: Las variables tratamiento y mejora de sintomas son independientes
#Ha: Las variables tratamiento y mejora de sintomas están asociadas

# Instalar y cargar el paquete vcd
install.packages("vcd")
library(vcd)

# Cargar el dataset Arthritis
data("Arthritis")
View(Arthritis)

# Mostrar una vista previa del dataset
head(Arthritis)

# Crear una tabla de contingencia para Treatment y Improved
contingency_table <- table(Arthritis$Treatment, Arthritis$Improved)
print(contingency_table)

# Realizar el test chi-cuadrado
chi_square_test <- chisq.test(contingency_table)

# Mostrar el resultado del test chi-cuadrado
print(chi_square_test)

#Evaluo frecuencias esperadas
print(chi_square_test$expected)
nrow(Arthritis)  #Tamaño muestral

##Fisher exact test (en caso de que las frecuencias esperadas sean bajas)
##################
#Supuestos
#en Fisher sólo hace falta que haya independencia entre las obs.
#Solo cuando se puede formar una tabla de contingencia 2x2
#No es supuesto pero usar cuando Freq Esp < 5

#Colapsando categorías
Arthritis$Improved2 <- ifelse(Arthritis$Improved == "None", "None", "Some or Marked")

# Crear una tabla de contingencia para Treatment y Improved
contingency2x2<- table(Arthritis$Treatment, Arthritis$Improved2)
print(contingency2x2)

# Realizar la prueba exacta de Fisher
fisher_result <- fisher.test(contingency2x2)
print(fisher_result)

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

pearson_test <- cor.test(mtcars$wt, mtcars$mpg, method="pearson")
print(pearson_test)

# Prueba de correlación de Spearman
spearman_test <- cor.test(mtcars$wt, mtcars$mpg, method="spearman")
print(spearman_test)