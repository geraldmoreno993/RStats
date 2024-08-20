rm(list =ls())
setwd("/home/gerald/Documentos/Clases_propias/curso_2/data_final")
getwd()
############################################################
# Nombre del Script: bioestadisica_flyteek_parte1.R
# Descripción: "Bioestadística con R parte I"
# Autor: Gerald Moreno Morales
# Fecha: 18/08/2024
# Referencias: https://upch-r4pubh.netlify.app/referencias#ref-rosner2015
############################################################

################################################## 
#              Tipos de objetos en R             #
##################################################

numeric_vector <- c(1, 2, 3, 4)
print(numeric_vector)
class(numeric_vector)

char_vector <- c("a", "b", "c")
print(char_vector)
class(char_vector)

gender <- factor(c("male", "female", "female", "male"))
print(gender)

matrix_data <- matrix(1:9, nrow=3, ncol=3)
print(matrix_data)
print(matrix_data)

################################################## 
#                   Dataframes                   #
##################################################

iris
my_dataframe <- iris
class(my_dataframe)


#Describiendo dataframes

str(my_dataframe)
summary(my_dataframe)

install.packages("summarytools")
library(summarytools)
dfSummary(my_dataframe)

install.packages("psych")
library(psych)
describe(my_dataframe)

#Importando dataframes(penguins.csv)

setwd("/home/gerald/Documentos/Clases_propias/curso_2/data_final")
getwd()

penguins <- read.csv("penguins.csv")
class(penguins)
str(penguins)

# Convertir todas las columnas de tipo carácter a factores usando dplyr
install.packages("dplyr")
library(dplyr)
penguins <- penguins %>% mutate_if(is.character, as.factor)


# Ver el tipo de cada columna
str(penguins)
# Explorar las primeras filas
head(penguins)

penguins <- read.csv("penguins.csv")


#Paquetes que se usaran el dia de hoy


install.packages("tidyverse")
install.packages("rstatix")
install.packages("BSDA")
install.packages("DescTools")

library(tidyverse)
library(rstatix)
library(BSDA)
library(DescTools)


################################################## 
#      T-test para muestras independientes       #
################################################## 

#Dataset a usar: lead.csv

# Efectos de la exposición al plomo en la función neurológica y psicológica de los niños
# Landrigan et al. (1975) estudiaron los efectos de la exposición a plomo en el bienestar psicológico y neurológico en niños. 
# Se midió el nivel de plomo en la sangre de niños que vivían cerca a lugares donde se realizaba fundición de plomo en El Paso, Texas. 
# 124 niños entraron al estudio, los cuales fueron separados en 2 grupos según su nivel de plomo en la sangre. 
#En el primer grupo se incluyó a niños con niveles de plomo en la sangre
# <40ug/100ml (66 casos), mientras que en el segundo a niños con niveles de plomo en la sangre
# >40g/100ml (34 casos). 
# El puntaje IQ de escala completa de Wechsler se midió como variable dependiente.
# El data set de este estudio fue recuperado de Rosner (2015) y se encuentra en el archivo lead.csv

lead <- read_csv("lead.csv")   #Importar data mediante "readr" o "tidyverse"
str(lead)

# el objetivo era evaluar si en el grupo más expuesto al plomo los indicadores psicológicos y 
#neurológicos eran peores que el grupo menos expuesto. 
# En la data, la variable de agrupación es la variable "group_fct"
# La variable IQ es la variable "iqf"

#Cambiar variables categóricas a factores
lead <- lead %>% mutate(group_fct = factor(Group)) 

# Boxplot moderno
lead %>%  ggplot(aes(group_fct, iqf)) + geom_boxplot() + 
  ggtitle("Boxplot de IQF por Grupo") + 
  theme(plot.title = element_text(hjust = 0.5))


# Boxplot clasico
boxplot(lead$iqf ~ lead$group_fct,
        main = "Boxplot de IQF por Grupo",
        xlab = "Grupo",
        ylab = "IQF",
        col = "lightblue")


# Usar describeBy() para obtener estadísticas descriptivas por grupo
df_summary <- describeBy(lead$iqf, group = lead$group_fct, mat = TRUE)
print(df_summary)

#Prueba t de Student para muestras independientes

# Definir hipotesis estadisticas:
# Hipótesis nula (Ho): La medias de ambas poblaciones son iguales (μ1=μ2).
# Hipótesis alternativa (Ha): La medias de ambas poblaciones no son iguales (μ1≠μ2)

#Supuestos:
# Variables independientes.
# Distribución de la muestra aproximadamente normal.
# Varianzas poblacionales desconocidas.
# Varianzas de las muestras iguales, se asume si n < 50 en cada grupo.
# Usar Welch T-test si... varianzas de las muestras diferentes, se asume si n > 50 en cada grupo.

#Evaluando normalidad
lead %>% 
  ggplot(aes(sample = iqf)) +
  facet_wrap(vars(group_fct)) +
  stat_qq() +
  stat_qq_line()

var.test(iqf ~ group_fct, data = lead)

#Prueba de hipótesis
t.test(iqf ~ group_fct, alternative = "two.sided", mu = 0, var.equal = TRUE, data = lead)
#p-value = 0.1259
#Conclusión: No hay diferencias significativas

################################################## 
#            Prueba U de Mann-Whitney            #
##################################################

# Hipotesis:
# Ho: la mediana de las diferencias poblacionales es igual cero 
# Ha: la mediana de las diferencias poblacionales es diferente de cero

#Supuestos:
# Variables independientes.
# Distribución no normal.
#"Tamaño de muestra pequeña".

wilcox.test(iqf ~ group_fct, alternative = "two.sided", mu = 0, data = lead)

#p-value = 0.1281
#Conclusión: No hay diferencias significativas


################################################## 
#          T-test para muestras pareadas         #
##################################################

#Dataset a usar: boneden.csv

# En esta sección utilizaremos el caso de estudio de los efectos del consumo
# de tabaco en la densidad ósea en gemelas. Hopper, J.L. y Seeman (1994)
# realizaron un estudio en gemelas sobre la relación entre densidad ósea y
# consumo de cigarros. Cuarenta y un pares de gemelas de mediana edad que no
# tenían el mismo hábito de consumo de tabaco fueron invitadas a un hospital en 
# Victoria, Australia, para una medición de densidad ósea. Además, se obtuvo detalles 
# acerca del uso de tabaco, consumo de alcohol, café y té, ingesta de calcio, historia menopáusica, reproductiva y de fracturas, uso de anticonceptivos orales o terapia de reemplazo de estrógenos, y se evaluo su actividad física.

# Las muestras, en este caso, son pareadas porque las gemelas tienen 
# características (genéticas, sociales, ambientales, etc.) similares, y 
# la única diferencia se está midiendo a nivel de consumo de tabaco. 
# Este tipo de pruebas también suelen ser utilizadas cuando se mide 
# múltiples veces a un individuo.


boneden <- read_csv("boneden.csv")

#Prueba T para muestras pareadas

# Definir hipotesis estadisticas:
# Ho: La medias de ambas poblaciones son iguales (μ1=μ2).
# Ha: La medias de ambas poblaciones no son iguales (μ1≠μ2)

#Supuestos:
#Variables pareadas.
#La distribución de las diferencias entre las observaciones es aproximadamente normal o tamaño de muestra es grande.
#Varianzas poblacionales desconocidas.

dfSummary(boneden$ls1)
dfSummary(boneden$ls2)

describe(boneden$ls1)
describe(boneden$ls2)

str(boneden)

#Evaluación de normalidad de la diferencia
boneden %>% 
  ggplot(aes(sample = ls2 - ls1)) +
  stat_qq() +
  stat_qq_line()

#Para un mejor manejo, necesitamos poner la data en formato largo (long) y cambiar el formato de la variable grupo a un factor.

boneden_long <- boneden %>%
  pivot_longer(cols = c("ls1", "ls2"), names_to = "group", values_to = "ls") %>%
  mutate(group = factor(group))

str(boneden_long)

#T-test pareado
t.test(ls ~ group, alternative = "two.sided", mu = 0, unpaired = FALSE, 
  data = boneden_long)
#p-value = 0.2291
#Conclusión: No hay diferencias significativas


################################################## 
#      Prueba de rango con signo de Wilcoxon     #
################################################## 

# Hipotesis:
# Ho: la mediana de las diferencias poblacionales es igual cero 
# Ha: la mediana de las diferencias poblacionales es diferente de cero 

#Supuestos:

# Muestras pareadas.
# "La distribución de las diferencias entre las observaciones no es 
# normal o el tamaño de muestra es pequeño".


wilcox.test(ls ~ group, alternative = "two.sided", mu = 0, unpaired = FALSE, 
            data = boneden_long, exact = FALSE)

#p-value = 0.3487
#Conclusión: No existen diferencias significativas en las medianas


################################################## 
#                  One Way ANOVA                 #
################################################## 
#Dataset: "lead.csv"
# Para este caso usaremos la data del caso de estudio de 
# niveles de plomo en la sangre de niños, pero usaremos 
# la variable lead_group que separa a los muestreados en 
# tres grupos.

library(psych)
library(tidyverse)


setwd("/home/gerald/Documentos/Clases_propias/curso_2")
getwd()

lead <- read_csv("lead.csv")   #Importar data mediante "readr" o "tidyverse"
str(lead)

#Cambiar variables categóricas a factores
lead <- lead %>% mutate(lead_grp_fct = factor(lead_grp)) 

lead %>%
  ggplot(aes(x = lead_grp_fct, y = iqp_cod)) +
  geom_boxplot() + 
  stat_summary(fun = mean, color = "red")

# Usar describeBy() para obtener estadísticas descriptivas por grupo
df_summary_1 <- describeBy(lead$iqp_cod, group = lead$lead_grp_fct, mat = TRUE)
print(df_summary_1)

#ANOVA:

#Ho: Las medias de IQ de las 3 poblaciones (categorías) son iguales 
#Ha: Al menos dos medias poblacionales son distintas

#Supuestos

#Supuestos son en base a los residuos:
  
# Más de 2 muestras aleatorias simples.
# La variable dependiente tiene una distribución normal en cada población.
# La varianza de la variable dependiente es igual en todas las poblaciones (homocedasticidad).


iqp_aov <- aov(iqp_cod ~ lead_grp_fct, data = lead)
summary(iqp_aov)

install.packages("ggfortify")
library(ggfortify)

#Supuestos
autoplot(iqp_aov)

# Se observa que los residuales parecen cumplir con 
# los supuestos de normalidad y homocedasticidad anteriormente 
# comentados.

#con rstatix ejecutar prueba posthoc:

lead %>% tukey_hsd(iqp_cod ~ lead_grp_fct, p.adjust.method = "bonferroni")

################################################## 
#            Prueba de Kruskall-Wallis           #
##################################################

#Dataset: "lead.csv" lead transformado en el ejercicio anterior

#En caso no se cumpla alguno de los supuestos de ANOVA
#Ho: Las distribuciones de IQ en los grupos son iguales.
#Ha: Las distribuciones de IQ en los grupos son diferentes.

lead %>% 
  kruskal_test(iqp_cod ~ lead_grp_fct)

lead %>% 
  dunn_test(iqp_cod ~ lead_grp)


###################################################### 
#  Prueba de Chi-cuadrado y prueba exacta de Fisher  #
######################################################

#Dataset: "alelos.csv"

# Se quiere evaluar la asociacion de ciertos SNPs con la
# presencia de cierta condicion patologica.

setwd("/home/gerald/Documentos/Clases_propias/curso_2/data_final")
snps <- read_csv("alelos.csv")

str(snps)
snps <- snps %>% mutate_if(is.character, as.factor)

#Describiendo data
tab1 <- table(snps$group, snps$SNP1)
print(tab1)
prop.table(tab1) #*100

# Crear tablas cruzadas para cada SNP
snp_columns <- names(snps)[names(snps) != "group"]

# Crear tablas cruzadas para cada SNP
tablas_cruzadas <- lapply(snp_columns, function(snp) {
  table(snps$group, snps[[snp]])
})

print(tablas_cruzadas)


#Chi-cuadrado de independencia

#Ho: Las variables group y SNP son independientes
#Ha: Las variables group y SNP están asociadas

#Supuestos

#1) Variables categóricas
#2) Tamaño de muestra mayor a 20 (ntotal > 20)
#3) Como máximo, el 20%  de celdas pueden tener menos de 5 observaciones esperadas
#... Si no se cumple, realizar la prueba exacta de Fisher 


tab1 <- table(snps$group, snps$SNP1)
print(tab1)

# Realizar el test chi-cuadrado
chi_square_test <- chisq.test(tab1)
print(chi_square_test)

#Evaluo frecuencias esperadas
print(chi_square_test$expected)


#Prueba exacta de Fisher
#Colapsando categorías
snps$SNP1_collapse <- ifelse(snps$SNP1 == "C/C", "Ausencia de A", "Presencia de A")

# Crear una tabla de contingencia para Treatment y Improved
contingency2x2<- table(snps$group, snps$SNP1_collapse)
print(contingency2x2)

# Realizar la prueba exacta de Fisher
fisher_result <- fisher.test(contingency2x2)
print(fisher_result)


#Prueba exacta de Fisher
#Colapsando categorías
snps$SNP1_collapse <- ifelse(snps$SNP1 == "C/C", "Ausencia de A", "Presencia de A")

# Crear una tabla de contingencia para Treatment y Improved
contingency2x2<- table(snps$group, snps$SNP1_collapse)
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

#test
pearson_test <- cor.test(mtcars$wt, mtcars$mpg, method="pearson")
print(pearson_test)
#el valor de p nos indica que el valor de rho es significativo


# Prueba de correlación de Pearson
##################################
#Supuestos
#Variables al menos ordinales
#Observaciones independientes
#Relación monotónica (se observa alguna tendencia a la linealidad)

pearson_test <- cor.test(mtcars$wt, mtcars$mpg, method="pearson")
print(pearson_test)

# Prueba de correlación de Spearman
#minimo las variables deben estar en escala ordinal
spearman_test <- cor.test(mtcars$wt, mtcars$mpg, method="spearman")
print(spearman_test)