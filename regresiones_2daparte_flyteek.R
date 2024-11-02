setwd("/home/gerald/Documentos/Clases_propias/bioestadistica_version2.0")

############################################################
# Nombre del Script: regresiones_flyteek_parte2.R
# Descripción: "Bioestadística con R parte II"
# Autor: Gerald Moreno Morales
# Fecha: 25/10/2024
# Referencias: ver paquetes
############################################################

################################################## 
#            Regresion lineal simple             #
##################################################
#Supuestos
#1) Relación Lineal entre las dos variables
#2) No Presencia de Outliers
#3) Homocedasticidad de residuos 
#4) Normalidad de los residuos
#5) Independencia de los residuos
#Pregunta: ¿Cuánto es el efecto de variable independiente Velocidad (mph) con una 
#variable dependiente Distancia de frenado (pies)?

#Ho : β1 = 0
#Ha : β1 != 0
# Cargar el dataset cars
data(cars)
plot(cars)
# Ajustar un modelo de regresión lineal
modelo <- lm(dist ~ speed, data = cars)

# Resumen del modelo
summary(modelo)

# Graficar los datos y la línea de regresión
plot(cars$speed, cars$dist, 
     main = "Relación entre velocidad y distancia de frenado",
     xlab = "Velocidad (mph)", 
     ylab = "Distancia de frenado (pies)",
     pch = 16, col = "blue")
abline(modelo, col = "red", lwd = 2)






################################ 
#         Chi cuadrado         #
################################

#Supuestos

#1) Variables categóricas
#2) Tamaño de muestra mayor a 20 (ntotal > 20)
#3) No más del 20% de los valoes esperados debe ser < 5 y ninguno es = 0
#Si no se cumple, realizar la prueba exacta de Fisher 



#Ho: Las variables genotipo y fenotipo son independientes (No hay asociación)
#Ha: Las variables genotipo y fenotipo estan asociadas
#Se desea evaluar la asociación de diversos genes de resistencia con el fenotipo


####Pregunta: "¿Existe una asociación significativa entre genotipo (gen mecA) y fenotipo (hospedero)?"



library(tidyverse)
staphy <- read_csv("staphy_final.csv")
str(staphy)

# Convertir todas las columnas character a factor
staphy<- staphy %>%
  mutate(across(1:ncol(.), as.factor))


#Lo que se desea hacer es evaluar las asociación entre la presencia de genes del resistoma
#vs el hospedero
table(staphy$Host)
staphy$Host <- ifelse(staphy$Host == "Human", 0, 1)
str(staphy)

staphy<- staphy %>%
  mutate(across(1:ncol(.), as.factor))


#describiendo

tab <- table(staphy$mecA,staphy$Host)  
print(tab)
chi <- chisq.test(tab)
print(chi)
chi$expected
fisher.test(tab)

resultado_fisher <- fisher.test(tab)
# Extraer el OR y el intervalo de confianza
odds_ratio <- resultado_fisher$estimate
intervalo_confianza <- resultado_fisher$conf.int

# Mostrar resultados
cat("Odds Ratio:", odds_ratio, "\n")
cat("Intervalo de Confianza (95%):", intervalo_confianza, "\n")



# Graficar el diagrama de barras
# Graficar el diagrama de barras con los colores y leyenda personalizados
barplot(tab, beside = FALSE, 
        legend.text = c("mecA-", "mecA+"),  # Nombres personalizados
        names.arg = c("Humano", "Animal"),
        main = "Distribución de mecA por Host",
        xlab = "Host",
        ylab = "Frecuencia",
        col = c("white", "darkgray"))  # Colores personalizados

############################################### 
#             Regresión logística             #
###############################################



#Supuestos
#1) Variable dependiente binaria
#2) Independencia de las observaciones
#4) Relación lineal entre el logit de la prob. de Y, y la variable independiente
#(solo aplicar a variables numericas)

#Pregunta de investigación: Se desea evaluar la asociación de diversos genes de resistencia con el fenotipo
#hospedero

# Modelo de regresión logística
modelo0 <- glm(Host ~  mecA  , data = staphy, family = binomial)
summary(modelo0)


# Extraer los coeficientes del modelo
coeficiente0 <- coef(modelo0)

# Convertir los coeficientes a odds ratios
OR0 <- exp(coeficiente0)


# Mostrar los OR
print(OR0)


#######################################################################
###################Genome Pangenome Asociation Study###################
#######################################################################

staph_GPAS <- staphy[,3:ncol(staphy)]

# Limpiar nombres de columnas para que no tengan caracteres especiales
names(staph_GPAS) <- make.names(names(staph_GPAS))
str(staph_GPAS)
# Cargar la librería broom si no está cargada
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
library(broom)

# Filtrar genes con al menos dos niveles en el dataframe
valid_genes <- names(staph_GPAS)[-which(names(staph_GPAS) == "Host")]
valid_genes <- valid_genes[sapply(staph_GPAS[valid_genes], function(x) length(unique(x)) > 1)]

# Realizar regresión logística para cada gen en el dataframe
results <- lapply(valid_genes, function(gene) {
  formula <- as.formula(paste("Host ~", gene))
  
  # Ajustar el modelo
  model <- tryCatch({
    glm(formula, data = staph_GPAS, family = "binomial")
  }, warning = function(w) {
    message("Warning in glm for gene ", gene, ": ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in glm for gene ", gene, ": ", conditionMessage(e))
    NULL
  })
  
  # Si el modelo es válido, procesar los resultados
  if (!is.null(model)) {
    model_summary <- tidy(model, conf.int = TRUE) %>%
      mutate(OR = exp(estimate),           # Transformar coeficiente a OR
             CI_low = exp(conf.low),       # Transformar límite inferior del intervalo de confianza a OR
             CI_high = exp(conf.high),     # Transformar límite superior del intervalo de confianza a OR
             gene = gene)                  # Agregar el nombre del gen
    
    return(model_summary)
  } else {
    return(NULL)
  }
})

# Eliminar los resultados nulos (para genes con problemas en el modelo)
results_df <- bind_rows(results[!sapply(results, is.null)], .id = "gene")
library(dplyr)


# Calcular la corrección de Bonferroni
n_tests <- length(valid_genes)
results_df <- results_df %>%
  mutate(bonferroni_corrected = pmin(p.value * n_tests, 1))  # Añadir la columna de Bonferroni

# Ordenar por la columna OR en orden descendente
results_df_ordenado <- results_df %>%
  arrange(desc(OR))

# Vista rápida de los primeros resultados ordenados
head(results_df_ordenado)

# Guardar los resultados ordenados en un archivo CSV
write.csv(results_df_ordenado, "resultados_ordenado_GPAs.csv", row.names = FALSE)

# Vista rápida de los resultados
head(results_df)


# Ordenar por la columna OR en orden descendente
results_df_ordenado <- results_df %>%
  arrange(desc(OR))

# Vista rápida de los primeros resultados ordenados
head(results_df_ordenado)


write.csv(results_df_ordenado, "resultados_ordenado_GPAs_flyteek.csv", row.names = FALSE)

# Vista rápida de los resultados
head(results_df)


########################################################
##VULCANO PLOT
##########################################################
# Cargar el archivo CSV
df <- read.csv("resultados_ordenado_GPAs.csv")

# 1. Volcano Plot Original (incluyendo todos los términos)
df$logOR <- log(df$OR)
df$neg_log_p <- -log10(df$bonferroni_corrected)

# Crear volcano plot original
library(ggplot2)
volcano_original <- ggplot(df, aes(x = logOR, y = neg_log_p)) +
  geom_point(alpha = 0.7) +  # Ajusta la transparencia de los puntos
  labs(x = "log(Odds Ratio)", y = "-log10(p-value ajustado)", title = "Volcano Plot Original") +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") + # Línea de significancia
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") # Línea neutra para el OR

# Mostrar volcano plot original
print(volcano_original)

# 2. Volcano Plot Filtrado (excluyendo 'Intercept' y términos vacíos)
df_filtrado <- df[df$term != "(Intercept)" & df$term != "", ]

# Calcular log(OR) y -log10(bonferroni_corrected) para el filtrado
df_filtrado$logOR <- log(df_filtrado$OR)
df_filtrado$neg_log_p <- -log10(df_filtrado$bonferroni_corrected)

# Crear volcano plot filtrado
volcano_filtrado <- ggplot(df_filtrado, aes(x = logOR, y = neg_log_p)) +
  geom_point(alpha = 0.7) +  # Ajusta la transparencia de los puntos
  labs(x = "log(Odds Ratio)", y = "-log10(p-value ajustado)", title = "Volcano Plot Filtrado (sin Intercept)") +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") + # Línea de significancia
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") # Línea neutra para el OR

# Mostrar volcano plot filtrado
print(volcano_filtrado)




####################################################################################
# Cargar el archivo CSV
df <- read.csv("resultados_ordenado_GPAs.csv")

# Filtrar para excluir el intercepto y posibles filas con términos vacíos
df <- df[df$term != "(Intercept)" & df$term != "", ]

# Calcular log(OR) y -log10(bonferroni_corrected)
df$logOR <- log(df$OR)
df$neg_log_p <- -log10(df$bonferroni_corrected)

# Filtrar para mostrar solo los nombres de genes con valores p ajustados significativos (p < 0.05)
df_significativos <- df[df$bonferroni_corrected < 0.05, ]

# Seleccionar los 10 términos más significativos según bonferroni_corrected
top_10_significativos <- df[order(df$bonferroni_corrected), ][1:10, ]

# Crear el volcano plot
library(ggplot2)
ggplot(df, aes(x = logOR, y = neg_log_p)) +
  geom_point(alpha = 0.7) +  # Ajusta la transparencia de los puntos
  labs(x = "log(Odds Ratio)", y = "-log10(p-value ajustado)", title = "Volcano Plot con Top 10 Genes Significativos") +
  theme_minimal() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") + # Línea de significancia
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") + # Línea neutra para el OR
  
  # Resaltar los 10 genes más significativos en el volcano plot
  geom_point(data = top_10_significativos, aes(x = logOR, y = neg_log_p), color = "red", size = 3) +
  
  # Etiquetar solo los genes significativos y rotar las etiquetas 45 grados
  geom_text(data = df_significativos, aes(x = logOR, y = neg_log_p, label = term), 
            vjust = -1.2, hjust = 0.5, size = 2.5, angle = 45, color = "black", fontface = "bold", check_overlap = TRUE) # Etiquetar genes significativos










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
#install.packages("MASS")
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