install.packages("psych")
install.packages("polycor")
install.packages("likert")

library(psych)
library(polycor)
library(corrplot)
library(likert)


# 4.1 ANÁLISIS DESCRIPTIVO DE LA MUESTRA

# Cargar datos
datos <- read.csv("respuestas.csv", sep = ",", header = TRUE)

# Estructura del dataset
str(datos)

# Resumen general
summary(datos)

names(datos)


# Renombrar variables
names(datos) <- c(
  "timestamp",
  "edad",
  "genero",
  "estudios",
  "relacion_club",
  "frecuencia",
  "anios_siguiendo",
  "motivos",
  "id_emocional",
  "id_identidad",
  "id_orgullo",
  "id_vinculo",
  "id_valores",
  "id_estabilidad",
  "id_mas_alla",
  "sat_rendimiento",
  "sat_proyecto",
  "sat_confianza",
  "sat_planificacion",
  "sat_gestion",
  "sat_fichajes",
  "sat_estabilidad",
  "sat_global",
  "fid_intencion",
  "fid_independiente",
  "fid_regularidad",
  "fid_fiel",
  "fid_recomendacion",
  "observaciones"
)

# Eliminar columna timestamp
datos <- datos[ , -1]


# todos los barplots entrarán aquí

par(mfrow = c(2, 3),
    mar = c(6, 4, 3, 1))  # más espacio vertical



# Género + Gráfico

tabla_genero <- table(datos$genero)
prop_genero <- prop.table(tabla_genero)*100

tabla_genero
round(prop_genero, 2)

barplot(tabla_genero,
        main = "Distribución por género",
        ylab = "Frecuencia",
        xlab = "Género",
        las = 1)

# Edad + Gráfico

tabla_edad <- table(datos$edad)
prop_edad <- prop.table(tabla_edad)*100

tabla_edad
round(prop_edad, 2)

barplot(tabla_edad,
        main = "Distribución por edad",
        ylab = "Frecuencia",
        xlab = "Edad",
        las = 1)

# Nivel de estudios + Gráfico

tabla_estudios <- table(datos$estudios)
prop_estudios <- prop.table(tabla_estudios)*100

tabla_estudios
round(prop_estudios, 2)

barplot(tabla_estudios,
        main = "Nivel de estudios",
        ylab = "Frecuencia",
        las = 1)  # rota etiquetas (importante aquí)

# Relación con el club + Gráfico

tabla_relacion <- table(datos$relacion_club)
prop_relacion <- prop.table(tabla_relacion)*100

tabla_relacion
round(prop_relacion, 2)

barplot(tabla_relacion,
        main = "Relación con el club",
        ylab = "Frecuencia",
        las = 1,
        names.arg = ifelse(names(tabla_relacion) == "Ni socio/a ni simpatizante de ningún club",
                           "Ni socio/a ni simpatizante",
                           names(tabla_relacion)))

# Frecuencia de seguimiento + Gráfico

tabla_frecuencia <- table(datos$frecuencia)
prop_frecuencia <- prop.table(tabla_frecuencia)*100

tabla_frecuencia
round(prop_frecuencia, 2)

barplot(tabla_frecuencia,
        main = "Frecuencia de seguimiento",
        ylab = "Frecuencia",
        las = 1)


# Años siguiendo + Gráfico

tabla_anios <- table(datos$anios_siguiendo)
prop_anios <- prop.table(tabla_anios) * 100

tabla_anios
round(prop_anios, 2)

barplot(tabla_anios,
        main = "Años siguiendo al club",
        ylab = "Frecuencia",
        las = 1)

# Resetear ventana gráfica

dev.off()



# 4.2 ANÁLISIS DESCRIPTIVO DE LAS VARIABLES

# Hay que pasar previamente las variables a numéricas

datos[,8:28] <- lapply(datos[,8:28], as.numeric)

# MEDIA

colMeans(datos[,8:28], na.rm = TRUE)

# DESVIACIÓN TÍPICA

apply(datos[,8:28], 2, sd, na.rm = TRUE)

# MEDIANA

apply(datos[,8:28], 2, median, na.rm = TRUE)

# PORCENTAJES DE RESPUESTA EN CADA ÍTEM

tabla_likert <- lapply(datos[,8:28], function(x) {
  round(prop.table(table(x)) * 100, 2)
})

tabla_likert





# 4.3 MATRIZ DE CORRELACIONES POLICÓRICAS

# Vamos a usar solo los datos de las preguntas de escala Likert
datos_likert <- datos[,8:27]

# Matriz de correlaciones policóricas
cor_poly <- polychoric(datos_likert)

# Valores de la matriz
cor_poly$rho

# Visualizar matriz gráficamente
corrplot(cor_poly$rho, method="color", tl.cex=0.6)





# 4.4 ANÁLISIS FACTORIAL EXPLORATORIO

KMO(datos[,8:27])

cortest.bartlett(datos[,8:27])

fa.parallel(datos[,8:27], fa="fa")

fa_result <- fa(datos[,8:27], nfactors=3, rotate="oblimin")
print(fa_result$loadings, cutoff=0.3)







# Gráficos Likert

# Identificación

identificacion <- datos[, c(
  "id_emocional",
  "id_identidad",
  "id_orgullo",
  "id_vinculo",
  "id_valores",
  "id_estabilidad",
  "id_mas_alla"
)]

colnames(identificacion) <- c(
  "Unión emocional",
  "Parte de mi identidad",
  "Orgullo de pertenencia",
  "Vínculo emocional",
  "Valores compartidos",
  "Estabilidad como afición",
  "Más allá de resultados"
)

identificacion[] <- lapply(
  identificacion,
  factor,
  levels = 1:5,
  ordered = TRUE
)

identificacion_likert <- likert(identificacion)

plot(
  identificacion_likert,
  centered = TRUE,
  group.order = colnames(identificacion_likert$items),
  wrap = 50,
  digits = 1
) +
  ggtitle("Distribución de respuestas de los ítems de identificación") +
  ylab("Porcentaje") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Respuesta"))



# Satisfacción

satisfaccion <- datos[, c(
  "sat_rendimiento",
  "sat_proyecto",
  "sat_confianza",
  "sat_planificacion",
  "sat_gestion",
  "sat_fichajes",
  "sat_estabilidad",
  "sat_global"
)]

colnames(satisfaccion) <- c(
  "Rendimiento deportivo",
  "Proyecto deportivo",
  "Confianza en la directiva",
  "Planificación deportiva",
  "Gestión deportiva",
  "Política de fichajes",
  "Estabilidad deportiva",
  "Satisfacción global"
)

satisfaccion[] <- lapply(
  satisfaccion,
  factor,
  levels = 1:5,
  ordered = TRUE
)

satisfaccion_likert <- likert(satisfaccion)

plot(
  satisfaccion_likert,
  centered = TRUE,
  group.order = colnames(satisfaccion_likert$items),
  wrap = 50,
  digits = 1
) +
  ggtitle("Distribución de respuestas de los ítems de satisfacción") +
  ylab("Porcentaje") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Respuesta"))


# Fidelidad

fidelidad <- datos[, c(
  "fid_intencion",
  "fid_independiente",
  "fid_regularidad",
  "fid_fiel",
  "fid_recomendacion"
)]

colnames(fidelidad) <- c(
  "Intención de seguir",
  "Independiente de resultados",
  "Seguimiento regular",
  "Aficionado fiel",
  "Recomendación"
)

fidelidad[] <- lapply(
  fidelidad,
  factor,
  levels = 1:5,
  ordered = TRUE
)

fidelidad_likert <- likert(fidelidad)

plot(
  fidelidad_likert,
  centered = TRUE,
  group.order = colnames(fidelidad_likert$items),
  wrap = 50,
  digits = 1
) +
  ggtitle("Distribución de respuestas de los ítems de fidelidad") +
  ylab("Porcentaje") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Respuesta"))
