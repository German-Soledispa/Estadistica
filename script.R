# =====================================================
# CASO 1: Distancia larga – Peso alto
# =====================================================
# En este caso, la prueba de Shapiro–Wilk arroja un p-value < 0.05,
# por lo que se rechaza la hipótesis de normalidad.
# Sin embargo, la prueba de Kolmogorov–Smirnov presenta un p-value > 0.05,
# indicando que los datos pueden modelarse adecuadamente mediante
# una distribución Normal a nivel global.

datos <- read.csv("Datos_Caso_1.csv")
head(datos)

# Histograma de la vida útil de la batería
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida útil (horas)",
     col = "lightgreen")

# Gráfico Q-Q para evaluar visualmente la normalidad
qqnorm(datos$Vida_util_horas)
qqline(datos$Vida_util_horas, col = "red")

# Prueba de normalidad de Shapiro–Wilk
shapiro.test(datos$Vida_util_horas)

# Vector de vida útil
vida <- datos$Vida_util_horas

# Prueba de bondad de ajuste Kolmogorov–Smirnov para Normal
ks.test(
  vida,
  "pnorm",
  mean = mean(vida),
  sd   = sd(vida)
)

# =====================================================
# CASO 2: Distancia corta – Peso alto
# =====================================================
# Se identifican valores atípicos mediante el criterio del rango intercuartílico (IQR).
# Para evaluar la normalidad sin la influencia de dichos valores extremos,
# se realiza el análisis sobre los datos acotados.
# La prueba de Shapiro–Wilk arroja un p-value > 0.05,
# por lo que no se rechaza la hipótesis de normalidad.

datos <- read.csv("Datos_Caso_2.csv")
head(datos)

# Histograma de los datos originales
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida útil (horas)",
     col = "lightgreen")

# Cálculo de límites para detección de valores atípicos (IQR)
Q1 <- quantile(datos$Vida_util_horas, 0.25)
Q3 <- quantile(datos$Vida_util_horas, 0.75)
IQR_val <- IQR(datos$Vida_util_horas)
lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val

# Eliminación de valores atípicos
datos_sin_outliers <- datos$Vida_util_horas[
  datos$Vida_util_horas >= lim_inf &
    datos$Vida_util_horas <= lim_sup
]

# Histograma sin valores atípicos
hist(datos_sin_outliers,
     main = "Histograma de la degradación (sin valores atípicos)",
     xlab = "Vida útil (horas)",
     col = "lightgreen")

# Gráfico Q-Q
qqnorm(datos_sin_outliers)
qqline(datos_sin_outliers, col = "red")

# Prueba de Shapiro–Wilk
shapiro.test(datos_sin_outliers)

# Prueba de Kolmogorov–Smirnov sobre los datos originales
vida <- datos$Vida_util_horas
ks.test(
  vida,
  "pnorm",
  mean = mean(vida),
  sd   = sd(vida)
)

# =====================================================
# CASO 3: Distancia corta – Peso bajo
# =====================================================
# Se identifican valores atípicos mediante el criterio IQR.
# Tras acotar los datos, la prueba de Shapiro–Wilk arroja un p-value muy alto,
# indicando que los datos siguen una distribución Normal.
# La prueba de Kolmogorov–Smirnov confirma este ajuste.

datos <- read.csv("Datos_Caso_3.csv")
head(datos)

# Histograma de los datos originales
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida útil (horas)",
     col = "lightgreen")

# Detección de valores atípicos
Q1 <- quantile(datos$Vida_util_horas, 0.25)
Q3 <- quantile(datos$Vida_util_horas, 0.75)
IQR_val <- IQR(datos$Vida_util_horas)
lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val

# Datos sin valores atípicos
datos_sin_outliers <- datos$Vida_util_horas[
  datos$Vida_util_horas >= lim_inf &
    datos$Vida_util_horas <= lim_sup
]

# Histograma sin valores atípicos
hist(datos_sin_outliers,
     main = "Histograma de la degradación (sin valores atípicos)",
     xlab = "Vida útil (horas)",
     col = "lightgreen")

# Gráfico Q-Q
qqnorm(datos_sin_outliers)
qqline(datos_sin_outliers, col = "red")

# Prueba de Shapiro–Wilk
shapiro.test(datos_sin_outliers)

# Prueba de Kolmogorov–Smirnov
vida <- datos$Vida_util_horas
ks.test(
  vida,
  "pnorm",
  mean = mean(vida),
  sd   = sd(vida)
)
