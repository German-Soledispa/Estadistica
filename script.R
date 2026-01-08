# =====================================================
# ANALISIS DE REGRESION Y SUPUESTOS DEL MODELO LINEAL
# =====================================================

# Cargar librerías necesarias
# lmtest: pruebas de independencia y homocedasticidad
# car: diagnóstico de multicolinealidad

library(lmtest)
library(car)

# =====================================================
# CASO 1: Distancia larga – Peso alto
# =====================================================

# Cargar datos
caso1 <- read.csv("dataset(largo_alto).csv")

# Histograma de la variable respuesta (Ciclo)
hist(caso1$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Evaluación visual de normalidad (Q-Q plot)
qqnorm(caso1$Ciclo)
qqline(caso1$Ciclo, col = "red")

# Prueba de normalidad sobre la variable respuesta
shapiro.test(caso1$Ciclo)

# Prueba Kolmogorov–Smirnov para normalidad
ciclo <- caso1$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Ajuste del modelo de regresión lineal múltiple
modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso1)
summary(modelo)

# Obtención de residuos del modelo
res <- residuals(modelo)

# Normalidad de los errores (gráfico y prueba)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Linealidad y homocedasticidad (residuos vs ajustados)
plot(modelo, which = 1)

# Normalidad de los residuos (Q-Q del modelo)
plot(modelo, which = 2)

# Gráficos diagnósticos completos del modelo
par(mfrow = c(2,2))
plot(modelo)

# Independencia de los errores (Durbin-Watson)
dwtest(modelo)

# Homocedasticidad (Breusch-Pagan)
bptest(modelo)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo)

# =====================================================
# CASO 2: Distancia corta – Peso alto
# =====================================================

# Cargar datos
caso2 <- read.csv("dataset(corta_alto).csv")

# Histograma de la variable respuesta
hist(caso2$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Evaluación visual de normalidad
qqnorm(caso2$Ciclo)
qqline(caso2$Ciclo, col = "red")

# Prueba de normalidad
shapiro.test(caso2$Ciclo)

# Kolmogorov–Smirnov
ciclo <- caso2$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo de regresión
modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso2)
summary(modelo)

# Residuos
res <- residuals(modelo)

# Normalidad de residuos
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Diagnósticos gráficos
plot(modelo, which = 1)
plot(modelo, which = 2)

par(mfrow = c(2,2))
plot(modelo)

# Supuestos del modelo
dwtest(modelo)
bptest(modelo)
vif(modelo)

# =====================================================
# CASO 3: Distancia corta – Peso bajo
# =====================================================

# Cargar datos
caso3 <- read.csv("dataset(corta_bajo).csv")

# Histograma
hist(caso3$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(caso3$Ciclo)
qqline(caso3$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(caso3$Ciclo)

ciclo <- caso3$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo
modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso3)
summary(modelo)

# Residuos
res <- residuals(modelo)

# Normalidad de errores
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Diagnósticos
plot(modelo, which = 1)
plot(modelo, which = 2)

par(mfrow = c(2,2))
plot(modelo)

# Verificación de supuestos
dwtest(modelo)
bptest(modelo)
vif(modelo)

# =====================================================
# CASO DE PRUEBA
# =====================================================

# Cargar datos de prueba
prueba <- read.csv("prueba3.csv")

# Histograma
hist(prueba$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(prueba$Ciclo)
qqline(prueba$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(prueba$Ciclo)

ciclo <- prueba$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo de regresión
modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = prueba)
summary(modelo)

# Residuos
res <- residuals(modelo)

# Normalidad de errores
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Diagnósticos del modelo
plot(modelo, which = 1)
plot(modelo, which = 2)

par(mfrow = c(2,2))
plot(modelo)

# Evaluación de supuestos
dwtest(modelo)
bptest(modelo)
vif(modelo)


