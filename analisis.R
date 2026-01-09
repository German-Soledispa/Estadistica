# =====================================================
# ANALISIS DE REGRESION Y SUPUESTOS DEL MODELO LINEAL
# =====================================================

# Cargar librerías necesarias
# lmtest: pruebas de independencia y homocedasticidad
# car: diagnóstico de multicolinealidad

library(lmtest)
library(car)

#Carga de semilla
set.seed(123)  

# =====================================================
# CASO 1: Distancia larga – Peso alto
# =====================================================

# Cargar datos
caso1 <- read.csv("data1.csv")

#Calculo tamaño muestra
sigma <- sd(caso1$Ciclo)
E <- 2
Z <- 1.96
n <- ceiling((Z * sigma / E)^2)
n

muestra <- caso1[sample(nrow(caso1), n), ]

summary(muestra)

muestra$SOH. <- NULL

View(muestra)

round(cor(muestra), 2)
round(cor(muestra[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(muestra[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(muestra$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(muestra$Ciclo)
qqline(muestra$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(muestra$Ciclo)

ciclo <- muestra$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = muestra)
summary(modelo1)

# Modelo2 de regresión
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom, data = muestra)
summary(modelo2)

#Verificamos cual es el modelo optimo
AIC(modelo1,modelo2)

# Obtención de residuos del modelo
res <- residuals(modelo2)

# Normalidad de los errores (gráfico y prueba)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Linealidad y homocedasticidad (residuos vs ajustados)
plot(modelo2, which = 1)

# Normalidad de los residuos (Q-Q del modelo)
plot(modelo2, which = 2)

# Gráficos diagnósticos completos del modelo
par(mfrow = c(2,2))
plot(modelo2)

# Independencia de los errores (Durbin-Watson)
dwtest(modelo2)

# Homocedasticidad (Breusch-Pagan)
bptest(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

Anova(modelo2)

# =====================================================
# CASO 2: Distancia corta – Peso alto
# =====================================================

# Cargar datos
caso2 <- read.csv("data2.csv")

#Calculo tamaño muestra
sigma <- sd(caso2$Ciclo)
E <- 2
Z <- 1.96
n <- ceiling((Z * sigma / E)^2)
n

muestra <- caso2[sample(nrow(caso2), n), ]

summary(muestra)

muestra$SOH. <- NULL

View(muestra)

round(cor(muestra), 2)
round(cor(muestra[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(muestra[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(muestra$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(muestra$Ciclo)
qqline(muestra$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(muestra$Ciclo)

ciclo <- muestra$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = muestra)
summary(modelo1)

# Modelo2 de regresión
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom, data = muestra)
summary(modelo2)

#Verificamos cual es el modelo optimo
AIC(modelo1,modelo2)

# Obtención de residuos del modelo
res <- residuals(modelo2)

# Normalidad de los errores (gráfico y prueba)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Linealidad y homocedasticidad (residuos vs ajustados)
plot(modelo2, which = 1)

# Normalidad de los residuos (Q-Q del modelo)
plot(modelo2, which = 2)

# Gráficos diagnósticos completos del modelo
par(mfrow = c(2,2))
plot(modelo2)

# Independencia de los errores (Durbin-Watson)
dwtest(modelo2)

# Homocedasticidad (Breusch-Pagan)
bptest(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

Anova(modelo2)

# =====================================================
# CASO 3: Distancia corta – Peso bajo
# =====================================================

# Cargar datos
caso3 <- read.csv("data3.csv")

#Calculo tamaño muestra
sigma <- sd(caso3$Ciclo)
E <- 2
Z <- 1.96
n <- ceiling((Z * sigma / E)^2)
n

muestra <- caso3[sample(nrow(caso3), n), ]

summary(muestra)

muestra$SOH. <- NULL

#View(muestra)

round(cor(muestra), 2)
round(cor(muestra[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(muestra[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(muestra$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(muestra$Ciclo)
qqline(muestra$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(muestra$Ciclo)

ciclo <- muestra$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = muestra)
summary(modelo1)

# Modelo2 de regresión
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom, data = muestra)
summary(modelo2)

#Verificamos cual es el modelo optimo
AIC(modelo1,modelo2)

# Obtención de residuos del modelo
res <- residuals(modelo2)

# Normalidad de los errores (gráfico y prueba)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res)

# Linealidad y homocedasticidad (residuos vs ajustados)
plot(modelo2, which = 1)

# Normalidad de los residuos (Q-Q del modelo)
plot(modelo2, which = 2)

# Gráficos diagnósticos completos del modelo
par(mfrow = c(2,2))
plot(modelo2)

# Independencia de los errores (Durbin-Watson)
dwtest(modelo2)

# Homocedasticidad (Breusch-Pagan)
bptest(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

Anova(modelo2)