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

caso1$SOH. <- NULL

View(caso1)

round(cor(caso1), 2)
round(cor(caso1[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(caso1[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(caso1$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(caso1$Ciclo)
qqline(caso1$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(caso1$Ciclo)

ciclo <- caso1$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = caso1)
summary(modelo1)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo1)

# Modelo2 de regresión
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom + energia_consumida_prom, data = caso1)
summary(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

# Modelo3 de regresión
modelo3 <- lm(Ciclo ~ peso_prom + dist_prom, data = caso1)
summary(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo3)

#Verificamos cual es el modelo optimo
AIC(modelo1,modelo2,modelo3)


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

Anova(modelo2)

# =====================================================
# CASO 2: Distancia corta – Peso alto
# =====================================================

# Cargar datos
caso2 <- read.csv("data2.csv")

caso2$SOH. <- NULL

View(caso2)

round(cor(caso2), 2)
round(cor(caso2[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(caso2[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(caso2$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(caso2$Ciclo)
qqline(caso2$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(caso2$Ciclo)

ciclo <- caso2$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = caso2)
summary(modelo1)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo1)

# Modelo2 de regresión
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom, data = caso2)
summary(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

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

Anova(modelo2)

# =====================================================
# CASO 3: Distancia corta – Peso bajo
# =====================================================

# Cargar datos
caso3 <- read.csv("data3.csv")

caso3$SOH. <- NULL

View(caso3)

round(cor(caso3), 2)
round(cor(caso3[, c("peso_prom","dist_prom","corriente_prom","temp_prom","energia_consumida_prom")]),2)
round(cor(caso3[, c("Ciclo","peso_prom","dist_prom","temp_prom")]),2)

# Histograma
par(mfrow = c(1,1))
hist(muestra$Ciclo,
     main = "Histograma de Ciclos",
     xlab = "Número de ciclos",
     col = "lightgreen")

# Normalidad visual
qqnorm(caso3$Ciclo)
qqline(caso3$Ciclo, col = "red")

# Pruebas de normalidad
shapiro.test(caso3$Ciclo)

ciclo <- caso3$Ciclo
ks.test(ciclo, "pnorm", mean = mean(ciclo), sd = sd(ciclo))

# Modelo1 de regresión 
modelo1 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + temp_prom + energia_consumida_prom, data = caso3)
summary(modelo1)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo1)

# Modelo2 de regresión 
modelo2 <- lm(Ciclo ~ peso_prom + dist_prom + corriente_prom + energia_consumida_prom, data = caso3)
summary(modelo2)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo2)

# Modelo3 de regresión 
modelo3 <- lm(Ciclo ~ peso_prom + dist_prom + energia_consumida_prom, data = caso3)
summary(modelo3)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo3)

# Modelo4 de regresión
modelo4 <- lm(Ciclo ~ peso_prom + dist_prom, data = caso3)
summary(modelo4)

# Multicolinealidad (Factor de Inflación de Varianza)
vif(modelo4)

#Verificamos cual es el modelo optimo
AIC(modelo1,modelo2,modelo3,modelo4)

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

Anova(modelo2)