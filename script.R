# =====================================================
# CASO 1: Distancia larga – Peso alto
# =====================================================

caso1 <- read.csv("dataset(largo_alto).csv")
hist(caso1$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Numeros de ciclos",
     col = "lightgreen")

qqnorm(caso1$Ciclo)
qqline(caso1$Ciclo, col = "red")

shapiro.test(caso1$Ciclo)
ciclo <- caso1$Ciclo
ks.test(
  ciclo,
  "pnorm",
  mean = mean(ciclo),
  sd   = sd(ciclo)
)

modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso1)
summary(modelo)
res <- residuals(modelo)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res) 
plot(modelo, which = 1)
plot(modelo, which = 2)

# =====================================================
# CASO_2: Distancia larga - Peso alto
# =====================================================

caso2 <- read.csv("dataset(corta_alto).csv")
hist(caso2$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Numeros de ciclos",
     col = "lightgreen")

qqnorm(caso2$Ciclo)
qqline(caso2$Ciclo, col = "red")

shapiro.test(caso2$Ciclo)
ciclo <- caso2$Ciclo
ks.test(
  ciclo,
  "pnorm",
  mean = mean(ciclo),
  sd   = sd(ciclo)
)

modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso2)
summary(modelo)
res <- residuals(modelo)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res) 
plot(modelo, which = 1)
plot(modelo, which = 2)


# =====================================================
# CASO 3: Distancia corta – Peso bajo
# =====================================================

caso3 <- read.csv("dataset(corta_bajo).csv")
hist(caso3$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Numeros de ciclos",
     col = "lightgreen")

qqnorm(caso3$Ciclo)
qqline(caso3$Ciclo, col = "red")

shapiro.test(caso3$Ciclo)
ciclo <- caso3$Ciclo
ks.test(
  ciclo,
  "pnorm",
  mean = mean(ciclo),
  sd   = sd(ciclo)
)

modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = caso3)
summary(modelo)
res <- residuals(modelo)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res) 
plot(modelo, which = 1)
plot(modelo, which = 2)