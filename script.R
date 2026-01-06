datosPrueba <- read.csv("pruebatesttanteo3.csv")
hist(datosPrueba$Ciclo,
     main = "Histograma de la degradación",
     xlab = "Numeros de ciclos",
     col = "lightgreen")

qqnorm(datosPrueba$Ciclo)
qqline(datosPrueba$Ciclo, col = "red")

shapiro.test(datosPrueba$Ciclo)
ciclo <- datosPrueba$Ciclo
ks.test(
  ciclo,
  "pnorm",
  mean = mean(ciclo),
  sd   = sd(ciclo)
)

modelo <- lm(Ciclo ~ peso_prom + dist_prom + temp_prom, data = datosPrueba)
summary(modelo)
res <- residuals(modelo)
qqnorm(res)
qqline(res, col = "red")
shapiro.test(res) 
plot(modelo, which = 1)
plot(modelo, which = 2)