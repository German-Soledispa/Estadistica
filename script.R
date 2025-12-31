#CASO_1: Distancia larga - Peso alto
#No se presentan valores atipicos
#Al realizar la prueba nuestro p_value = 0.01782, por lo tanto no es Normal

datos <- read.csv("Datos_Caso_1.csv")
head(datos)
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida_util_hora ",
     col = "lightgreen")

qqnorm(datos$Vida_util_horas)
qqline(datos$Vida_util_horas, col = "red")

shapiro.test(datos$Vida_util_horas)

#CASO_2: Distancia larga - Peso alto
#Se presentan valores atipicos por lo tanto se acota nuestros datos, para evitar errores
#Al realizar la prueba nuestro p_value = 0.3711, por lo tanto es Normal

datos <- read.csv("Datos_Caso_2.csv")
head(datos)
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida_util_hora ",
     col = "lightgreen")

Q1 <- quantile(datos$Vida_util_horas, 0.25)
Q3 <- quantile(datos$Vida_util_horas, 0.75)
IQR_val <- IQR(datos$Vida_util_horas)

lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val
datos_sin_outliers <- datos$Vida_util_horas[
  datos$Vida_util_horas >= lim_inf &
    datos$Vida_util_horas <= lim_sup
]

hist(datos_sin_outliers,
     main = "Histograma de la degradación",
     xlab = "Vida_util_hora ",
     col = "lightgreen")

qqnorm(datos_sin_outliers)
qqline(datos_sin_outliers, col = "red")

shapiro.test(datos_sin_outliers)

#CASO_3: Distancia larga - Peso alto
#Se presentan valores atipicos por lo tanto se acota nuestros datos, para evitar errores
#Al realizar la prueba nuestro p_value = 0.9881, por lo tanto es Normal

datos <- read.csv("Datos_Caso_3.csv")
head(datos)
hist(datos$Vida_util_horas,
     main = "Histograma de la degradación",
     xlab = "Vida_util_hora ",
     col = "lightgreen")

Q1 <- quantile(datos$Vida_util_horas, 0.25)
Q3 <- quantile(datos$Vida_util_horas, 0.75)
IQR_val <- IQR(datos$Vida_util_horas)

lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val
datos_sin_outliers <- datos$Vida_util_horas[
  datos$Vida_util_horas >= lim_inf &
    datos$Vida_util_horas <= lim_sup
]

hist(datos_sin_outliers,
     main = "Histograma de la degradación",
     xlab = "Vida_util_hora ",
     col = "lightgreen")

qqnorm(datos_sin_outliers)
qqline(datos_sin_outliers, col = "red")

shapiro.test(datos_sin_outliers)
