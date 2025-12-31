import numpy as np
import pandas as pd

# Semilla para reproducibilidad
np.random.seed(42)

# Número de observaciones por caso
n = 100

# Parámetros del modelo
beta_0 = 5000          # horas hasta SoH = 80%
beta_1 = 1.5 / 100     # impacto por kg
beta_2 = 0.8 / 100     # impacto por metro
sigma = 200            # desviación del error

# Función para simular un escenario
def simular_caso(peso_min, peso_max, dist_min, dist_max, etiqueta):
    peso = np.random.uniform(peso_min, peso_max, n)
    distancia = np.random.uniform(dist_min, dist_max, n)
    error = np.random.normal(0, sigma, n)

    vida_util = (
        beta_0
        - beta_1 * peso
        - beta_2 * distancia
        + error
    )

    return pd.DataFrame({
        "Caso": etiqueta,
        "Peso_kg": peso,
        "Distancia_m": distancia,
        "Vida_util_horas": vida_util
    })

# Simulación de los 3 casos
caso_1 = simular_caso(
    700, 1000, 300, 600,
    "Distancia larga - Peso alto"
)

caso_2 = simular_caso(
    700, 1000, 50, 150,
    "Distancia corta - Peso alto"
)

caso_3 = simular_caso(
    100, 300, 50, 150,
    "Distancia corta - Peso bajo"
)

# Dataset final

datos = pd.concat([caso_1, caso_2, caso_3], ignore_index=True)


# Evitar valores negativos de vida útil
datos["Vida_util_horas"] = datos["Vida_util_horas"].clip(lower=0)

# Mostrar primeras filas
print(caso_1.tail())
print(caso_2.tail())
print(caso_3.tail())

#Guardar datos
caso_1.to_csv("Datos_Caso_1.csv")
caso_2.to_csv("Datos_Caso_2.csv")
caso_3.to_csv("Datos_Caso_3.csv")