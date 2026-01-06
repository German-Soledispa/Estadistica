import numpy as np
import pandas as pd

tamanio_muestra = 200

#Distana - Peso
larga_alto = [700, 1000, 300, 600,]
corta_alto = [700, 1000, 50, 150]
corta_bajo = [100, 300, 50, 150]

def DatosESt(num_ciclos, parametros):
    data = []

    # Variables acumuladas
    peso_total = 0
    distancia_total = 0
    corriente_total = 0
    energia_total = 0
    temp_total = 0
    current_soh = 100  # Estado de salud inicial (%)

    for ciclo in range(1, num_ciclos + 1):

        # 1. Generación de variables de operación
        peso_carga = np.random.uniform(parametros[2], parametros[3])      # kg
        distancia = np.random.uniform(parametros[0], parametros[1])       # metros

        peso_total += peso_carga
        distancia_total += distancia

        # 2. Modelo físico simplificado del consumo
        # A mayor peso y distancia, mayor corriente
        corriente = (peso_carga * 0.002) + (distancia * 0.001)
        corriente_total += corriente

        # 3. Modelo térmico
        temp_ambiente = 25  # °C
        temp_current = (corriente * 0.5) + np.random.normal(0, 1)
        temp_bateria = temp_ambiente + temp_current
        temp_total += temp_bateria

        # 4. Consumo de energía
        energia_consumida = corriente * (distancia / 1000)
        energia_total += energia_consumida

        # 5. Degradación del SOH
        degradacion = energia_consumida * 0.02
        current_soh -= degradacion

        # 6. Condición de paro
        if current_soh <= 80:
            peso_prom = peso_total / ciclo
            distancia_prom = distancia_total / ciclo
            corriente_prom = corriente_total / ciclo
            temp_prom = temp_total / ciclo
            energia_consumida_prom = energia_total / ciclo

            data.append({
                "Ciclo": ciclo,
                "peso_prom":round(peso_prom,2),
                "dist_prom":round(distancia_prom,2),
                "corriente_prom":round(corriente_prom,2),
                "temp_prom":round(temp_prom,2),
                "energia_consumida_prom":round(energia_consumida_prom,2),
                "SOH%":round(current_soh, 2)
            })

            break

    return data[-1]

i = 0
data_t = []
while tamanio_muestra > i: 
    dato = DatosESt(15000, corta_bajo)
    data_t.append(dato)
    i += 1

df = pd.DataFrame(data_t)
df.to_csv("pruebatesttanteo3.csv", index=False)