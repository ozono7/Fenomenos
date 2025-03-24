# Problema 6: Cálculo de la caída de presión en una tubería horizontal
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30
# Descripción: Este código calcula la caída de presión en una tubería horizontal
# que transporta aceite lubricante. 

# Paso 1: Definir las constantes y variables
Q <- 0.8                     # Caudal volumétrico en ft^3/s
L <- 1000                    # Longitud de la tubería en ft
D <- 0.5054                  # Diámetro interno de la tubería en ft (6 pulgadas cedula 40)
S <- 0.2006                  # Área transversal de la tubería en ft^2
mu <- 0.07889                # Viscosidad dinámica del aceite en lb/(ft·s)
gc <- 32.2                   # Factor de conversión gravitacional en (lb·ft)/(lb·s^2)
gamma <- 0.82                # Peso específico del aceite (adimensional)

# Paso 2: Calcular la velocidad del flujo (U)
# U = Q / S
U <- Q / S                   # Velocidad del flujo en ft/s

# Paso 3: Calcular la caída de presión (ΔP)
# ΔP = (32 * U * mu * L) / (gc * D^2)
delta_P <- (32 * U * mu * L) / (gc * D^2)  # Caída de presión en lb/ft^2

# Paso 4: Mostrar el resultado
print(paste("La caída de presión en la tubería es:", round(delta_P, 4), "lb/ft^2"))

# Paso 5: Convertir la caída de presión a psi (opcional)
# 1 psi = 144 lb/ft^2
delta_P_psi <- delta_P / 144  # Caída de presión en psi
print(paste("La caída de presión en la tubería es:", round(delta_P_psi, 4), "psi"))

