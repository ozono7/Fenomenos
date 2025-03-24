# Problema 8: Cálculo de la velocidad de flujo en estado estacionario en un tubo horizontal
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30
# Descripción: Este código calcula la velocidad de flujo en estado estacionario de asfalto
# en un tubo circular horizontal. 

# Paso 1: Definir las constantes y variables
delta_P <- 144              # Gradiente de presión en lb/ft^2 (1 psi/ft)
gc <- 32.2                  # Factor de conversión gravitacional en (lb·ft)/(lb·s^2)
D <- 0.1722                 # Diámetro interno de la tubería en ft (2 pulgadas)
mu <- 67.2                  # Viscosidad dinámica del asfalto en lb/(ft·s)
L <- 1                      # Longitud de la tubería en ft (gradiente de presión por ft)
S <- 0.02330                # Área transversal de la tubería en ft^2

# Paso 2: Calcular la velocidad del flujo (U)
# U = (ΔP * gc * D^2) / (32 * L * μ)
U <- (delta_P * gc * D^2) / (32 * L * mu)  # Velocidad del flujo en ft/s

# Paso 3: Calcular el caudal volumétrico (Q)
# Q = U * S
Q <- U * S                  # Caudal volumétrico en ft^3/s

# Paso 4: Mostrar los resultados
print(paste("La velocidad del flujo es:", round(U, 5), "ft/s"))
print(paste("El caudal volumétrico es:", round(Q, 5), "ft^3/s"))

# Paso 5: Convertir el caudal volumétrico a galones por minuto (opcional)
# 1 ft^3/s = 448.831 gal/min
Q_gal_min <- Q * 448.831    # Caudal volumétrico en gal/min
print(paste("El caudal volumétrico es:", round(Q_gal_min, 5), "gal/min"))

