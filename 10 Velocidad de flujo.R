# Problema 10: Cálculo de la velocidad de flujo en una tubería de 4 pulgadas de diámetro
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30
# Descripción: Este código calcula la velocidad de flujo de petróleo crudo en una tubería
# de 4 pulgadas de diámetro, utilizando las lecturas manométricas en dos estaciones
# hidrométricas.

# Paso 1: Definir las constantes y variables
D <- 0.3355                # Diámetro interno de la tubería en ft (4 pulgadas)
L <- 50                    # Longitud entre las estaciones A y B en ft
P_A <- 25.4                # Presión en la estación A en psig
P_B <- 16.7                # Presión en la estación B en psig
nu <- 0.005                # Viscosidad cinemática del petróleo en ft²/s
dr <- 0.90                 # Gravedad específica del petróleo (adimensional)
S <- 0.08840               # Área transversal de la tubería en ft²
gc <- 32.2                 # Factor de conversión gravitacional en (lb·ft)/(lb·s²)

# Paso 2: Calcular la densidad del petróleo (ρ)
# ρ = dr * densidad del agua
rho_agua <- 62.371         # Densidad del agua en lb/ft³
rho <- dr * rho_agua       # Densidad del petróleo en lb/ft³

# Paso 3: Calcular la viscosidad dinámica del petróleo (μ)
# μ = ν * ρ
mu <- nu * rho             # Viscosidad dinámica en lb/(ft·s)

# Paso 4: Calcular la diferencia de presión (ΔP)
# ΔP = (P_A - P_B) * 144 (convertir de psi a lb/ft²)
delta_P <- (P_A - P_B) * 144  # Diferencia de presión en lb/ft²

# Paso 5: Calcular la velocidad del flujo (U)
# U = (ΔP * gc * D²) / (32 * μ * L)
U <- (delta_P * gc * D^2) / (32 * mu * L)  # Velocidad del flujo en ft/s

# Paso 6: Calcular el caudal volumétrico (Q)
# Q = U * S
Q <- U * S                  # Caudal volumétrico en ft³/s

# Paso 7: Mostrar los resultados
print(paste("La velocidad del flujo es:", round(U, 4), "ft/s"))
print(paste("El caudal volumétrico es:", round(Q, 4), "ft^3/s"))

# Paso 8: Convertir el caudal volumétrico a galones por minuto (opcional)
# 1 ft^3/s = 448.831 gal/min
Q_gal_min <- Q * 448.831    # Caudal volumétrico en gal/min
print(paste("El caudal volumétrico es:", round(Q_gal_min, 4), "gal/min"))
