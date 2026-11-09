# ==========================================================================
# Problema 7: Cálculo de la presión relativa en el tanque A
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Este código calcula la presión relativa (manométrica) en el tanque A para que exista un flujo de petróleo a través de una tubería.
# ==========================================================================


# Paso 1: Definir las constantes y variables
# El dato del problema es 50 gal/min; 1 ft^3/s = 448.831 gal/min, por lo
# que Q = 50 / 448.831 = 0.1114 ft^3/s. El valor anterior (0.114 ft^3/s)
# era una conversión incorrecta (equivale a 51.2 gal/min).
Q <- 50 / 448.831           # Caudal volumétrico en ft^3/s (50 gal/min)
L <- 3000                   # Longitud de la tubería en ft
D <- 0.2557                 # Diámetro interno de la tubería en ft (3 pulgadas cedula 40)
rho <- 62.3                 # Densidad del petróleo en lb/ft^3
mu <- 0.0336                # Viscosidad dinámica del petróleo en lb/(ft·s)
gc <- 32.2                  # Factor de conversión gravitacional en (lb·ft)/(lb·s^2)
P_atm <- 2116.8             # Presión atmosférica en lb/ft^2

# Paso 2: Calcular la velocidad del flujo (U)
# U = Q / (π/4 * D^2)
U <- Q / ((pi / 4) * D^2)   # Velocidad del flujo en ft/s

# Paso 3: Calcular el número de Reynolds (N_Re)
# N_Re = (D * rho * U) / mu
N_Re <- (D * rho * U) / mu  # Número de Reynolds (adimensional)

# Paso 4: Calcular la caída de presión (ΔP)
# ΔP = (32 * U * mu * L) / (gc * D^2)
delta_P <- (32 * U * mu * L) / (gc * D^2)  # Caída de presión en lb/ft^2

# Paso 5: Calcular la presión en el tanque A
# La presión RELATIVA (manométrica) es la caída de presión ΔP:
# el tanque debe estar ΔP por arriba de la atmósfera para impulsar el flujo
P1_rel <- delta_P           # Presión relativa en lb/ft^2
# La presión ABSOLUTA es la relativa más la atmosférica
P1_abs <- delta_P + P_atm   # Presión absoluta en lb/ft^2

# Paso 6: Mostrar los resultados
print(paste("La velocidad del flujo es:", round(U, 4), "ft/s"))
print(paste("El número de Reynolds es:", round(N_Re, 4)))
print(paste("La caída de presión es:", round(delta_P, 4), "lb/ft^2"))
print(paste("La presión relativa en el tanque A es:", round(P1_rel, 4), "lb/ft^2"))
print(paste("La presión absoluta en el tanque A es:", round(P1_abs, 4), "lb/ft^2"))

# Paso 7: Convertir la presión relativa a psi (opcional)
# 1 psi = 144 lb/ft^2
P1_rel_psi <- P1_rel / 144  # Presión relativa en psi
P1_abs_psi <- P1_abs / 144  # Presión absoluta en psi
print(paste("La presión relativa en el tanque A es:", round(P1_rel_psi, 4), "psi"))
print(paste("La presión absoluta en el tanque A es:", round(P1_abs_psi, 4), "psi"))

