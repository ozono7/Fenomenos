# Problema 12: Cálculo de la potencia de la bomba y caída de presión en accesorios
# Autor: José Juan Torres Martínez
# Descripción: Este código calcula la potencia de la bomba necesaria para bombear benceno líquido
# desde un tanque de almacenamiento hasta un reactor químico, y la caída de presión en los accesorios.
# Fecha: 2023-10-30

# Paso 1: Definir las constantes y variables
Q <- 3.53147                # Caudal volumétrico en ft^3/s
L <- 50 * 3.28084           # Longitud de la tubería en ft (50m)
D <- 0.1722                 # Diámetro interno de la tubería en ft (2 pulgadas)
rho <- 54.5                 # Densidad del benceno en lb/ft^3
mu <- 0.00067               # Viscosidad dinámica del benceno en lb/(ft·s)
gc <- 32.2                  # Factor de conversión gravitacional en (lb·ft)/(lb_f·s^2)
P1_abs <- 2116.8            # Presión absoluta en el tanque de almacenamiento en lb_f/ft^2
P2_abs <- 12558.43413       # Presión absoluta en el reactor en lb_f/ft^2
Z2_Z1 <- 656.16798          # Diferencia de altura entre el reactor y el tanque en ft
U1 <- 0                     # Velocidad en el tanque de almacenamiento en ft/s
U2 <- 151.56524             # Velocidad en el reactor en ft/s
K_T <- 21.2                 # Coeficiente total de pérdidas por accesorios

# Paso 2: Calcular la potencia de la bomba (-W_f)
# -W_f = (Z2 - Z1) * (g / gc) + (P2 - P1) / rho + (U2^2 - U1^2) / (2 * gc) + (K_T * U2^2) / (2 * gc)
W_f <- (Z2_Z1 * (32.2 / gc)) + ((P2_abs - P1_abs) / rho) + ((U2^2 - U1^2) / (2 * gc)) + ((K_T * U2^2) / (2 * gc))

# Paso 3: Calcular la potencia hidráulica (P_H)
# P_H = W_f * w
w <- rho * Q                # Flujo másico en lb/s
P_H <- W_f * w              # Potencia hidráulica en lb_f·ft/s

# Paso 4: Convertir la potencia hidráulica a hp
# 1 hp = 550 lb_f·ft/s
P_H_hp <- P_H / 550         # Potencia hidráulica en hp

# Paso 5: Mostrar los resultados para el inciso a)
print(paste("La potencia de la bomba es:", round(W_f, 4), "lb_f·ft/lb"))
print(paste("La potencia hidráulica es:", round(P_H, 4), "lb_f·ft/s"))
print(paste("La potencia hidráulica es:", round(P_H_hp, 4), "hp"))

# Paso 6: Calcular la caída de presión en los accesorios (inciso b)
# Para la válvula VCTA
K_VCTA <- 0.24
delta_P_VCTA <- (K_VCTA * U2^2 * rho) / (2 * gc)  # Caída de presión en lb_f/ft^2

# Para los codos de 90°
K_codo <- 0.59
delta_P_codo <- (K_codo * U2^2 * rho) / (2 * gc)  # Caída de presión en lb_f/ft^2

# Paso 7: Mostrar los resultados para el inciso b)
print(paste("La caída de presión en la válvula VCTA es:", round(delta_P_VCTA, 4), "lb_f/ft^2"))
print(paste("La caída de presión en los codos de 90° es:", round(delta_P_codo, 4), "lb_f/ft^2"))

