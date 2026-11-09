# ==========================================================================
# Problema 12: Cálculo de la potencia de la bomba y caída de presión en accesorios
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Este código calcula la potencia de la bomba necesaria para bombear benceno líquido desde un tanque de almacenamiento hasta un reactor químico, y la caída de presión en los accesorios.
# ==========================================================================


# Paso 1: Definir las constantes y variables
Q <- 3.53147                # Caudal volumétrico en ft^3/s
L <- 50 * 3.28084           # Longitud de la tubería en ft (50 m; no se usa en el cálculo porque K_T ya incluye la fricción de la tubería)
D <- 0.1722                 # Diámetro interno de la tubería en ft (2 pulgadas)
rho <- 54.5                 # Densidad del benceno en lb/ft^3
mu <- 0.00067               # Viscosidad dinámica del benceno en lb/(ft·s)
gc <- 32.2                  # Factor de conversión gravitacional en (lb·ft)/(lb_f·s^2)
P1_abs <- 2116.8            # Presión absoluta en el tanque de almacenamiento en lb_f/ft^2
# Dato del problema: presión manométrica en el reactor, 5 bar (confirmado
# por Juan, 2026-11-09). Se convierte a presión absoluta en lb_f/ft^2:
# 1 bar = 14.5038 psi; presión atmosférica = 14.696 psi.
P2_man_bar <- 5             # Presión manométrica en el reactor, en bar
P2_abs <- (P2_man_bar * 14.5038 + 14.696) * 144  # Presión absoluta en el reactor en lb_f/ft^2 (12558.96; el valor original del problema, 12558.43, difiere solo 0.004 % por redondeo de las constantes)
Z2_Z1 <- 200 * 3.28084      # Diferencia de altura entre el reactor y el tanque en ft (200 m)
U1 <- 0                     # Velocidad en el tanque de almacenamiento en ft/s
S <- pi * (D / 2)^2         # Área transversal de la tubería en ft^2
U2 <- Q / S                 # Velocidad en el reactor en ft/s
K_T <- 21.2                 # Coeficiente total de pérdidas por accesorios

# Paso 2: Calcular el trabajo de la bomba por unidad de masa (-W_s)
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
print(paste("El trabajo de la bomba por unidad de masa es:", round(W_f, 4), "lb_f·ft/lb"))
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

