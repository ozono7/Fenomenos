# Problema 11: Cálculo de la potencia necesaria para bombear un líquido a través de una tubería
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30

# Nota: Este problema calcula la potencia necesaria por cada milla para bombear un líquido
# a través de una tubería de 18 pulgadas de diámetro. El caudal es de 3 ft³/s, y las
# propiedades del líquido son: viscosidad cinemática ν = 3.3 × 10⁻⁴ ft²/s y densidad
# ρ = 55 lb/ft³. Se utiliza la ecuación de Blasius para determinar el factor de fricción.

# Paso 1: Definir las constantes y variables
D <- 1.4063                # Diámetro interno de la tubería en ft (18 pulgadas)
S <- 1.5533                # Área transversal de la tubería en ft²
Q <- 3                     # Caudal volumétrico en ft³/s
nu <- 3.3e-4               # Viscosidad cinemática en ft²/s
rho <- 55                  # Densidad del líquido en lb/ft³
gc <- 32.2                 # Factor de conversión gravitacional en (lb·ft)/(lb·s²)
L_milla <- 5278.87139      # Longitud de una milla en ft

# Paso 2: Calcular la velocidad del flujo (U)
U <- Q / S                 # Velocidad del flujo en ft/s

# Paso 3: Calcular el número de Reynolds (N_Re)
N_Re <- (D * U) / nu       # Número de Reynolds (adimensional)

# Paso 4: Calcular el factor de fricción (f) usando la ecuación de Blasius
f <- 0.3164 / (N_Re^(1/4)) # Factor de fricción (adimensional)

# Paso 5: Calcular la pérdida de energía debido a la fricción (-W_f)
# -W_f = (f * U² * L_T) / (2 * gc * D)
W_f <- (f * U^2 * L_milla) / (2 * gc * D)  # Pérdida de energía en lb_f·ft/lb

# Paso 6: Calcular el flujo másico (w)
w <- rho * Q               # Flujo másico en lb/s

# Paso 7: Calcular la potencia hidráulica (P_H)
P_H <- W_f * w             # Potencia hidráulica en lb_f·ft/s

# Paso 8: Convertir la potencia a caballos de fuerza (hp)
# 1 hp = 550 lb_f·ft/s
P_H_hp <- P_H / 550        # Potencia en hp

# Paso 9: Mostrar los resultados
print(paste("La velocidad del flujo es:", round(U, 4), "ft/s"))
print(paste("El número de Reynolds es:", round(N_Re, 4)))
print(paste("El factor de fricción es:", round(f, 5)))
print(paste("La pérdida de energía debido a la fricción es:", round(W_f, 4), "lb_f·ft/lb"))
print(paste("La potencia hidráulica es:", round(P_H, 4), "lb_f·ft/s"))
print(paste("La potencia necesaria es:", round(P_H_hp, 4), "hp"))
