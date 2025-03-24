# Problema 14: Cálculo del ahorro en el costo de bombeo por año y por kilómetro de tubería
# Autor: José Juan Torres 
# Descripción: Este código calcula el ahorro en el costo de bombeo después de aplicar un revestimiento
# a una tubería vieja, considerando la rugosidad y la eficiencia de la bomba.
# Fecha: 2025

# Paso 1: Definir las constantes y variables
Q <- 6                      # Caudal volumétrico en m^3/s
D_old <- 2                  # Diámetro de la tubería vieja en m
epsilon_old <- 0.03         # Rugosidad de la tubería vieja en m
epsilon_new <- 0.001        # Rugosidad de la tubería nueva en m
thickness <- 0.012          # Espesor del revestimiento en m
rho <- 998.2                # Densidad del agua a 20°C en kg/m^3
mu <- 9.8e-4                # Viscosidad dinámica del agua a 20°C en kg/(m·s)
g <- 9.81                   # Aceleración debido a la gravedad en m/s^2
L_T <- 1000                 # Longitud de la tubería en m
efficiency <- 0.8           # Eficiencia global de la bomba y motores
cost_per_kWh <- 0.01        # Costo de la energía en dólares por kWh

# Paso 2: Calcular el diámetro y área transversal después del revestimiento
D_new <- D_old - 2 * thickness  # Diámetro después del revestimiento en m
S_new <- pi * (D_new / 2)^2     # Área transversal después del revestimiento en m^2

# Paso 3: Calcular la velocidad del flujo antes y después del revestimiento
U_old <- Q / (pi * (D_old / 2)^2)  # Velocidad antes del revestimiento en m/s
U_new <- Q / S_new                 # Velocidad después del revestimiento en m/s

# Paso 4: Calcular el número de Reynolds antes y después del revestimiento
N_Re_old <- (D_old * U_old * rho) / mu
N_Re_new <- (D_new * U_new * rho) / mu

# Paso 5: Calcular el factor de fricción antes y después del revestimiento
# Usando la ecuación de Colebrook-White
friction_factor <- function(epsilon, D, N_Re) {
  f <- 0.02  # Valor inicial para f
  tolerance <- 1e-6
  max_iter <- 1000
  for (i in 1:max_iter) {
    f_new <- (-2 * log10((epsilon / D) / 3.7 + 2.51 / (N_Re * sqrt(f))))^(-2)
    if (abs(f_new - f) < tolerance) {
      return(f_new)
    }
    f <- f_new
  }
  return(f)
}

f_old <- friction_factor(epsilon_old, D_old, N_Re_old)
f_new <- friction_factor(epsilon_new, D_new, N_Re_new)

# Paso 6: Calcular la pérdida de carga antes y después del revestimiento
W_f_old <- (U_old^2) / (2 * g) + (f_old * U_old^2 * L_T) / (2 * g * D_old)
W_f_new <- (U_new^2) / (2 * g) + (f_new * U_new^2 * L_T) / (2 * g * D_new)

# Paso 7: Calcular la potencia hidráulica antes y después del revestimiento
w <- Q * rho  # Flujo másico en kg/s
P_H_old <- W_f_old * w  # Potencia hidráulica antes del revestimiento en W
P_H_new <- W_f_new * w  # Potencia hidráulica después del revestimiento en W

# Paso 8: Calcular la potencia requerida antes y después del revestimiento
P_R_old <- P_H_old / efficiency  # Potencia requerida antes del revestimiento en W
P_R_new <- P_H_new / efficiency  # Potencia requerida después del revestimiento en W

# Paso 9: Calcular el costo de bombeo antes y después del revestimiento
cost_old <- (P_R_old / 1000) * cost_per_kWh  # Costo antes del revestimiento en $/h
cost_new <- (P_R_new / 1000) * cost_per_kWh  # Costo después del revestimiento en $/h

# Paso 10: Calcular el ahorro en el costo de bombeo por año y por kilómetro
savings_per_hour <- cost_old - cost_new  # Ahorro por hora en $/h
savings_per_year <- savings_per_hour * 24 * 365  # Ahorro por año en $/año

# Paso 11: Mostrar los resultados
print(paste("El ahorro en el costo de bombeo por kilómetro es:", round(savings_per_hour, 4), "$/h"))
print(paste("El ahorro en el costo de bombeo por año es:", round(savings_per_year, 4), "$/año"))

