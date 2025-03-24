# Problema 13: Cálculo del diámetro de tubería necesaria para transportar agua
# Autor: José Juan Torres
# Descripción: Este código calcula el diámetro de una tubería nueva de fierro fundido
# necesaria para transportar agua a 25°C con una pérdida de carga específica.
# Fecha: 2023-10-30

# Paso 1: Definir las constantes y variables
Q <- 0.02208157            # Caudal volumétrico en m^3/s (350 GPM)
L_T <- 1000                # Longitud de la tubería en m
h <- 2                     # Pérdida de carga en m
g <- 9.81                  # Aceleración debido a la gravedad en m/s^2
rho <- 997                 # Densidad del agua a 25°C en kg/m^3
mu <- 8.8e-4               # Viscosidad dinámica del agua a 25°C en kg/(m·s)
epsilon <- 2.6e-4          # Rugosidad absoluta del fierro fundido en m

# Paso 2: Función para calcular el diámetro de la tubería
# D = (8 * Q^2 * L_T * f) / (pi^2 * h * g * D^4)
# Se utiliza un método iterativo para encontrar el diámetro correcto

# Función para calcular el número de Reynolds
N_Re <- function(D) {
  (4 * Q * rho) / (pi * mu * D)
}

# Función para calcular el factor de fricción (f) usando la ecuación de Colebrook-White
friction_factor <- function(D, epsilon, N_Re) {
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

# Método iterativo para encontrar el diámetro
D_initial <- 0.15  # Diámetro inicial en m
tolerance <- 1e-6
max_iter <- 1000

for (i in 1:max_iter) {
  N_Re_val <- N_Re(D_initial)
  f <- friction_factor(D_initial, epsilon, N_Re_val)
  D_new <- (8 * Q^2 * L_T * f) / (pi^2 * h * g * D_initial^4)
  D_new <- D_new^(1/5)
  
  if (abs(D_new - D_initial) < tolerance) {
    D_final <- D_new
    break
  }
  D_initial <- D_new
}

# Paso 3: Mostrar el resultado
print(paste("El diámetro de la tubería necesario es:", round(D_final, 4), "m"))

# Paso 4: Verificar el número de Reynolds y el factor de fricción final
N_Re_final <- N_Re(D_final)
f_final <- friction_factor(D_final, epsilon, N_Re_final)
print(paste("El número de Reynolds es:", round(N_Re_final, 4)))
print(paste("El factor de fricción es:", round(f_final, 4)))
