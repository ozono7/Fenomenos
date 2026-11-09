# ==========================================================================
# Problema 15: Cálculo del gasto descargado por un tubo
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Este código calcula el gasto descargado por un tubo que transporta agua a 150°F.
# ==========================================================================


# Paso 1: Definir las constantes y variables
Z1_Z2 <- 260                # Diferencia de altura en ft
g <- 32.2                   # Aceleración debido a la gravedad en ft/s^2
gc <- 32.2                  # Factor de conversión gravitacional en (lb·ft)/(lb_f·s^2)
D <- 0.1722                 # Diámetro interno de la tubería en ft
L_T <- 248.9544             # Longitud total de la tubería en ft
rho <- 61.188               # Densidad del agua a 150°F en lb/ft^3
mu <- 2.8896e-4             # Viscosidad dinámica del agua a 150°F en lb/(ft·s)
# Rugosidad ABSOLUTA para acero comercial nuevo: 0.045 mm = 0.00015 ft
# (tabla de rugosidades de Moody/Colebrook; Cengel, Tabla 8-2). La
# rugosidad relativa es epsilon/D, adimensional. El valor anterior
# (0.0009 ft = 0.27 mm) corresponde a fierro fundido o concreto, no a
# acero comercial. Con 0.0009 como rugosidad relativa (epsilon/D = 0.0009)
# el gasto saldría 0.5575 ft^3/s, casi igual al de aquí porque
# 0.0009 * D = 0.000155 ft, prácticamente el valor estándar de acero comercial.
epsilon <- 0.00015          # Rugosidad absoluta para acero comercial en ft

# Paso 2: Función para calcular el factor de fricción (f) usando la ecuación de Colebrook-White
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

# Paso 3: Método iterativo para encontrar el gasto (Q)
Q_initial <- 0.23  # Valor inicial para Q en ft^3/s
tolerance <- 1e-6
max_iter <- 1000

for (i in 1:max_iter) {
  N_Re <- (4 * Q_initial * rho) / (pi * mu * D)
  f <- friction_factor(epsilon, D, N_Re)
  
  denominator <- (8 / (pi^2 * gc)) * ((f * L_T) / D^5 + 1 / D^4)
  Q_new <- sqrt((Z1_Z2 * (g / gc)) / denominator)
  
  if (abs(Q_new - Q_initial) < tolerance) {
    Q_final <- Q_new
    break
  }
  Q_initial <- Q_new
}

# Paso 4: Mostrar el resultado
print(paste("El gasto descargado por el tubo es:", round(Q_final, 4), "ft^3/s"))

# Paso 5: Verificar el número de Reynolds y el factor de fricción final
N_Re_final <- (4 * Q_final * rho) / (pi * mu * D)
f_final <- friction_factor(epsilon, D, N_Re_final)
print(paste("El número de Reynolds es:", round(N_Re_final, 4)))
print(paste("El factor de fricción es:", round(f_final, 4)))

