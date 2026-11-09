# ==========================================================================
# Flujo Volumétrico en Tuberías
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Calcular Q, v y Re usando la ecuación de Darcy-Weisbach
# ==========================================================================


# Datos del problema  
delta_P <- 150000      # Caída de presión en Pa (150 kPa = 150,000 Pa)  
L <- 100               # Longitud de la tubería en m  
D <- 0.15              # Diámetro interno en m  
epsilon <- 0.000046    # Rugosidad absoluta en m (acero comercial)  
rho <- 998.2           # Densidad del agua a 20°C en kg/m³  
mu <- 0.001002         # Viscosidad dinámica del agua a 20°C en Pa·s  
g <- 9.81              # Gravedad en m/s²  

# 1. Suposición inicial de factor de fricción (f) para flujo turbulento  
f <- 0.02              # Valor inicial típico  

# 2. Cálculo de la velocidad (v) usando Darcy-Weisbach (sin pérdidas menores)  
# Ecuación: delta_P = f * (L/D) * (rho * v² / 2)  
v <- sqrt((2 * delta_P * D) / (f * L * rho))  

# 3. Cálculo del número de Reynolds (Re)  
Re <- (rho * v * D) / mu  

# 4. Iterar el factor de fricción (f) con la ecuación de Colebrook-White
# Función para resolver Colebrook-White iterativamente  
colebrook <- function(f, Re, epsilon, D) {  
  1/sqrt(f) + 2 * log10((epsilon/D)/3.7 + 2.51/(Re * sqrt(f)))  
}  

# Iterar con uniroot hasta que f, v y Re sean consistentes entre sí
# (f depende de Re, Re depende de v y v depende de f)
tolerancia <- 1e-6
for (i in 1:100) {
  f_nuevo <- uniroot(function(f) colebrook(f, Re, epsilon, D), interval = c(0.001, 0.1))$root
  v_nuevo <- sqrt((2 * delta_P * D) / (f_nuevo * L * rho))
  Re_nuevo <- (rho * v_nuevo * D) / mu
  if (abs(f_nuevo - f) < tolerancia) {
    f <- f_nuevo
    v <- v_nuevo
    Re <- Re_nuevo
    break
  }
  f <- f_nuevo
  v <- v_nuevo
  Re <- Re_nuevo
}

# 5. Calcular el flujo volumétrico (Q)
Q <- v * (pi * D^2 / 4)

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Velocidad del flujo (v):", round(v, 2), "m/s\n")  
cat("Flujo volumétrico (Q):", round(Q, 4), "m³/s\n")  
cat("Número de Reynolds (Re):", round(Re, 0), "\n")  
cat("Factor de fricción (f):", round(f, 4), "\n")  

# Determinar tipo de flujo (criterio de Bird, Stewart y Lightfoot,
# Fenómenos de Transporte: laminar Re < 2100, transicional 2100-4000,
# turbulento Re > 4000)
if (Re < 2100) {
  cat("El flujo es laminar.\n")
} else if (Re <= 4000) {
  cat("El flujo es transicional.\n")
} else {
  cat("El flujo es turbulento.\n")
}
cat("----------------------------------------\n")  

