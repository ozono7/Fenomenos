# ==========================================================================
# Ajuste de Longitud de Tubería para Cambiar Velocidad
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Calcular la longitud de tubería a eliminar
# ==========================================================================


# Datos del problema  
v1 <- 1.0                          # Velocidad inicial en m/s  
v2 <- 2.5                          # Velocidad deseada en m/s  
D <- 100 / 1000                    # Diámetro en m (100 mm → 0.1 m)  
L1 <- 1000                         # Longitud inicial en m  
rho <- 998.2                       # Densidad del agua a 20°C en kg/m³  
mu <- 1.002e-3                     # Viscosidad dinámica en Pa·s  
g <- 9.81                          # Gravedad en m/s²  


# Cálculo del número de Reynolds inicial (Re1)  
Re1 <- (rho * v1 * D) / mu  

# Factor de fricción inicial (f1) para tubería lisa (Ecuación de Blasius)  
f1 <- 0.3164 / (Re1^(1/4))  

# Pérdida de carga inicial (hL1) usando Darcy-Weisbach  
hL1 <- f1 * (L1 / D) * (v1^2) / (2 * g)  

# Relación de velocidades y pérdidas de carga
# Con la misma energía disponible, la pérdida de carga es la misma (hL1 = hL2):
# hL1 = f1 * (L1/D) * (v1²/2g)
# hL2 = f2 * (L2/D) * (v2²/2g)
# Igualando hL1 = hL2 se despeja la longitud nueva:
# L2 = L1 * (f1/f2) * (v1/v2)^2

# Cálculo de Re2 y f2  
Re2 <- (rho * v2 * D) / mu  
f2 <- 0.3164 / (Re2^(1/4))  

# Longitud final necesaria (L2)  
L2 <- L1 * (f1 / f2) * (v1 / v2)^2  

# Longitud a eliminar  
L_eliminar <- L1 - L2  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Longitud inicial (L1):", L1, "m\n")  
cat("Longitud necesaria (L2):", round(L2, 2), "m\n")  
cat("Longitud a eliminar:", round(L_eliminar, 2), "m\n")  
cat("Factor de fricción inicial (f1):", round(f1, 4), "\n")  
cat("Factor de fricción final (f2):", round(f2, 4), "\n")  
cat("Número de Reynolds inicial (Re1):", round(Re1, 0), "\n")  
cat("Número de Reynolds final (Re2):", round(Re2, 0), "\n")  
cat("----------------------------------------\n")  
