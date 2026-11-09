# ==========================================================================
# Flujo en Ramales de Tuberías con Benceno
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Calcular el caudal volumétrico en cada ramal
# ==========================================================================


# Datos del problema  
rho <- 55                    # Densidad del benceno en lb/ft³  
mu <- 4.365e-4               # Viscosidad dinámica en lb/(ft·s)  
g <- 32.2                    # Gravedad en ft/s²  
gc <- 32.2                   # Factor de conversión (lb·ft)/(lb_f·s²)  
delta_Z <- 23                # Diferencia de altura en ft  
Hfs_total <- delta_Z         # Pérdida de carga total (lb·ft)/lb  

# Datos de los ramales  
# Ramal 1  
L1 <- 30                     # Longitud en ft  
D1 <- 0.1722                 # Diámetro interno en ft (2 in, cédula 40)
S1 <- pi * (D1^2) / 4        # Área transversal en ft²
fT1 <- 0.019                 # Factor de fricción de Darcy
K_T1 <- fT1 * (L1 / D1)      # Coeficiente de pérdida total

# Ramal 2  
L2 <- 30                     # Longitud en ft  
D2 <- 0.2557                 # Diámetro interno en ft (3 in, cédula 40)
S2 <- pi * (D2^2) / 4        # Área transversal en ft²
fT2 <- 0.018                 # Factor de fricción de Darcy
K_T2 <- fT2 * (L2 / D2)      # Coeficiente de pérdida total

# Función para calcular caudal (Q) dada la pérdida de carga  
calcular_Q <- function(K_T, S, Hfs) {
  sqrt((2 * gc * S^2 * Hfs) / K_T)  
}  

# Cálculo de caudales  
Q1 <- calcular_Q(K_T1, S1, Hfs_total)
Q2 <- calcular_Q(K_T2, S2, Hfs_total)

# Cálculo de velocidades  
v1 <- Q1 / S1
v2 <- Q2 / S2

# Cálculo del número de Reynolds  
Re1 <- (rho * v1 * D1) / mu  
Re2 <- (rho * v2 * D2) / mu  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Caudal en Ramal 1 (Q1):", round(Q1, 4), "ft³/s\n")  
cat("Caudal en Ramal 2 (Q2):", round(Q2, 4), "ft³/s\n")  
cat("Número de Reynolds en Ramal 1 (Re1):", round(Re1, 0), "\n")  
cat("Número de Reynolds en Ramal 2 (Re2):", round(Re2, 0), "\n")  

# Tipo de flujo (criterio de Bird, Stewart y Lightfoot, Fenómenos de
# Transporte: laminar Re < 2100, transicional 2100-4000, turbulento Re > 4000)
tipo_flujo <- function(Re) {
  if (Re < 2100) {
    return("Laminar")
  } else if (Re <= 4000) {
    return("Transicional")
  } else {
    return("Turbulento")
  }
}

cat("Flujo en Ramal 1:", tipo_flujo(Re1), "\n")
cat("Flujo en Ramal 2:", tipo_flujo(Re2), "\n")
cat("----------------------------------------\n")

