# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Flujo en Ramales de Tuberías con Benceno  
# Objetivo: Calcular el caudal volumétrico en cada ramal  

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
D1 <- 2 / 12                 # Diámetro en ft (convertido de in a ft)  
S1 <- 0.02330                # Área transversal en ft²  
fT1 <- 0.019                 # Factor de fricción de Darcy  
K_T1 <- 3.31010453           # Coeficiente de pérdida total  

# Ramal 2  
L2 <- 30                     # Longitud en ft  
D2 <- 3 / 12                 # Diámetro en ft (convertido de in a ft)  
S2 <- 0.05130                # Área transversal en ft²  
fT2 <- 0.018                 # Factor de fricción de Darcy  
K_T2 <- 2.111849824          # Coeficiente de pérdida total  

# Función para calcular caudal (Q) dada la pérdida de carga  
calcular_Q <- function(K_T, S, rho, Hfs) {  
  sqrt((2 * gc * S^2 * Hfs) / K_T)  
}  

# Cálculo de caudales  
Q1 <- calcular_Q(K_T1, S1, rho, Hfs_total)  
Q2 <- calcular_Q(K_T2, S2, rho, Hfs_total)  

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

# Tipo de flujo  
if (Re1 < 2300) {  
  cat("Flujo en Ramal 1: Laminar\n")  
} else {  
  cat("Flujo en Ramal 1: Turbulento\n")  
}  

if (Re2 < 2300) {  
  cat("Flujo en Ramal 2: Laminar\n")  
} else {  
  cat("Flujo en Ramal 2: Turbulento\n")  
}  
cat("----------------------------------------\n")

