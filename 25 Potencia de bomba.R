# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular  
# -----------------------------------------------------------

# Tema: Cálculo de Potencia de Bomba para Sistema Multisalida  
# Objetivo: Determinar la potencia requerida (HP)  

# Datos del problema  
eficiencia <- 0.68                # Eficiencia de la bomba (68%)  
rho <- 62.37                      # Densidad del agua a 60°F en lb/ft³  
g <- 9.81                         # Gravedad en m/s²  
gc <- 1                           # Factor de conversión en SI (kg·m/N·s²)  

# Datos de las salidas (convertidos a unidades consistentes)  
salidas <- data.frame(
  salida = c(5, 6, 7, 8),
  Z = c(5, 0, -3, 10),            # Elevación en m  
  Q_m3s = c(0.003154, 0.006308, 0.006308, 0.003154), # Caudal en m³/s  
  P_psig = c(0, 5, 15, 0),        # Presión manométrica en psig  
  Pabs_kgm2 = c(7943.05, 11460.53, 18491.53, 1943.05) # Presión absoluta en kg/m²
)

# Convertir presiones a Pa (1 kg/m² = 9.80665 Pa)  
salidas$Pabs_Pa <- salidas$Pabs_kgm2 * 9.80665  

# Diámetros y velocidades (para pérdidas por fricción)  
diametros <- data.frame(
  D_in = c(4, 3.5, 3, 2),
  D_m = c(0.1022, 0.0901, 0.0779, 0.0524),
  S_m2 = c(0.0082, 0.0063, 0.0047, 0.0021),
  U_ms = c(0.38409, 0.98871, 1.32521, 1.46027)
)

# Longitudes de tubería (supuestas para el ejemplo)  
L1 <- 100 * 0.3048                # Convertir ft a m (100 ft)  
L2 <- 300 * 0.3048                # Convertir ft a m (300 ft)  

# Cálculo de pérdidas por fricción (simplificado con Darcy-Weisbach)  
# Usaremos un factor de fricción (f) supuesto de 0.02 para todas las tuberías  
f <- 0.02  

# Pérdidas en cada tramo (ejemplo para tubería de 4 in)  
hL1 <- f * (L1 / diametros$D_m[1]) * (diametros$U_ms[1]^2) / (2 * g)  
hL2 <- f * (L2 / diametros$D_m[4]) * (diametros$U_ms[4]^2) / (2 * g)  

# Pérdidas totales (sumando todos los tramos)  
hL_total <- hL1 + hL2             # En metros  

# Cálculo de la altura total (H) para cada salida  
salidas$H <- salidas$Z + (salidas$Pabs_Pa / (rho * 16.0185 * g)) + (salidas$Q_m3s^2 / (2 * g * diametros$S_m2[1]^2)) + hL_total  

# Altura máxima requerida (H_max)  
H_max <- max(salidas$H)  

# Caudal total (Q_total)  
Q_total <- sum(salidas$Q_m3s)     # 0.018924 m³/s  

# Potencia de la bomba (en Watts)  
Potencia_W <- (rho * 16.0185) * Q_total * g * H_max / eficiencia  

# Convertir a HP (1 HP = 745.7 W)  
Potencia_HP <- Potencia_W / 745.7  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Altura máxima requerida (H_max):", round(H_max, 2), "m\n")  
cat("Caudal total (Q_total):", round(Q_total, 6), "m³/s\n")  
cat("Pérdidas totales por fricción (hL_total):", round(hL_total, 2), "m\n")  
cat("Potencia de la bomba:", round(Potencia_HP, 2), "HP\n")  
cat("----------------------------------------\n")  
