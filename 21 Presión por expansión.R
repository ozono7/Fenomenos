# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Pérdida de Presión en Expansión Repentina  
# Objetivo: Calcular ΔP debido a la expansión  

# Datos del problema  
Q <- 1.2                          # Flujo volumétrico en ft³/s  
rho <- 62.37                      # Densidad del agua a 60°F en lb/ft³  
g <- 32.2                         # Gravedad en ft/s²  
gc <- 32.2                        # Factor de conversión  

# Diámetros de tuberías  
D1 <- 4 / 12                      # Diámetro menor (4 in) en ft  
D2 <- 8 / 12                      # Diámetro mayor (8 in) en ft  

# Áreas transversales  
A1 <- pi * (D1^2) / 4             # Área tubería 4 in  
A2 <- pi * (D2^2) / 4             # Área tubería 8 in  

# Velocidades del flujo  
v1 <- Q / A1                      # Velocidad en tubería 4 in  
v2 <- Q / A2                      # Velocidad en tubería 8 in  

# Coeficiente de pérdida por expansión (K_Expansion)  
K_Expansion <- (1 - (D1^2 / D2^2))^2  

# Coeficiente de pérdida por fricción (K_TR)  
f <- 0.017                        # Factor de fricción supuesto  
L <- 1                            # Longitud supuesta en ft  
K_TR <- f * (L / D1)              # Coeficiente de pérdida por fricción  

# Coeficiente de pérdida total (K_TA)  
K_TA <- K_TR + K_Expansion  

# Pérdidas de carga  
Hfs_A <- (K_TA * v1^2) / (2 * gc)  # Pérdida en tubería 4 in  
Hfs_B <- (K_Expansion * v2^2) / (2 * gc)  # Pérdida en tubería 8 in  

# Pérdida de presión total (ΔP)  
Delta_P <- rho * (Hfs_A + Hfs_B)   # en lb/ft²  

# Convertir a psi (1 psi = 144 lb/ft²)  
Delta_P_psi <- Delta_P / 144  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Velocidad en tubería 4 in:", round(v1, 2), "ft/s\n")  
cat("Velocidad en tubería 8 in:", round(v2, 2), "ft/s\n")  
cat("Coeficiente de expansión (K_Expansion):", round(K_Expansion, 4), "\n")  
cat("Coeficiente de fricción (K_TR):", round(K_TR, 4), "\n")  
cat("Pérdida de presión total (ΔP):", round(Delta_P_psi, 4), "psi\n")  
cat("----------------------------------------\n")  

