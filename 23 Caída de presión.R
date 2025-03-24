# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Laboratorio de Biotecnología Molecular  
# -----------------------------------------------------------

# Datos del problema
Q <- 1.2                          # ft³/s
rho <- 62.37                      # lb/ft³
gc <- 32.2                        # (lb·ft)/(lb_f·s²)

# Geometría
D1 <- 0.3355                      # ft (4 in)
D2 <- 0.6651                      # ft (8 in)
A1 <- 0.08840                     # ft²
A2 <- 0.3474                      # ft²

# Velocidades
U1 <- Q / A1                      # 13.57 ft/s
U2 <- Q / A2                      # 3.45 ft/s

# Coeficiente de expansión (Borda-Carnot)
K_expansion <- (1 - (D1^2 / D2^2))^2  # 0.5558

# Cálculo CORRECTO de ΔP (pérdida de presión POSITIVA)
# Ecuación: ΔP/ρ = (U1² - U2²)/(2gc) - (K_expansion * U1²)/(2gc)
Delta_P <- rho * ( (U1^2 - U2^2)/(2*gc) - (K_expansion * U1^2)/(2*gc) )

# Conversión a psi
Delta_P_psi <- Delta_P / 144

# Resultados
cat("----------------------------------------\n")
cat("Resultados CORREGIDOS:\n")
cat("----------------------------------------\n")
cat("Velocidad en 4 in:", round(U1, 2), "ft/s\n")
cat("Velocidad en 8 in:", round(U2, 2), "ft/s\n")
cat("Coeficiente de expansión:", round(K_expansion, 4), "\n")
cat("Caída de presión (ΔP):", round(Delta_P_psi, 4), "psi\n")
cat("----------------------------------------\n")
