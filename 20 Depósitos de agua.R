# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Diferencia de Alturas en Tuberías en Serie  
# Objetivo: Calcular ΔZ entre depósitos  

# Datos del problema
Q <- 5.0                          # Flujo volumétrico en ft³/s
rho <- 62.6                       # Densidad del agua en lb/ft³
mu <- 7.728e-4                    # Viscosidad dinámica en lb/(ft·s)
g <- 32.2                         # Gravedad en ft/s²
gc <- 32.2                        # Factor de conversión

# Geometría de tuberías
D1 <- 18 / 12                     # Diámetro tubería 1 (18 in) en ft
D2 <- 12 / 12                     # Diámetro tubería 2 (12 in) en ft
L1 <- 1000                        # Longitud tubería 1 en ft
L2 <- 200                         # Longitud tubería 2 en ft
epsilon <- 0.00085                # Rugosidad para hierro fundido en ft

# Áreas transversales
A1 <- pi * (D1^2) / 4             # Área tubería 1
A2 <- pi * (D2^2) / 4             # Área tubería 2

# Velocidades del flujo
v1 <- Q / A1                      # Velocidad en tubería 1
v2 <- Q / A2                      # Velocidad en tubería 2

# Cálculo de números de Reynolds
Re1 <- (rho * v1 * D1) / mu
Re2 <- (rho * v2 * D2) / mu

# Función para calcular factor de fricción
calcular_f <- function(Re, D) {
  if (Re < 2300) {
    return(64 / Re)
  } else {
    f <- uniroot(function(f) {
      1/sqrt(f) + 2*log10(epsilon/(3.7*D) + 2.51/(Re*sqrt(f)))
    }, interval = c(0.001, 0.1))$root
    return(f)
  }
}

# Factores de fricción
f1 <- calcular_f(Re1, D1)
f2 <- calcular_f(Re2, D2)

# Pérdidas por fricción
hL_friccion1 <- f1 * (L1 / D1) * (v1^2) / (2 * g)
hL_friccion2 <- f2 * (L2 / D2) * (v2^2) / (2 * g)

# Pérdidas totales (asumiendo pérdidas menores despreciables)
hL_total <- hL_friccion1 + hL_friccion2

# Diferencia de alturas (ΔZ = hL_total)
delta_Z <- hL_total

# Resultados
cat("----------------------------------------\n")
cat("Resultados del problema:\n")
cat("----------------------------------------\n")
cat("Diferencia de alturas (ΔZ):", round(delta_Z, 2), "ft\n")
cat("Velocidad en tubería 18 in:", round(v1, 2), "ft/s\n")
cat("Velocidad en tubería 12 in:", round(v2, 2), "ft/s\n")
cat("Número de Reynolds (Re1):", round(Re1, 0), "\n")
cat("Número de Reynolds (Re2):", round(Re2, 0), "\n")
cat("Factor de fricción (f1):", round(f1, 6), "\n")
cat("Factor de fricción (f2):", round(f2, 6), "\n")
cat("----------------------------------------\n")
