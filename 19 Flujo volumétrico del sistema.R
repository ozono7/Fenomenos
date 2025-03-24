# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Flujo por Gravedad en Sistema de Tuberías  
# Objetivo: Calcular el flujo volumétrico (Q)  

# Datos del problema  
T <- 75                              # Temperatura en °F  
rho <- 62.24                         # Densidad del agua a 75°F en lb/ft³  
mu <- 6.05e-5                        # Viscosidad dinámica en lb/(ft·s)  
g <- 32.2                            # Gravedad en ft/s²  
gc <- 32.2                           # Factor de conversión  

# Geometría de tuberías y accesorios  
D1 <- 4 / 12                         # Diámetro menor (4 in) en ft  
D2 <- 6 / 12                         # Diámetro mayor (6 in) en ft  
L1 <- 200                            # Longitud tubería 4 in en ft  
L2 <- 100                            # Longitud tubería 6 in en ft  
epsilon <- 0.00085                   # Rugosidad para hierro fundido en ft  

# Coeficientes de pérdida (K) para accesorios  
K_entrada <- 0.78                    # Entrada  
K_codos_4in <- 1.5                   # 3 codos de 90° en tubería 4 in  
K_expansion <- 0.3128                # Expansión 4"→6"  
K_codos_6in <- 0.92                  # 2 codos de 90° en tubería 6 in  
K_valvula <- 5.2                     # Válvula de compuerta  
K_salida <- 1                        # Salida  
K_total <- K_entrada + K_codos_4in + K_expansion + K_codos_6in + K_valvula + K_salida  

# Áreas transversales  
A1 <- pi * (D1^2) / 4                # Área tubería 4 in  
A2 <- pi * (D2^2) / 4                # Área tubería 6 in  

# Suposición inicial de Q (ft³/s)  
Q <- 1.0                             # Valor inicial  

# Función para resolver el balance de energía  
calcular_Q <- function(Q) {
  v1 <- Q / A1                       # Velocidad en tubería 4 in  
  v2 <- Q / A2                       # Velocidad en tubería 6 in  
  
  # Cálculo de números de Reynolds  
  Re1 <- (rho * v1 * D1) / mu  
  Re2 <- (rho * v2 * D2) / mu  
  
  # Factores de fricción (Colebrook-White)  
  f1 <- ifelse(Re1 < 2300, 64/Re1, uniroot(function(f) 1/sqrt(f) + 2*log10(epsilon/(3.7*D1) + 2.51/(Re1*sqrt(f))), c(0.001, 0.1))$root)  
  f2 <- ifelse(Re2 < 2300, 64/Re2, uniroot(function(f) 1/sqrt(f) + 2*log10(epsilon/(3.7*D2) + 2.51/(Re2*sqrt(f))), c(0.001, 0.1))$root)  
  
  # Pérdidas por fricción  
  hL_friccion <- (f1 * (L1/D1) * (v1^2)/(2*g)) + (f2 * (L2/D2) * (v2^2)/(2*g))  
  
  # Pérdidas por accesorios  
  hL_accesorios <- K_total * (v2^2)/(2*g)  # Usamos v2 (diámetro mayor) como referencia  
  
  # Pérdida total (asumiendo que la diferencia de altura Z es conocida)  
  Z <- 10                              # Diferencia de altura supuesta en ft (ajustar según problema)  
  hL_total <- hL_friccion + hL_accesorios  
  
  # Balance de energía: Z = hL_total  
  diferencia <- Z - hL_total  
  return(diferencia)
}

# Resolver para Q usando uniroot  
Q_solucion <- uniroot(calcular_Q, interval = c(0.1, 10))$root  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Flujo volumétrico (Q):", round(Q_solucion, 4), "ft³/s\n")  
cat("Velocidad en tubería 4 in:", round(Q_solucion/A1, 2), "ft/s\n")  
cat("Velocidad en tubería 6 in:", round(Q_solucion/A2, 2), "ft/s\n")  
cat("----------------------------------------\n")  
