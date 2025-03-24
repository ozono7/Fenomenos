# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular  
# -----------------------------------------------------------

# Tema: Evaluación de Sistema de Bombeo para Benceno  
# Objetivos: a) Adecuación de bomba, b) Tiempo de vaciado, c) Trabajo, d) NPSH  

# Datos del problema  
volumen_gal <- 10000              # Volumen en galones  
volumen_ft3 <- volumen_gal / 7.48052  # Convertir a ft³  
rho <- 54.1                       # Densidad del benceno en lb/ft³  
tiempo_deseado <- 3 * 3600        # Tiempo deseado en segundos (3 horas)  
g <- 32.2                         # Gravedad en ft/s²  
gc <- 32.2                        # Factor de conversión  

# Datos de la bomba (Q en ft³/s, H en ft, eficiencia n%)  
datos_bomba <- data.frame(
  Q_gpm = c(0, 20, 40, 60, 80, 100, 120, 140),
  H_ft = c(110, 106, 90, 63, 41, 22, 12, 7),
  n = c(0, 29.2, 40, 45, 47, 48, 46.5, 40),
  Q_ft3s = c(0, 0.0446, 0.0892, 0.1339, 0.1785, 0.2231, 0.2677, 0.3124)
)

# Pérdidas por accesorios (Let total)  
Let_total <- 186.922              # Longitud equivalente total en ft  

# Suponemos diámetro de tubería (D) y calculamos pérdidas  
D <- 0.5                          # Diámetro supuesto en ft (ajustar según sistema real)  
A <- pi * (D^2) / 4               # Área transversal en ft²  

# Cálculo del flujo requerido (Q_req)  
Q_req_ft3s <- volumen_ft3 / tiempo_deseado  
Q_req_gpm <- Q_req_ft3s * 448.831 # Conversión a GPM  

# Interpolar datos de la bomba para Q_req  
library(dplyr)  
bomba_req <- approx(datos_bomba$Q_ft3s, datos_bomba$H_ft, xout = Q_req_ft3s)  
H_bomba <- bomba_req$y  
n_bomba <- approx(datos_bomba$Q_ft3s, datos_bomba$n, xout = Q_req_ft3s)$y  

# Cálculo de pérdidas de carga (Hf)  
v <- Q_req_ft3s / A               # Velocidad en ft/s  
Re <- (rho * v * D) / (6.72e-4)   # Viscosidad del benceno ≈ 6.72e-4 lb/(ft·s)  
f <- if (Re < 2300) 64/Re else 0.3164 / (Re^0.25)  # Factor de fricción  
Hf <- f * (Let_total / D) * (v^2) / (2 * g)        # Pérdidas en ft  

# Altura total requerida (H_total)  
H_total <- Hf + 10                # 10 ft supuestos por diferencia de altura (ajustar)  

# Evaluación de la bomba  
bomba_adecuada <- H_bomba >= H_total  

# Tiempo real de vaciado (si H_bomba < H_total)  
if (!bomba_adecuada) {  
  Q_max_posible <- max(datos_bomba$Q_ft3s[datos_bomba$H_ft >= H_total])  
  tiempo_real <- volumen_ft3 / Q_max_posible  
} else {  
  tiempo_real <- tiempo_deseado  
}  

# Trabajo realizado (HP)  
Potencia <- (rho * Q_req_ft3s * H_bomba) / (550 * n_bomba/100)  # 1 HP = 550 ft·lb/s  

# Cálculo de NPSH disponible (simplificado)  
P_vapor <- 1.8                    # Presión de vapor del benceno a 80°F en psi  
P_atm <- 14.7                     # Presión atmosférica en psi  
NPSH_disp <- (P_atm - P_vapor) * 144 / rho + 5  # 5 ft supuestos por altura de succión  

# Resultados  
cat("----------------------------------------\n")  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("a) ¿La bomba es adecuada?:", ifelse(bomba_adecuada, "Sí", "No"), "\n")  
cat("b) Tiempo real de vaciado:", round(tiempo_real/3600, 2), "horas\n")  
cat("c) Potencia requerida:", round(Potencia, 2), "HP\n")  
cat("d) NPSH disponible:", round(NPSH_disp, 2), "ft\n")  
cat("----------------------------------------\n")  
