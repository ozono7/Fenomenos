# ==========================================================================
# Problema 9: Determinar el tipo de flujo en una tubería de 4 pulgadas de diámetro
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Fecha: 2023-10-30
#
# Descripción: Este código determina el tipo de flujo (laminar o turbulento) en una tubería de 4 pulgadas de diámetro para diferentes fluidos: aire, agua, petróleo crudo y glicerina.
# ==========================================================================


# Paso 1: Definir las constantes y variables
D <- 0.3355                # Diámetro interno de la tubería en ft (4 pulgadas)
U <- 3                     # Velocidad promedio del flujo en ft/s

# Propiedades de los fluidos
# Aire a 60°F y 14.7 lb/in²
rho_aire <- 0.0764         # Densidad del aire en lb/ft³
mu_aire <- 1.34e-5         # Viscosidad dinámica del aire en lb/(ft·s)

# Agua a 60°F
rho_agua <- 62.371         # Densidad del agua en lb/ft³
mu_agua <- 7.39e-4         # Viscosidad dinámica del agua en lb/(ft·s)

# Petróleo crudo (gravedad específica 0.93)
rho_petroleo <- 58.005     # Densidad del petróleo en lb/ft³
mu_petroleo <- 0.6757      # Viscosidad dinámica del petróleo en lb/(ft·s)

# Glicerina a 65°F (18°C)
rho_glicerina <- 78.5854   # Densidad de la glicerina en lb/ft³
# Viscosidad dinámica de la glicerina: 1.49 Pa·s a 20°C (tabla de
# propiedades físicas de la glicerina); 1 lb/(ft·s) = 1.48816 Pa·s,
# por lo que mu = 1.49 / 1.48816 = 1.0012 lb/(ft·s). El valor anterior
# (6.435 lb/(ft·s) = 9.58 Pa·s) era unas 6 veces el valor moderno.
mu_glicerina <- 1.0012     # Viscosidad dinámica de la glicerina en lb/(ft·s)

# Paso 2: Calcular el número de Reynolds (N_Re) para cada fluido
# N_Re = (ρ * U * D) / μ

# Aire
N_Re_aire <- (rho_aire * U * D) / mu_aire

# Agua
N_Re_agua <- (rho_agua * U * D) / mu_agua

# Petróleo crudo
N_Re_petroleo <- (rho_petroleo * U * D) / mu_petroleo

# Glicerina
N_Re_glicerina <- (rho_glicerina * U * D) / mu_glicerina

# Paso 3: Determinar el tipo de flujo para cada fluido
# Criterio de Bird, Stewart y Lightfoot (Fenómenos de Transporte):
# Flujo laminar: N_Re < 2100
# Flujo transicional: 2100 <= N_Re <= 4000
# Flujo turbulento: N_Re > 4000

tipo_flujo <- function(N_Re) {
  ifelse(N_Re < 2100, "Laminar",
         ifelse(N_Re <= 4000, "Transicional", "Turbulento"))
}

tipo_flujo_aire <- tipo_flujo(N_Re_aire)
tipo_flujo_agua <- tipo_flujo(N_Re_agua)
tipo_flujo_petroleo <- tipo_flujo(N_Re_petroleo)
tipo_flujo_glicerina <- tipo_flujo(N_Re_glicerina)

# Paso 4: Mostrar los resultados
print(paste("Número de Reynolds para el aire:", round(N_Re_aire, 2), "- Flujo:", tipo_flujo_aire))
print(paste("Número de Reynolds para el agua:", round(N_Re_agua, 2), "- Flujo:", tipo_flujo_agua))
print(paste("Número de Reynolds para el petróleo crudo:", round(N_Re_petroleo, 2), "- Flujo:", tipo_flujo_petroleo))
print(paste("Número de Reynolds para la glicerina:", round(N_Re_glicerina, 2), "- Flujo:", tipo_flujo_glicerina))

