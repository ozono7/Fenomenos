# Problema 9: Determinar el tipo de flujo en una tubería de 4 pulgadas de diámetro
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# Fecha: 2023-10-30
# Descripción: Este código determina el tipo de flujo (laminar o turbulento) en una tubería
# de 4 pulgadas de diámetro para diferentes fluidos: aire, agua, petróleo crudo y glicerina.

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

# Glicerina a 65°F
rho_glicerina <- 78.5854   # Densidad de la glicerina en lb/ft³
mu_glicerina <- 6.435      # Viscosidad dinámica de la glicerina en lb/(ft·s)

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
# Flujo laminar: N_Re < 2300
# Flujo turbulento: N_Re >= 2300

tipo_flujo_aire <- ifelse(N_Re_aire < 2300, "Laminar", "Turbulento")
tipo_flujo_agua <- ifelse(N_Re_agua < 2300, "Laminar", "Turbulento")
tipo_flujo_petroleo <- ifelse(N_Re_petroleo < 2300, "Laminar", "Turbulento")
tipo_flujo_glicerina <- ifelse(N_Re_glicerina < 2300, "Laminar", "Turbulento")

# Paso 4: Mostrar los resultados
print(paste("Número de Reynolds para el aire:", round(N_Re_aire, 2), "- Flujo:", tipo_flujo_aire))
print(paste("Número de Reynolds para el agua:", round(N_Re_agua, 2), "- Flujo:", tipo_flujo_agua))
print(paste("Número de Reynolds para el petróleo crudo:", round(N_Re_petroleo, 2), "- Flujo:", tipo_flujo_petroleo))
print(paste("Número de Reynolds para la glicerina:", round(N_Re_glicerina, 2), "- Flujo:", tipo_flujo_glicerina))

