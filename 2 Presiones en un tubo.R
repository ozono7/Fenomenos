# 2 Presiones en un tubo
# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------
### Problema: Cálculo de las presiones P_A y P_B en un piezómetro
# Un tubo abierto (piezómetro) se conecta a un tanque con agua.
# El agua sube hasta una altura de 90 mm dentro del tubo.
# Se desea calcular las presiones P_A y P_B (en Pa) del aire por encima del agua, ignorando efectos capilares.

# 1. Definir las constantes y variables
P_atm <- 101325  # Presión atmosférica en Pa
densidad_agua <- 999.6  # Densidad del agua en kg/m^3
g <- 9.81  # Aceleración gravitacional en m/s^2

h1 <- 0.09  # Altura del agua en el piezómetro en metros (90 mm)
h2 <- 0.40  # Altura de referencia para P_B en metros
h3 <- 0.60  # Altura de referencia para P_A en metros

# 2. Calcular la presión P1 en la base del piezómetro
P1 <- P_atm + (densidad_agua * g * h1)

# 3. Calcular la presión P_B
P_B <- P1 - (densidad_agua * g * h2)

# 4. Calcular la presión P_A
P_A <- P1 - (densidad_agua * g * h3)

# 5. Mostrar los resultados
cat("Presión en la base del piezómetro (P1):", round(P1, 2), "Pa\n")
cat("Presión en el punto B (P_B):", round(P_B, 2), "Pa\n")
cat("Presión en el punto A (P_A):", round(P_A, 2), "Pa\n")

