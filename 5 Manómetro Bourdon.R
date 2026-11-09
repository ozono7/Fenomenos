# ==========================================================================
# 5 Manómetro Bourdon
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cálculo de la elevación en los piezómetros y lectura del manómetro de mercurio
# ==========================================================================


# 1. Definir las constantes y variables
densidad_agua <- 999.98  # Densidad del agua en kg/m³
S1 <- 0.7  # Densidad relativa del fluido en E
S2 <- 1.0  # Densidad relativa del fluido en F
S3 <- 1.6  # Densidad relativa del fluido en G
S4 <- 13.6  # Densidad relativa del mercurio

P1 <- -17.65  # Presión manométrica en kPa
P1_kg_m2 <- P1 * 101.9716  # Convertir presión a kg/m²

# 2. Calcular la altura en el piezómetro E
h1 <- P1_kg_m2 / (S1 * densidad_agua)
altura_E <- 15 - h1

# 3. Calcular la presión en P4 y la altura en el piezómetro F
P4 <- P1_kg_m2 + (15 - 12) * (S1 * densidad_agua)
h2 <- P4 / (S2 * densidad_agua)
altura_F <- 12 + h2

# 4. Calcular la presión en P6 y la altura en el piezómetro G
P6 <- P4 + (12 - 8) * (S2 * densidad_agua)
h3 <- P6 / (S3 * densidad_agua)
altura_G <- 8 + h3

# 5. Calcular la presión en P9 y la altura en el manómetro de mercurio
# El tramo 8 -> 4 m contiene el fluido G (S3): cada tramo usa el fluido
# del piezómetro de su extremo superior (E en 15 -> 12 m, F en 12 -> 8 m,
# G en 8 -> 4 m).
P9 <- P6 + (8 - 4) * (S3 * densidad_agua)
H1 <- P9 / (S4 * densidad_agua)
altura_Hg <- 4 + H1

# 6. Mostrar los resultados
cat("Altura en el piezómetro E:", round(altura_E, 4), "m\n")
cat("Altura en el piezómetro F:", round(altura_F, 4), "m\n")
cat("Altura en el piezómetro G:", round(altura_G, 4), "m\n")
cat("Lectura del manómetro de mercurio H:", round(altura_Hg, 4), "m\n")


