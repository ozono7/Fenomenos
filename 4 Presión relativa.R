# ==========================================================================
# 4 Presión relativa
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cálculo de la presión relativa y la altura del mercurio en un manómetro
# ==========================================================================


# 1. Definir las constantes y variables
densidad_liquido <- 800  # Densidad del líquido en kg/m³
densidad_agua <- 999.64  # Densidad del agua en kg/m³
densidad_relativa_liquido <- densidad_liquido / densidad_agua  # Relación con el agua

gamma_agua <- 999.64  # Peso específico del agua en kg/m³
gamma_liquido <- densidad_relativa_liquido * gamma_agua  # Peso específico del líquido en kg/m³
gamma_mercurio <- 13.6 * gamma_agua  # Peso específico del mercurio en kg/m³

P_vapor <- 120  # Presión del vapor en kPa (abs)
P_atm <- 101  # Presión atmosférica en kPa (abs)

h_liquido <- 1  # Altura del líquido en metros

# 2. Convertir presiones a kg/m²
P_vapor_kg_m2 <- P_vapor * 101.9716  # Conversión de kPa a kg/m²
P_atm_kg_m2 <- P_atm * 101.9716  # Conversión de kPa a kg/m²

# 3. Calcular la presión en el punto 4 (presión del líquido en el fondo)
P4 <- P_vapor_kg_m2 + (gamma_liquido * h_liquido)

# 4. Calcular la presión en el punto 2 (misma que P4)
P2 <- P4

# 5. Calcular la altura h del manómetro de mercurio
h <- (P4 - P_atm_kg_m2) / gamma_mercurio

# 6. Calcular la presión relativa en el indicador de presión
P_relativa <- P4 - P_atm_kg_m2  # Diferencia con la presión atmosférica

# 7. Convertir la presión relativa a kPa
P_relativa_kPa <- P_relativa / 101.9716

# 8. Mostrar los resultados
cat("Altura del mercurio en el manómetro:", round(h, 4), "m\n")
cat("Presión relativa en el indicador:", round(P_relativa_kPa, 2), "kPa\n")

