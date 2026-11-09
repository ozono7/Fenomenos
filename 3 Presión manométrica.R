# ==========================================================================
# 3 Presión manométrica
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cálculo de la lectura del manómetro A en psi
# ==========================================================================


# 1. Definir las constantes y variables
densidad_relativa_aceite <- 0.75  # Densidad relativa del aceite
densidad_relativa_Hg <- 13.6  # Densidad relativa del mercurio
gamma_agua <- 0.03609  # Peso específico del agua en lb/in^3

h1 <- 10  # Altura del aceite en cm
h2 <- 4  # Altura del aceite en m
h3 <- 23  # Altura de la columna de mercurio en cm

# 2. Convertir alturas a pulgadas
h1_pulgadas <- h1 * 0.393701
h2_pulgadas <- h2 * 39.3701
h3_pulgadas <- h3 * 0.393701

# 3. Calcular los pesos específicos
gamma_Hg <- densidad_relativa_Hg * gamma_agua  # Peso específico del mercurio en lb/in^3
gamma_aceite <- densidad_relativa_aceite * gamma_agua  # Peso específico del aceite en lb/in^3

# 4. Definir la presión atmosférica en psi
P_atm <- 14.7  # Presión atmosférica en psi

# 5. Calcular la presión del aire en el depósito
P_aire <- P_atm - (gamma_Hg * h3_pulgadas)

# 6. Calcular la presión en el manómetro A
# Ecuación hidrostática del manómetro: al recorrer el manómetro desde A
# hasta el depósito se desciende por dos columnas de aceite, h2 y h1,
# por lo que ambas suman (gamma_aceite * h). h1 estaba definida y
# convertida pero no se usaba en la ecuación.
P_A <- (gamma_aceite * h2_pulgadas) + (gamma_aceite * h1_pulgadas) + P_aire

# 7. Mostrar la presión en el manómetro A
cat("Lectura del manómetro A:", round(P_A, 2), "psi\n")
