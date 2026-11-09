# ==========================================================================
# 1-Manómetro inclinado
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Relación entre el desplazamiento del aceite en un manómetro inclinado y la diferencia de presión
# ==========================================================================


# 1. Definir las constantes y variables
densidad_relativa <- 0.827
densidad_agua <- 999.64  # kg/m^3
densidad_aceite <- densidad_relativa * densidad_agua  # kg/m^3

angulo <- 20  # grados
radio_tubo <- 1  # mm (Diámetro de 2 mm)
radio_deposito <- 15  # mm (Diámetro de 30 mm)
g <- 9.81  # Aceleración gravitacional en m/s^2

# 2. Calcular las áreas del tubo y el depósito
area_tubo <- pi * (radio_tubo)^2  # mm^2
area_deposito <- pi * (radio_deposito)^2  # mm^2

# 3. Calcular la relación ΔP/ΔX
# ΔP = densidad_aceite * g * ΔX * (sin(ángulo) + A_tubo / A_depósito)
relacion_presion_desplazamiento <- densidad_aceite * g * (sin(angulo * pi / 180) + (area_tubo / area_deposito))

# 4. Convertir el resultado a Pa/mm (de Pa/m a Pa/mm)
relacion_presion_desplazamiento_pa_mm <- relacion_presion_desplazamiento / 1000

# 5. Mostrar el resultado
cat("Relación entre la diferencia de presión y el desplazamiento del aceite en el tubo inclinado:", 
    round(relacion_presion_desplazamiento_pa_mm, 4), "Pa/mm\n")


