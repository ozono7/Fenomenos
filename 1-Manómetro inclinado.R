#1-Manómetro inclinado

# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------
### Problema: Relación entre el desplazamiento del aceite en un manómetro inclinado y la diferencia de presión
# Un manómetro inclinado con un tubo de 2 mm de diámetro y un depósito de 30 mm de diámetro tiene un ángulo de inclinación de 20°.
# Se usa aceite rojo manométrico con una densidad relativa de 0.827.
# Se desea calcular la relación entre la medida del desplazamiento del aceite en el tubo inclinado (en mm) y la diferencia de presión (en Pa).

# 1. Definir las constantes y variables
densidad_relativa <- 0.827
densidad_agua <- 999.64  # kg/m^3
densidad_aceite <- densidad_relativa * densidad_agua  # kg/m^3

angulo <- 20  # grados
radio_tubo <- 1  # mm (Diámetro de 2 mm)
radio_deposito <- 15  # mm (Diámetro de 30 mm)

# 2. Calcular las áreas del tubo y el depósito
area_tubo <- pi * (radio_tubo)^2  # mm^2
area_deposito <- pi * (radio_deposito)^2  # mm^2

# 3. Calcular la relación ΔP/ΔX
relacion_presion_desplazamiento <- densidad_aceite * (sin(angulo * pi / 180) + (area_tubo / area_deposito))

# 4. Convertir el resultado a Pa/mm
relacion_presion_desplazamiento_pa_mm <- relacion_presion_desplazamiento * (101325 / 10333) * (1 / 1000)

# 5. Mostrar el resultado
cat("Relación entre la diferencia de presión y el desplazamiento del aceite en el tubo inclinado:", 
    round(relacion_presion_desplazamiento_pa_mm, 4), "Pa/mm\n")


