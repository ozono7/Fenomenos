# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Factor de Fricción en Accesorios  
# Objetivo: Calcular la pérdida de carga total y el factor de fricción equivalente en accesorios  

# Datos del problema  
Q <- 0.01  # Flujo volumétrico en m³/s  
D <- 0.1  # Diámetro de la tubería en m  
L <- 50  # Longitud de la tubería en m  
g <- 9.81  # Aceleración debida a la gravedad en m/s²  

# Coeficientes de pérdida (K) para los accesorios  
K_codo <- 0.9  # Coeficiente de pérdida para un codo de 90°  
K_valvula <- 0.2  # Coeficiente de pérdida para una válvula de compuerta  
K_tee <- 1.8  # Coeficiente de pérdida para un tee estándar  

# Número de accesorios  
n_codos <- 2  # Número de codos de 90°  
n_valvulas <- 1  # Número de válvulas de compuerta  
n_tees <- 1  # Número de tees estándar  

# Cálculo del área de la sección transversal de la tubería  
A <- (pi * D^2) / 4  # Área en m²  

# Cálculo de la velocidad del flujo  
v <- Q / A  # Velocidad en m/s  

# Cálculo de la pérdida de carga total debido a los accesorios  
hL_acc <- (n_codos * K_codo + n_valvulas * K_valvula + n_tees * K_tee) * (v^2 / (2 * g))  

# Cálculo del factor de fricción equivalente (f_eq)  
f_eq <- (hL_acc * D * 2 * g) / (L * v^2)  

# Mostrar resultados  
cat("Resultados del problema:\n")  
cat("----------------------------------------\n")  
cat("Pérdida de carga total debido a los accesorios (hL_acc):", round(hL_acc, 4), "m\n")  
cat("Factor de fricción equivalente (f_eq):", round(f_eq, 4), "\n")  
cat("----------------------------------------\n")  

