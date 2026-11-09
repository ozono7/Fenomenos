# ==========================================================================
# 27. Dispersión gaussiana de contaminantes: modelo de penacho desde una
#    chimenea
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Fenómenos de Transporte" — Lección 27 (Simulación ambiental)
#
# Descripción: Se modela la dispersión de dióxido de azufre (SO2)
#   emitido por la chimenea de una planta industrial con el modelo
#   gaussiano de penacho en estado estacionario. La tasa de emisión Q
#   y la velocidad del viento u se calculan A PARTIR DE LAS MEDICIONES
#   del archivo datos_emision.csv (10 días de medición en la chimenea);
#   la altura de la chimenea H es la medida en la planta. Con esos
#   datos se calcula la concentración de SO2 a nivel del suelo a
#   distintas distancias viento abajo y se dibuja el mapa de la zona
#   afectada. El viento dominante sopla desde el NE (Lección 26), así
#   que el penacho se dirige hacia el SO: el eje x del modelo apunta
#   en la dirección del viento.
#
# Objetivos de aprendizaje:
#   - Calcular la tasa media de emisión y la velocidad media del viento
#     a partir de mediciones.
#   - Aplicar la ecuación gaussiana de penacho con los coeficientes de
#     dispersión de Briggs para terreno abierto.
#   - Calcular la concentración a nivel del suelo sobre el eje del
#     penacho y fuera de él.
#   - Encontrar la distancia a la que ocurre la concentración máxima
#     y compararla con los valores límite de la NOM-022-SSA1-2019.
#   - Dibujar el mapa de la zona afectada por el contaminante.
#
# Datos: archivo datos_emision.csv (10 días, 21 al 30 de octubre de
#   2026). Columnas: fecha, altura_chimenea_m (m, medida en la planta),
#   emision_SO2_gs (g/s, medida en la chimenea), velocidad_viento_ms
#   (m/s, medida a la altura de la chimenea) y clase_estabilidad
#   (clase de Pasquill-Gifford, D = neutra en los 10 días).
#
# Paquetes: tidyverse (ggplot2, dplyr, readr). Ver la nota de paquetes
#   en la Lección 26.
#
# Modelo (referencias):
#   Ecuación gaussiana de penacho para una fuente puntual continua en
#   estado estacionario, con reflexión en el suelo (Seinfeld y Pandis,
#   Atmospheric Chemistry and Physics, 2016; Boubel et al., Fundamentals
#   of Air Pollution, 1994):
#
#   C(x, y, z) = Q / (2 * pi * u * sy * sz) * exp(-y^2 / (2 * sy^2))
#                * [exp(-(z - H)^2 / (2 * sz^2))
#                   + exp(-(z + H)^2 / (2 * sz^2))]
#
#   con Q = tasa de emisión, u = velocidad del viento, sy y sz =
#   coeficientes de dispersión horizontal y vertical (m), H = altura
#   efectiva de emisión (m). A nivel del suelo (z = 0) y sobre el eje
#   del penacho (y = 0) se reduce a:
#
#   C(x, 0, 0) = Q / (pi * u * sy * sz) * exp(-H^2 / (2 * sz^2))
#
#   Coeficientes de dispersión de Briggs (1973) para terreno abierto,
#   válidos para 100 m < x < 10 000 m (Briggs, G. A., Diffusion
#   estimation for small emissions, NOAA, 1973):
#
#   Clase  sy (m)                          sz (m)
#   A      0.22x(1+0.0001x)^-0.5           0.20x
#   B      0.16x(1+0.0001x)^-0.5           0.12x
#   C      0.11x(1+0.0001x)^-0.5           0.08x(1+0.0002x)^-0.5
#   D      0.08x(1+0.0001x)^-0.5           0.06x(1+0.0015x)^-0.5
#   E      0.06x(1+0.0001x)^-0.5           0.03x(1+0.0003x)^-1
#   F      0.04x(1+0.0001x)^-0.5           0.016x(1+0.0003x)^-1
#
#   con x en metros. Clases de estabilidad de Pasquill-Gifford: A =
#   muy inestable, B = moderadamente inestable, C = ligeramente
#   inestable, D = neutra, E = ligeramente estable, F = moderadamente
#   estable.
#
#   Valores límite de SO2 en aire ambiente, NOM-022-SSA1-2019
#   (DOF 20-08-2019): 0.075 ppm como promedio de 1 hora y 0.04 ppm
#   como promedio de 24 horas. Conversión a 25 °C y 1 atm
#   (1 ppm de SO2 = 2620 µg/m3, masa molar 64.07 g/mol): 0.075 ppm =
#   196.5 µg/m3 y 0.04 ppm = 104.8 µg/m3.
#
# Supuestos del cálculo: altura efectiva H igual a la altura física de la
#   chimenea (30 m), es decir, sin sobreelevación del penacho por
#   flotación ni por momento; terreno plano; viento constante en
#   dirección e intensidad durante el periodo modelado; clase de
#   estabilidad D en todo el periodo; sin deposición, sin lavado por
#   lluvia y sin reacciones químicas del SO2 en el aire. El modelo
#   estima concentraciones medias horarias, no picos instantáneos.
# ==========================================================================

library(tidyverse)

# --------------------------------------------------------------------------
# 1. Lectura de las mediciones de emisión
# --------------------------------------------------------------------------

emision <- read_csv("datos_emision.csv", col_types = cols(
  fecha = col_character(),
  altura_chimenea_m = col_double(),
  emision_SO2_gs = col_double(),
  velocidad_viento_ms = col_double(),
  clase_estabilidad = col_character()
))

# --------------------------------------------------------------------------
# 2. Parámetros de la fuente calculados a partir de los datos
# --------------------------------------------------------------------------
# Q es el promedio de las tasas de emisión medidas en 10 días y u es
# el promedio de las velocidades de viento medidas a la altura de la
# chimenea en esos mismos días. H es la altura medida de la chimenea.

Q_gs <- mean(emision$emision_SO2_gs)  # g/s
Q_ugs <- Q_gs * 1e6  # µg/s (1 g = 1e6 µg)
u_ms <- mean(emision$velocidad_viento_ms)  # m/s
H_m <- unique(emision$altura_chimenea_m)  # m
clase_usada <- unique(emision$clase_estabilidad)  # clase de Pasquill-Gifford

# Valores límite de SO2, NOM-022-SSA1-2019 (ver encabezado)
limite_1h <- 196.5  # µg/m3, promedio de 1 hora (0.075 ppm)
limite_24h <- 104.8  # µg/m3, promedio de 24 horas (0.04 ppm)

# --------------------------------------------------------------------------
# 3. Coeficientes de dispersión de Briggs para terreno abierto
# --------------------------------------------------------------------------

tabla_briggs <- tibble(
  clase = c("A", "B", "C", "D", "E", "F"),
  a_y = c(0.22, 0.16, 0.11, 0.08, 0.06, 0.04),
  a_z = c(0.20, 0.12, 0.08, 0.06, 0.03, 0.016),
  b_z = c(0, 0, 0.0002, 0.0015, 0.0003, 0.0003),
  e_z = c(0, 0, -0.5, -0.5, -1, -1)
)

coef_clase <- tabla_briggs %>% filter(clase == clase_usada)

sigma_y <- function(x_m) {
  coef_clase$a_y * x_m * (1 + 0.0001 * x_m)^(-0.5)
}

sigma_z <- function(x_m) {
  coef_clase$a_z * x_m * (1 + coef_clase$b_z * x_m)^coef_clase$e_z
}

# Concentración gaussiana a nivel del suelo (z = 0) en un punto
# (x_m, y_m), con x viento abajo de la fuente e y transversal al eje.
concentracion <- function(x_m, y_m) {
  sy <- sigma_y(x_m)
  sz <- sigma_z(x_m)
  Q_ugs / (2 * pi * u_ms * sy * sz) *
    exp(-y_m^2 / (2 * sy^2)) *
    (exp(-H_m^2 / (2 * sz^2)) + exp(-H_m^2 / (2 * sz^2)))
}

# --------------------------------------------------------------------------
# 4. Concentración sobre el eje del penacho (y = 0, z = 0)
# --------------------------------------------------------------------------

distancias <- seq(100, 5000, by = 25)  # m, dentro del rango de Briggs

perfil <- tibble(
  x_m = distancias,
  sigma_y_m = sigma_y(distancias),
  sigma_z_m = sigma_z(distancias),
  C_ug_m3 = concentracion(distancias, 0)
)

x_max <- perfil$x_m[which.max(perfil$C_ug_m3)]  # m
C_max <- max(perfil$C_ug_m3)  # µg/m3

# Primera distancia después del máximo en la que la concentración
# baja del valor límite de 24 horas.
pos_cruce <- which(perfil$x_m > x_max & perfil$C_ug_m3 < limite_24h)
x_cruce_24h <- perfil$x_m[min(pos_cruce)]  # m

# --------------------------------------------------------------------------
# 5. Resultados en consola
# --------------------------------------------------------------------------

cat("----------------------------------------\n")
cat("Dispersión gaussiana de SO2 — chimenea de 30 m, clase D\n")
cat("----------------------------------------\n")
cat("Tasa media de emisión Q:", round(Q_gs, 2), "g/s\n")
cat("Velocidad media del viento u:", round(u_ms, 2), "m/s\n")
cat("Altura de la chimenea H:", H_m, "m\n")
cat("Clase de estabilidad:", clase_usada, "(neutra)\n")
cat("Concentración máxima a nivel del suelo:", round(C_max, 1),
    "µg/m3 a", x_max, "m de la chimenea\n")
cat("Valor límite NOM-022 (1 hora):", limite_1h, "µg/m3\n")
cat("Valor límite NOM-022 (24 horas):", limite_24h, "µg/m3\n")
cat("La concentración baja del límite de 24 h a partir de",
    x_cruce_24h, "m viento abajo\n")
cat("----------------------------------------\n")
print(as.data.frame(perfil[perfil$x_m %% 500 == 0, ]),
      row.names = FALSE)
cat("----------------------------------------\n")

# --------------------------------------------------------------------------
# 6. Gráfico 1: concentración a nivel del suelo contra distancia
# --------------------------------------------------------------------------

grafica_perfil <- ggplot(perfil, aes(x = x_m, y = C_ug_m3)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(data = perfil %>% filter(x_m == x_max),
             aes(x = x_m, y = C_ug_m3),
             color = "darkred", size = 3) +
  geom_vline(xintercept = x_max, linetype = "dotted",
             color = "darkred", linewidth = 0.7) +
  geom_hline(yintercept = limite_1h, linetype = "dashed",
             color = "darkred", linewidth = 0.8) +
  geom_hline(yintercept = limite_24h, linetype = "dashed",
             color = "darkorange", linewidth = 0.8) +
  annotate("text", x = 4800, y = limite_1h * 1.25,
           label = "Límite 1 h NOM-022: 196.5 µg/m³",
           hjust = 1, size = 3.5, color = "darkred") +
  annotate("text", x = 4800, y = limite_24h * 1.25,
           label = "Límite 24 h NOM-022: 104.8 µg/m³",
           hjust = 1, size = 3.5, color = "darkorange") +
  annotate("text", x = x_max, y = C_max * 1.35,
           label = paste0("Máximo: ", round(C_max, 1),
                          " µg/m³ a ", x_max, " m"),
           hjust = 0.5, size = 3.5, color = "darkred") +
  scale_y_log10() +
  labs(
    title = "Concentración de SO2 a nivel del suelo sobre el eje del penacho",
    subtitle = paste0("Q = ", round(Q_gs, 2), " g/s, u = ",
                      round(u_ms, 2), " m/s, H = ", H_m,
                      " m, clase de estabilidad ", clase_usada,
                      " (Briggs, terreno abierto)"),
    x = "Distancia viento abajo de la chimenea (m)",
    y = "Concentración de SO2 (µg/m³, escala logarítmica)"
  ) +
  theme_minimal()

ggsave("27_perfil_concentracion_SO2.png", grafica_perfil,
       width = 9, height = 5.5, dpi = 300)

# Interpretación
#   Qué muestra: la concentración de SO2 a nivel del suelo sobre el
#   eje del penacho (justo en la línea del viento, y = 0), desde 100 m
#   hasta 5000 m viento abajo de la chimenea, con los dos valores
#   límite de la NOM-022-SSA1-2019.
#   Ejes: el eje x es la distancia viento abajo de la chimenea en
#   metros (m). El eje y es la concentración de SO2 en microgramos por
#   metro cúbico (µg/m³), en escala logarítmica porque la concentración
#   cambia en varios órdenes de magnitud entre 100 m y 5000 m.
#   Patrón: cerca de la chimenea la concentración a nivel del suelo
#   es casi cero, porque el penacho sale a 30 m de altura y todavía
#   no ha bajado hasta el suelo. La concentración sube al alejarse,
#   alcanza su máximo de 196.1 µg/m³ a 425 m de la chimenea y después
#   baja de forma continua al diluirse el penacho: a 1000 m ya es
#   102.1 µg/m³, a 2000 m es 40.7 µg/m³ y a 5000 m es 11.5 µg/m³.
#   Qué significa: el máximo calculado, 196.1 µg/m³, queda
#   prácticamente sobre el valor límite de 1 hora de la norma
#   (196.5 µg/m³). No se puede afirmar que la emisión cumple la
#   norma con seguridad, porque la diferencia es menor que la
#   incertidumbre propia del modelo y de las mediciones; la emisión
#   está en el límite y cualquier aumento de Q, o un viento más
#   débil, la pondría por encima. Respecto al límite de 24 horas
#   (104.8 µg/m³), la concentración lo rebasa entre unos 250 m y
#   1000 m viento abajo de la chimenea: esa franja es la zona que
#   requiere atención.

# --------------------------------------------------------------------------
# 7. Gráfico 2: mapa de la zona afectada
# --------------------------------------------------------------------------
# Malla de puntos (x viento abajo, y transversal al eje del penacho)
# con la concentración a nivel del suelo en cada punto. El eje x del
# mapa apunta en la dirección del viento (hacia el SO, según la
# Lección 26, porque el viento dominante sopla desde el NE).

malla <- expand.grid(
  x_m = seq(100, 3000, by = 50),
  y_m = seq(-600, 600, by = 25)
) %>%
  mutate(C_ug_m3 = concentracion(x_m, y_m))

mapa <- ggplot(malla, aes(x = x_m, y = y_m, fill = pmax(C_ug_m3, 0.1))) +
  geom_raster() +
  geom_contour(aes(z = C_ug_m3), color = "white",
               breaks = c(25, 50, limite_24h, limite_1h),
               linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "white", linewidth = 0.7) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 3,
             inherit.aes = FALSE) +
  annotate("text", x = 60, y = 90, label = "Chimenea",
           hjust = 0, size = 3.5, color = "white") +
  scale_fill_viridis_c(trans = "log10",
                       name = "SO2 (µg/m³)",
                       option = "viridis") +
  coord_fixed(ratio = 1) +
  labs(
    title = "Zona afectada por la emisión de SO2 (nivel del suelo)",
    subtitle = "El viento sopla desde el NE: el penacho avanza hacia el SO (eje x). La línea punteada es el eje del penacho.",
    x = "Distancia viento abajo (m)",
    y = "Distancia transversal al eje del penacho (m)"
  ) +
  theme_minimal()

ggsave("27_mapa_zona_afectada_SO2.png", mapa,
       width = 9, height = 6, dpi = 300)

# Interpretación
#   Qué muestra: la concentración de SO2 a nivel del suelo en el
#   plano alrededor de la chimenea, con el viento soplando en la
#   dirección del eje x (hacia el SO). Los contornos blancos marcan
#   25, 50, 104.8 y 196.5 µg/m³.
#   Ejes: el eje x es la distancia viento abajo de la chimenea en
#   metros (m), de 100 a 3000 m. El eje y es la distancia transversal
#   al eje del penacho en metros (m): valores positivos y negativos
#   indican a un lado y al otro del eje. El color representa la
#   concentración de SO2 en µg/m³, en escala logarítmica.
#   Patrón: el penacho forma una franja alargada sobre el eje del
#   viento, angosta cerca de la chimenea y cada vez más ancha al
#   alejarse, porque los coeficientes de dispersión sy y sz crecen
#   con la distancia. La zona con más de 104.8 µg/m³ (límite de
#   24 horas) es una franja de unos 750 m de largo, de unos 250 m a
#   unos 1000 m de la chimenea: a 500 m de la chimenea mide unos
#   85 m de ancho total, y se va cerrando hasta desaparecer cerca
#   de los 1000 m. Fuera de esa franja la
#   concentración cae rápido al alejarse del eje: a 500 m de la
#   chimenea, a 100 m del eje la concentración ya es de solo
#   7.2 µg/m³.
#   Qué significa: la zona afectada es una franja angosta y alargada
#   viento abajo de la chimenea, no un círculo alrededor de la
#   planta. Cualquier receptor (viviendas, escuela, cultivo) que
#   esté dentro de esa franja, entre unos 250 m y 1000 m viento
#   abajo y a menos de unos 46 m del eje, es el que recibe las
#   concentraciones más altas. Esa es la zona que la planta debe
#   vigilar primero.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
#   - El modelo gaussiano de penacho calcula la concentración de un
#     contaminante a partir de la tasa de emisión Q (g/s), la
#     velocidad del viento u (m/s), la altura efectiva H (m) y los
#     coeficientes de dispersión sy y sz (m), que crecen con la
#     distancia y dependen de la clase de estabilidad.
#   - Los coeficientes de Briggs para terreno abierto dan sy y sz
#     para cada clase de Pasquill-Gifford; aquí se usó la clase D
#     (neutra), válida para 100 m < x < 10 000 m.
#   - La concentración a nivel del suelo es casi cero junto a la
#     chimenea, sube hasta un máximo (196.1 µg/m³ a 425 m) y luego
#     baja al diluirse el penacho.
#   - El máximo calculado queda prácticamente sobre el valor límite
#     de 1 hora de la NOM-022-SSA1-2019 (196.5 µg/m³), así que la
#     emisión está en el límite; y rebasa el límite de 24 horas
#     (104.8 µg/m³) entre unos 250 m y 1000 m viento abajo.
#   - La zona afectada es una franja alargada en la dirección del
#     viento (hacia el SO, según la rosa de vientos de la
#     Lección 26), angosta cerca de la fuente y más ancha lejos.
#   - El modelo supone altura efectiva igual a la altura física,
#     terreno plano, viento constante y clase D: es una estimación
#     de primer orden, no una medición.
