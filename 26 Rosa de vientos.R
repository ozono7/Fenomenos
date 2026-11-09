# ==========================================================================
# 26. Rosa de vientos: dirección y velocidad del viento en una planta
#    industrial
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "Fenómenos de Transporte" — Lección 26 (Simulación ambiental)
#
# Descripción: Se leen las mediciones horarias de dirección y velocidad
#   del viento de una estación meteorológica instalada en el patio de una
#   planta industrial (datos_viento.csv, octubre de 2026) y se construye
#   la rosa de vientos: frecuencia de ocurrencia por sector de dirección
#   (16 sectores de 22.5 grados) y velocidad media por sector. La
#   dirección del viento se registra con la convención meteorológica:
#   es la dirección DESDE LA QUE sopla el viento, en grados medidos
#   desde el norte y en sentido horario (0 grados = norte, 90 = este,
#   180 = sur, 270 = oeste). Todas las frecuencias y velocidades medias
#   se calculan A PARTIR DE LOS DATOS; nada está dado de antemano.
#
# Objetivos de aprendizaje:
#   - Leer mediciones reales de viento con read_csv().
#   - Separar las calmas (viento menor que 0.5 m/s), que no tienen
#     dirección definida.
#   - Clasificar cada medición en uno de 16 sectores de dirección.
#   - Calcular la frecuencia por sector y la velocidad media por sector.
#   - Construir la rosa de vientos con ggplot2 en coordenadas polares.
#
# Datos: archivo datos_viento.csv (240 mediciones, una cada 3 horas,
#   del 1 al 30 de octubre de 2026). Columnas: fecha_hora,
#   direccion_grados (grados, convención meteorológica) y
#   velocidad_ms (m/s). La estación es la de la planta; los datos son
#   de ejemplo para la lección.
#
# Paquetes: tidyverse (ggplot2, dplyr, readr). Los demás archivos del
#   repositorio usan R base; estas dos lecciones de simulación ambiental
#   usan tidyverse para los gráficos. Si falta, instalarlo una vez con
#   install.packages("tidyverse").
#
# Referencia: Organización Meteorológica Mundial (OMM), Guide to
#   Meteorological Instruments and Methods of Observation (WMO-No. 8):
#   la dirección del viento se reporta como la dirección desde la que
#   sopla, en grados desde el norte, y la rosa de vientos resume la
#   frecuencia por sector. El umbral de calma de 0.5 m/s es el valor
#   habitual de trabajo para anemómetros de cazoletas en estudios de
#   rosa de vientos.
# ==========================================================================

library(tidyverse)

# --------------------------------------------------------------------------
# 1. Lectura de los datos
# --------------------------------------------------------------------------

viento <- read_csv("datos_viento.csv", col_types = cols(
  fecha_hora = col_character(),
  direccion_grados = col_double(),
  velocidad_ms = col_double()
))

n_total <- nrow(viento)

# --------------------------------------------------------------------------
# 2. Calmas: mediciones con velocidad menor que 0.5 m/s
# --------------------------------------------------------------------------
# Con viento tan débil la veleta no marca una dirección confiable, así
# que esas mediciones se cuentan aparte como calmas y no entran en la
# rosa de vientos. La frecuencia por sector se calcula sobre las
# mediciones con dirección válida.

umbral_calma <- 0.5  # m/s

viento <- viento %>%
  mutate(calmo = velocidad_ms < umbral_calma)

n_calmas <- sum(viento$calmo)
porcentaje_calmas <- 100 * n_calmas / n_total

validas <- viento %>% filter(!calmo)
n_validas <- nrow(validas)

velocidad_media_total <- mean(viento$velocidad_ms)  # m/s, todas las mediciones
velocidad_media_validas <- mean(validas$velocidad_ms)  # m/s, sin calmas
velocidad_maxima <- max(viento$velocidad_ms)  # m/s

# --------------------------------------------------------------------------
# 3. Clasificación en 16 sectores de dirección
# --------------------------------------------------------------------------
# Cada sector mide 22.5 grados (360 / 16). El sector N cubre de 348.75
# a 11.25 grados, el NNE de 11.25 a 33.75, y así en sentido horario.
# Sumar 11.25 grados antes de dividir centra cada sector en su rumbo
# nominal (N = 0, NNE = 22.5, NE = 45, ...).

sectores <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
              "S", "SSO", "SO", "OSO", "O", "ONO", "NO", "NNO")

validas <- validas %>%
  mutate(
    indice_sector = ((direccion_grados + 11.25) %/% 22.5) %% 16 + 1,
    sector = factor(sectores[indice_sector], levels = sectores),
    clase_velocidad = cut(velocidad_ms,
                          breaks = c(0.5, 2, 4, 6, Inf),
                          labels = c("0.5-2 m/s", "2-4 m/s",
                                     "4-6 m/s", "> 6 m/s"),
                          right = FALSE)
  )

# --------------------------------------------------------------------------
# 4. Frecuencia por sector y velocidad media por sector
# --------------------------------------------------------------------------
# La frecuencia de un sector es el número de mediciones en ese sector
# dividido entre el número de mediciones con dirección válida, en
# porcentaje. La velocidad media por sector es el promedio de las
# velocidades medidas en ese sector.

rosa <- validas %>%
  count(sector, clase_velocidad, name = "n") %>%
  mutate(frecuencia_pct = 100 * n / n_validas)

velocidad_por_sector <- validas %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    frecuencia_pct = 100 * n() / n_validas,
    velocidad_media = mean(velocidad_ms),  # m/s
    .groups = "drop"
  ) %>%
  arrange(desc(frecuencia_pct))

sector_dominante <- velocidad_por_sector$sector[1]
frecuencia_dominante <- velocidad_por_sector$frecuencia_pct[1]

# --------------------------------------------------------------------------
# 5. Resultados en consola
# --------------------------------------------------------------------------

cat("----------------------------------------\n")
cat("Rosa de vientos — octubre de 2026\n")
cat("----------------------------------------\n")
cat("Mediciones totales:", n_total, "\n")
cat("Calmas (< 0.5 m/s):", n_calmas,
    "(", round(porcentaje_calmas, 1), "%)\n")
cat("Mediciones con dirección válida:", n_validas, "\n")
cat("Velocidad media (todas):", round(velocidad_media_total, 2), "m/s\n")
cat("Velocidad media (sin calmas):", round(velocidad_media_validas, 2), "m/s\n")
cat("Velocidad máxima:", round(velocidad_maxima, 1), "m/s\n")
cat("Sector dominante:", as.character(sector_dominante),
    "con", round(frecuencia_dominante, 1), "% de las mediciones válidas\n")
cat("----------------------------------------\n")
print(as.data.frame(velocidad_por_sector), row.names = FALSE)
cat("----------------------------------------\n")

# --------------------------------------------------------------------------
# 6. Gráfico 1: rosa de vientos
# --------------------------------------------------------------------------
# Cada barra es un sector de dirección; su longitud es la frecuencia
# en porcentaje y está dividida por clase de velocidad. La rosa muestra
# desde dónde sopla el viento: una barra larga hacia el NE significa
# que el viento sopla frecuentemente DESDE el noreste.

rosa_vientos <- ggplot(rosa, aes(x = sector, y = frecuencia_pct,
                                 fill = clase_velocidad)) +
  geom_col(width = 1, color = "white", linewidth = 0.3) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  scale_fill_brewer(palette = "Blues", name = "Velocidad del viento") +
  labs(
    title = "Rosa de vientos — Planta industrial, octubre de 2026",
    subtitle = paste0(n_validas, " mediciones con dirección válida; ",
                      round(porcentaje_calmas, 1),
                      "% de calmas (< 0.5 m/s) no graficadas"),
    x = "Dirección desde la que sopla el viento",
    y = "Frecuencia (%)",
    caption = "Dirección meteorológica: 0° = norte, 90° = este, 180° = sur, 270° = oeste."
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggsave("26_rosa_de_vientos.png", rosa_vientos,
       width = 8, height = 7, dpi = 300)

# Interpretación
#   Qué muestra: la frecuencia con la que el viento sopló desde cada
#   uno de los 16 sectores de dirección durante octubre de 2026, con
#   cada barra dividida por clase de velocidad del viento.
#   Ejes: el ángulo es la dirección DESDE LA QUE sopla el viento, en
#   grados desde el norte en sentido horario (0° = N arriba, 90° = E a
#   la derecha, 180° = S abajo, 270° = O a la izquierda). La distancia
#   desde el centro es la frecuencia en porcentaje (%), calculada sobre
#   las mediciones con dirección válida. El color indica la clase de
#   velocidad del viento en m/s (metros por segundo). La frecuencia es
#   adimensional en el sentido de que es un porcentaje, pero se expresa
#   en % sobre el total de mediciones válidas.
#   Patrón: la barra más larga apunta al NE, con 35.1% de las
#   mediciones válidas, seguida por el ENE con 21.2%. Juntas, las
#   direcciones del noreste y este-noreste concentran más de la mitad
#   del mes. Las velocidades más frecuentes en esos sectores están
#   entre 2 y 6 m/s. Los sectores del sur y suroeste casi no aparecen:
#   el viento rara vez sopló desde ahí en este mes.
#   Qué significa: en esta planta el viento dominante viene del
#   noreste. Cualquier emisión de la planta se transporta
#   preferentemente hacia el suroeste, que es la dirección opuesta a
#   la de origen del viento. Esa es la información que usa la
#   Lección 27 para saber hacia dónde se dispersa un contaminante.

# --------------------------------------------------------------------------
# 7. Gráfico 2: velocidad media por sector
# --------------------------------------------------------------------------

grafica_velocidad <- ggplot(velocidad_por_sector,
                            aes(x = sector, y = velocidad_media)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = velocidad_media_validas,
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  labs(
    title = "Velocidad media del viento por sector de dirección",
    subtitle = paste0("Línea punteada: media general sin calmas (",
                      round(velocidad_media_validas, 2), " m/s)"),
    x = "Sector de dirección (desde donde sopla el viento)",
    y = "Velocidad media (m/s)"
  ) +
  theme_minimal()

ggsave("26_velocidad_media_por_sector.png", grafica_velocidad,
       width = 8, height = 5, dpi = 300)

# Interpretación
#   Qué muestra: la velocidad media del viento en cada sector de
#   dirección, calculada a partir de las mediciones de ese sector.
#   Ejes: el eje x es el sector de dirección (categoría, sin unidades;
#   cada sector cubre 22.5 grados). El eje y es la velocidad media del
#   viento en m/s (metros por segundo). La línea punteada marca la
#   velocidad media general sin calmas, 3.84 m/s.
#   Patrón: las velocidades medias por sector son parecidas entre sí,
#   entre 2.2 y 5.0 m/s. Los sectores O y SE tienen las medias más
#   altas (4.95 y 4.74 m/s), pero son poco frecuentes (1.8% y 3.6% de
#   las mediciones), así que aportan poco al promedio general. El
#   sector dominante NE tiene una media de 3.81 m/s, prácticamente
#   igual a la media general. El sector SSO tiene la media más baja
#   (2.2 m/s) con solo 3 mediciones.
#   Qué significa: la velocidad del viento no depende mucho de la
#   dirección en este mes; el viento es moderado (alrededor de 4 m/s)
#   venga de donde venga. Para la dispersión de contaminantes, lo
#   que cambia con la dirección es hacia dónde va el penacho, no qué
#   tan rápido se diluye, porque la velocidad es parecida en todos
#   los sectores.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
#   - La dirección del viento se reporta con la convención
#     meteorológica: es la dirección DESDE LA QUE sopla, en grados
#     desde el norte y en sentido horario.
#   - Las calmas (menos de 0.5 m/s) no tienen dirección confiable y se
#     cuentan aparte: en este mes fueron 7.5% de las mediciones.
#   - La rosa de vientos resume 240 mediciones en 16 frecuencias por
#     sector: el viento dominante sopló desde el NE (35.1%) y el ENE
#     (21.2%).
#   - La velocidad media del viento fue 3.57 m/s con todas las
#     mediciones y 3.84 m/s sin contar las calmas; el máximo medido
#     fue 7.6 m/s.
#   - La dirección dominante dice hacia dónde se transporta lo que
#     emite la planta: del NE hacia el SO. La Lección 27 usa esa
#     idea para modelar la dispersión de un contaminante.
