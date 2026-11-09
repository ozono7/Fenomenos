# Fenómenos de Transporte

Problemas resueltos en R de mecánica de fluidos y fenómenos de
transporte: medición de presión con manómetros, velocidad y tipo de
flujo en tuberías, pérdidas de carga por fricción y accesorios, potencia
de bombeo y costo de bombeo, rosa de vientos y dispersión gaussiana de
contaminantes. Cada archivo resuelve un problema con sus cálculos.

## Contenido

- **Presión y manómetros:** manómetro inclinado, manómetro Bourdon,
  presión manométrica y presión relativa.
- **Flujo en tuberías:** velocidad de flujo, número de Reynolds y tipo
  de flujo (laminar o turbulento), ecuación de Blasius y ecuación de
  Darcy-Weisbach.
- **Pérdidas de carga:** caída de presión en tuberías, fricción en
  accesorios, expansión repentina y flujo en ramales.
- **Sistemas de bombeo:** potencia de bomba, tiempo de descarga de
  tanque, NPSH, diferencia de alturas entre depósitos y costo de
  bombeo.
- **Simulación ambiental:** rosa de vientos a partir de mediciones de
  dirección y velocidad del viento (26) y dispersión gaussiana de
  contaminantes desde una chimenea, con comparación con la
  NOM-022-SSA1-2019 (27).
- **Apoyo:** ejercicios básicos de hidráulica, carta psicrométrica e
  introducción a R.

## Cómo usarlo

Cada archivo `.R` es un problema independiente y documentado: ábrelo en
R o RStudio y ejecútalo completo. Solo necesitas R instalado; los cálculos usan R base, sin paquetes adicionales.

Las lecciones 26 y 27 (simulación ambiental) usan tidyverse (ggplot2,
dplyr, readr) para leer los datos y hacer los gráficos, y cada una lee
su archivo de datos (`datos_viento.csv` y `datos_emision.csv`) del
mismo directorio. Si falta tidyverse, instalarlo una vez con
`install.packages("tidyverse")`. Cada gráfico se exporta en PNG a
300 dpi con `ggsave()`.

## Autor

**Dr. José Juan Torres Martínez**
Docente de Ciencias Químicas
Universidad Autónoma de San Luis Potosí (UASLP), México

Material académico para estudiantes.
