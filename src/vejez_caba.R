rm(list = ls())

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)

# RUTA DEL ARCHIVO

archivo <- "censo/edad_radio.xlsX"
carto <- st_read("censo/fracciones_censales/fracciones_censales.shp")
carto_caba <- carto %>% 
  filter(cpr == "02") %>% 
  mutate(cod_indec = as.character(cod_indec))

# 1. CARGA CRUDA
# Saltamos las primeras 13 filas que son puro título del reporte.
# Importante: col_names = FALSE para que R no intente adivinar encabezados.
df_raw <- read_excel(archivo, col_names = FALSE, skip =13)
df_raw <- df_raw %>% 
  select(-...1)

# 2. Definir dónde empiezan los datos
# Si el patrón es constante cada 28 filas y son 15 comunas:
filas_inicio <- seq(from = 5, by = 28, length.out = 15)

# 3. Función para extraer cada comuna
extraer_comuna <- function(fila_start, indice_comuna) {
  
  # Definimos fila de fin (23 filas de datos: start + 22)
  fila_end <- fila_start + 22
  
  # Extraemos el bloque (slice)
  bloque <- df_raw %>%
    slice(fila_start:fila_end) %>%
    select(1:4) # Seleccionamos solo las 4 columnas que te interesan
  
  # Asignamos nombres manualmente
  colnames(bloque) <- c("EDAD", "MUJER", "HOMBRE", "TOTAL")
  
  # Agregamos la columna de identificación
  # Opción A: Usar el número secuencial (1, 2, 3...)
  bloque$COMUNA <- paste("Comuna", indice_comuna)
  
  # Opción B (Más robusta): Leer el nombre real desde el Excel si está arriba
  # Por ejemplo, si el título siempre está 4 filas antes de los datos:
  # nombre_real <- df_raw[fila_start - 4, 1] 
  # bloque$COMUNA <- as.character(nombre_real)
  
  # Reordenar para que COMUNA sea la primera
  bloque <- bloque %>% select(COMUNA, everything())
  
  return(bloque)
}

# 4. Ejecutar la magia (Iterar y Unir)
# map2_dfr aplica la función a cada fila de inicio y une los resultados en un solo DF
df_final <- map2_dfr(filas_inicio, 1:15, extraer_comuna)

# 5. Verificación y Limpieza final de tipos
df_final <- df_final %>%
  mutate(
    # Convertir a numérico lo que sea necesario
    MUJER = as.numeric(MUJER),
    HOMBRE = as.numeric(HOMBRE),
    TOTAL = as.numeric(TOTAL)
  )


#############################################################################################
#TRANSFORMACIÓN DE DF EN LARGO

df_largo <- df_final %>%
  # Quitamos la columna TOTAL porque ensucia el gráfico (la suma de M+H ya es el total)
  select(-TOTAL) %>% 
  pivot_longer(
    cols = c(MUJER, HOMBRE),
    names_to = "SEXO",
    values_to = "POBLACION"
  )


#PIRAMIDE

# 1. Preparar datos para pirámide
datos_piramide <- df_largo %>%
  mutate(
    # Truco: Hombres a la izquierda (negativo), Mujeres a la derecha (positivo)
    POB_GRAFICO = ifelse(SEXO == "HOMBRE", -POBLACION, POBLACION),
    
    # IMPORTANTE: Asegurar el orden de las edades
    # Convertimos EDAD a "factor" usando el orden en que aparecen en el excel
    # Si no haces esto, ggplot ordenará alfabéticamente (y "10 a 14" saldría antes que "5 a 9")
    EDAD = factor(EDAD, levels = unique(EDAD)) 
  )


# 2. Graficar
ggplot(datos_piramide %>% 
         filter(EDAD != "Total"), aes(x = EDAD, y = POB_GRAFICO, fill = SEXO)) +
  geom_col(width = 0.8) + # Barras
  coord_flip() +          # Girar para que quede horizontal
  
  # Estética
  theme_minimal() +
  scale_fill_manual(values = c("HOMBRE" = "#2c7bb6", "MUJER" = "#d7191c")) + 
  
  # Arreglar el eje X para que no muestre números negativos
  scale_y_continuous(labels = abs) + 
  
  labs(
    title = "Pirámide de Población - Censo 2022",
    subtitle = "Comparativa por Sexo y Edad",
    x = "Grupo de Edad",
    y = "Población",
    caption = "Fuente: INDEC, Censo Nacional de Población, Hogares y Viviendas 2022"
  )+
  facet_wrap(~ COMUNA, scales = "free_x")


#############################################################
#Envejecimiento

# Paso 1: Clasificar las edades de tu tabla en los 3 grandes grupos
# Nota: Definimos manualmente qué texto corresponde a qué grupo para no errar.
df_indicadores <- df_final %>%
  mutate(grupo_demografico = case_when(
    EDAD %in% c("00 a 04", "05 a 09", "10 a 14") ~ "jovenes_0_14",
    EDAD %in% c("15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", 
                "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "activos_15_64",
    TRUE ~ "mayores_65_mas" # Asumimos que todo lo demás (65 en adelante) es mayor
  )) %>%
  
  # Paso 2: Sumar la población TOTAL por Comuna y Grupo
  group_by(COMUNA, grupo_demografico) %>%
  summarise(poblacion = sum(TOTAL), .groups = "drop") %>%
  
  # Paso 3: Pivotar para tener columnas separadas (necesario para la fórmula)
  pivot_wider(names_from = grupo_demografico, values_from = poblacion) %>%
  
  # Paso 4: Calcular el índice
  mutate(
    indice_envejecimiento = (mayores_65_mas / jovenes_0_14) * 100,
    indice_dependencia = ((jovenes_0_14 + mayores_65_mas) / activos_15_64) * 100
  ) %>%
  arrange(desc(indice_envejecimiento)) # Ordenar del más envejecido al más joven

# Ver la tabla de resultados
print(df_indicadores)
#2. Visualizar los Resultados
#Los números solos son fríos. Un gráfico de barras ordenado te mostrará claramente la disparidad entre el Norte (más envejecido) y el Sur (más joven) de la ciudad.

#R

ggplot(df_indicadores, aes(x = reorder(COMUNA, indice_envejecimiento), y = indice_envejecimiento)) +
  geom_col(fill = "#4e79a7") + # Color azul acero
  geom_text(aes(label = round(indice_envejecimiento, 1)), 
            hjust = -0.2, size = 3.5) + # Etiquetas con el número
  coord_flip() + # Barras horizontales para leer bien los nombres
  theme_minimal() +
  labs(
    title = "Índice de Envejecimiento por Comuna (Censo 2022)",
    subtitle = "Número de personas de 65+ años por cada 100 menores de 15 años",
    x = "",
    y = "Índice de Envejecimiento",
    caption = "Fuente: Elaboración propia en base a INDEC"
  ) +
  theme(panel.grid.major.y = element_blank()) # Limpiar líneas de fondo

########################################################################################
#Mapa

library(sf)
library(tidyverse) # Ya lo tienes cargado, pero por las dudas

# URL oficial del Portal de Datos Abiertos de CABA
url_comunas <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas.geojson"

# Leemos el mapa
mapa_caba <- st_read("data/comunas/comunas_wgs84.shp")

# Miremos qué tiene adentro para saber cómo unir
# Usualmente tiene una columna 'COMUNAS' con el número (ej: 1, 2, 3...)
head(mapa_caba)

# Limpiamos tu tabla de indicadores para tener un ID numérico limpio
df_mapa_data <- df_indicadores %>%
  mutate(
    # Extraemos solo el número de la columna "Comuna 1", "Comuna 2"...
    # 'readr::parse_number' es genial para esto.
    COMUNAS = readr::parse_number(COMUNA) 
  )

# Aseguramos que el ID en el mapa también sea numérico para que el join funcione
mapa_caba <- mapa_caba %>%
  mutate(COMUNAS = as.numeric(COMUNAS))

mapa_final <- mapa_caba %>%
  left_join(df_mapa_data, by = "COMUNAS")

ggplot(mapa_final) +
  # Capa del mapa
  geom_sf(aes(fill = indice_envejecimiento), color = "white", lwd = 0.2) +
  
  # Etiquetas: Agregamos el número de comuna en el centro
  geom_sf_text(aes(label = COMUNAS), size = 3, color = "black", fontface = "bold") +
  
  # Escala de colores:
  # Usamos 'viridis' opción 'magma' o 'plasma' que son excelentes para escalas de calor
  # direction = -1 invierte para que el color más oscuro/intenso sea el valor más alto
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Índice\nEnvejecimiento") +
  
  # Estética
  theme_void() + # Quita ejes y fondo gris, deja solo el mapa limpio
  labs(
    title = "Envejecimiento Poblacional en CABA",
    subtitle = "Adultos mayores (65+) por cada 100 jóvenes (0-14)",
    caption = "Fuente: Censo 2022 (INDEC) & BA Data"
  )


########################################################################################
#Radio Censal



# Asumimos que tu data frame se llama 'df_raw'

df_radios_final <- df_raw %>%
  # 1. IDENTIFICAR EL RADIO (Estrategia Fill Down)
  mutate(
    # Si la celda en '...2' dice "AREA #", capturamos ese texto. Si no, NA.
    radio_temp = if_else(str_detect(...2, "AREA #"), ...2, NA_character_)
  ) %>%
  
  # 2. Rellenar hacia abajo
  # Esto copia "AREA # 0200701" en todas las filas siguientes hasta encontrar otro radio
  fill(radio_temp, .direction = "down") %>%
  
  # 3. FILTRAR FILAS DE DATOS
  # Nos quedamos solo con las filas donde la columna '...2' empieza con un número
  # Esto elimina automáticamente los títulos "Edad en grupos...", "Mujer/Femenino", etc.
  filter(str_detect(...2, "^[0-9]")) %>%
  
  # 4. SELECCIONAR Y RENOMBRAR
  select(
    LINK_SUCIO = radio_temp,
    EDAD = ...2,
    MUJER = ...3,
    VARON = ...4
  ) %>%
  
  # 5. LIMPIEZA FINAL DE VALORES
  mutate(
    # A. Limpiar el ID del Radio: Extraemos solo los números del texto "AREA # 0200701"
    LINK = str_extract(LINK_SUCIO, "[0-9]+"),
    
    # B. Tratar los guiones "-" como ceros "0" antes de convertir
    MUJER = str_replace(MUJER, "-", "0"),
    VARON = str_replace(VARON, "-", "0"),
    
    # C. Convertir a numérico (ahora sí es seguro)
    MUJER = as.numeric(MUJER),
    VARON = as.numeric(VARON)
  ) %>%
  
  # Quitamos la columna sucia auxiliar
  select(LINK, EDAD, MUJER, VARON)%>%
  mutate(LINK = as.character(LINK))

# Verificamos los primeros resultados
head(df_radios_final)


###MAPA

# 2. Realizar la unión (Left Join)
# "Al mapa (izquierda), pegale los datos (derecha)"
mapa_completo <- carto_caba %>%
  left_join(df_radios_final, by = c("cod_indec" = "LINK"))

# 3. Verificación de seguridad
# Si ves muchas filas con EDAD = NA, es que los códigos no coincidieron.
print(paste("Fracciones sin datos unidos:", sum(is.na(mapa_completo$EDAD))))


# Primero, como un radio/fracción tiene muchas filas (una por grupo de edad),
# necesitamos "aplanar" los datos para tener 1 fila por Fracción antes de mapear.

datos_agrupados <- mapa_completo %>%
  st_drop_geometry() %>% # Trabajamos solo con la tabla para acelerar el cálculo
  group_by(cod_indec) %>%
  summarise(
    jovenes_0_14 = sum(MUJER[EDAD %in% c("00 a 04", "05 a 09", "10 a 14")] + 
                         VARON[EDAD %in% c("00 a 04", "05 a 09", "10 a 14")], na.rm = TRUE),
    
    mayores_65   = sum(MUJER[EDAD %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89", 
                                         "90 a 94", "95 a 99", "100 a 104", "105 y más")] + 
                         VARON[EDAD %in% c("65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89", 
                                           "90 a 94", "95 a 99", "100 a 104", "105 y más")], na.rm = TRUE)
  ) %>%
  mutate(
    indice_envejecimiento = if_else(jovenes_0_14 > 0, (mayores_65 / jovenes_0_14) * 100, 0)
  )

# Ahora volvemos a unir este resumen al mapa original de fracciones
mapa_indice <- carto_caba %>%
  left_join(datos_agrupados, by = "cod_indec")


ggplot(mapa_indice) +
  # Geometría coloreada por el índice
  geom_sf(aes(fill = indice_envejecimiento), color = NA) + 
  
  # Escala de color "Magma" (Oscuro = Más envejecido, Claro = Más joven)
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    name = "Índice Env.",
    na.value = "grey80" # Color para zonas sin datos (puerto, reserva)
  ) +
  
  theme_void() +
  labs(
    title = "Envejecimiento en CABA por Fracción Censal",
    subtitle = "Datos: Censo 2022 (INDEC)",
    caption = "Fuente: Elaboración propia"
  )

######################
#Version Leaflet
# 1. Definir la Paleta de Colores
# Usamos 'magma' igual que antes, pero adaptada a Leaflet
# domain = NULL hace que se ajuste automáticamente a tus valores mín/máx
paleta <- colorNumeric(
  palette = "magma", 
  domain = mapa_indice$indice_envejecimiento,
  reverse = TRUE # Invertimos para que oscuro = más envejecido (igual que ggplot)
)

# 2. Crear el contenido del Popup (HTML básico)
# Esto formatea lo que saldrá al hacer clic. 
# <br> es salto de línea, <b> es negrita.
mapa_indice$popup_info <- paste0(
  "<b>Fracción ID:</b> ", mapa_indice$cod_indec, "<br>",
  "<b>Índice Envejecimiento:</b> ", round(mapa_indice$indice_envejecimiento, 1), "<br>",
  "<hr>", # Una línea separadora
  "👴 <b>Mayores (65+):</b> ", mapa_indice$mayores_65, "<br>",
  "👶 <b>Jóvenes (0-14):</b> ", mapa_indice$jovenes_0_14
)

# 3. Generar el Mapa
p <- leaflet(mapa_indice) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # Fondo minimalista gris
  addPolygons(
    fillColor = ~paleta(indice_envejecimiento),
    fillOpacity = 0.7,
    color = "white",       # Color del borde
    weight = 1,            # Grosor del borde (fino queda elegante)
    opacity = 1,
    
    # Interacción: Resaltar al pasar el mouse
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # El Popup mágico
    popup = ~popup_info,
    label = ~paste("Índice:", round(indice_envejecimiento, 1)) # Etiqueta rápida al pasar mouse
  ) %>%
  addLegend(
    pal = paleta, 
    values = ~indice_envejecimiento, 
    opacity = 0.7, 
    title = "Índice de<br>Envejecimiento",
    position = "bottomright"
  )




library(htmlwidgets)
mi_mapa <- p # Asigna el código anterior a una variable
saveWidget(mi_mapa, file = "mapa_envejecimiento_caba.html")




