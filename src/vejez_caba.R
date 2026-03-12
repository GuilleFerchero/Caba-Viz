rm(list = ls())


library(tidyverse)
library(readxl)
library(sf)
library(leaflet)

# RUTA DEL ARCHIVO

archivo <- "data/censo/edad.xlsX"
archivo_radio <-"data/censo/edad_radio.xlsX"
carto <- st_read("data/censo/fracciones_censales/fracciones_censales.shp")
carto_caba <- carto %>%
  filter(cpr == "02") %>%
  mutate(cod_indec = as.character(cod_indec))

#  CARGA CRUDA
# saltamos las primeras filas que son títulos
df_raw <- read_excel(archivo, col_names = FALSE, skip =13)
df_raw <- df_raw %>%
  select(-...1)

# Definir dónde empiezan los datos
# Si el patrón es constante cada 28 filas y son 15 comunas:
filas_inicio <- seq(from = 5, by = 28, length.out = 15)

# Función para extraer cada comuna
extraer_comuna <- function(fila_start, indice_comuna) {

  # Definimos fila de fin (23 filas de datos: start + 22)
  fila_end <- fila_start + 22

  bloque <- df_raw %>%
    slice(fila_start:fila_end) %>%
    select(1:4)

  # Asignamos nombres
  colnames(bloque) <- c("EDAD", "MUJER", "HOMBRE", "TOTAL")

  bloque$COMUNA <- paste("Comuna", indice_comuna)
  bloque <- bloque %>% select(COMUNA, everything())

  return(bloque)
}

df_final <- map2_dfr(filas_inicio, 1:15, extraer_comuna)

# Verificación y Limpieza final de tipos
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
  select(-TOTAL) %>%
  pivot_longer(
    cols = c(MUJER, HOMBRE),
    names_to = "SEXO",
    values_to = "POBLACION"
  )


#PIRAMIDE


datos_piramide <- df_largo %>%
  mutate(

    POB_GRAFICO = ifelse(SEXO == "HOMBRE", -POBLACION, POBLACION),
    # Convertimos EDAD a "factor" usando el orden en que aparecen en el excel

    EDAD = factor(EDAD, levels = unique(EDAD))
  )


# Grafico
piramide <- ggplot(datos_piramide %>%
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
  facet_wrap(~ COMUNA, scales = "free_x")+
  theme(
    plot.background = element_rect(fill = "white", color = NA), # Fondo del lienzo
    panel.background = element_rect(fill = "white", color = NA) # Fondo del área del gráfico
  )


ggsave("output/piramide.png",piramide)


#############################################################
#Envejecimiento

df_indicadores <- df_final %>%
  filter(!is.na(TOTAL)) %>%
  filter(EDAD !="Total") %>%
  mutate(grupo_demografico = case_when(
    EDAD %in% c("00 a 04", "05 a 09", "10 a 14") ~ "jovenes_0_14",
    EDAD %in% c("15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39",
                "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64") ~ "activos_15_64",
    TRUE ~ "mayores_65_mas"
  )) %>%

  group_by(COMUNA, grupo_demografico) %>%
  summarise(poblacion = sum(TOTAL), .groups = "drop") %>%

  pivot_wider(names_from = grupo_demografico, values_from = poblacion) %>%
  mutate(
    indice_envejecimiento = (mayores_65_mas / jovenes_0_14) * 100,
    indice_dependencia = ((jovenes_0_14 + mayores_65_mas) / activos_15_64) * 100
  ) %>%
  arrange(desc(indice_envejecimiento)) # Ordena del más envejecido al más joven


#Visualizar los Resultados

indice <-  ggplot(df_indicadores, aes(x = reorder(COMUNA, indice_envejecimiento), y = indice_envejecimiento)) +
  geom_col(fill = "#4e79a7") +
  geom_text(aes(label = round(indice_envejecimiento, 1)),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Índice de Envejecimiento por Comuna (Censo 2022)",
    subtitle = "Número de personas de 65+ años por cada 100 menores de 15 años",
    x = "",
    y = "Índice de Envejecimiento",
    caption = "Fuente: Elaboración propia en base a INDEC"
  ) +
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) # Limpiar líneas de fondo

ggsave("output/indice.png",indice)


########################################################################################
#Mapa

# URL oficial del Portal de Datos Abiertos de CABA
url_comunas <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.csv"

# Leemos el mapa
mapa_caba <- st_read(url_comunas)

df_mapa_data <- df_indicadores %>%
  mutate(
    # Se extrae solo el número de la columna "Comuna 1", "Comuna 2"...
    COMUNAS = readr::parse_number(COMUNA)
  )

mapa_caba <- mapa_caba %>%
  mutate(COMUNAS = as.numeric(comuna))

mapa_final <- mapa_caba %>%
  left_join(df_mapa_data, by = "COMUNAS")%>%
  st_as_sf(wkt = "geometry", crs = 4326)

mapa_est <- ggplot(mapa_final) +
  geom_sf(aes(fill = indice_envejecimiento), color = "white", lwd = 0.2) +
  geom_sf_text(aes(label = COMUNAS), size = 3, color = "black", fontface = "bold") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Índice\nEnvejecimiento") +
  theme_void() +
  labs(
    title = "Envejecimiento Poblacional en CABA",
    subtitle = "Adultos mayores (65+) por cada 100 jóvenes (0-14)",
    caption = "Fuente: Censo 2022 (INDEC) & BA Data"
  )+
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) # Limpiar líneas de fondo


ggsave("output/mapa_est.png",mapa_est)

########################################################################################
#Radio Censal



df_raw <- read_excel(archivo_radio, col_names = FALSE, skip =13)

df_radios_final <- df_raw %>%
  mutate(
    radio_temp = if_else(str_detect(...2, "AREA #"), ...2, NA_character_)
  ) %>%
  fill(radio_temp, .direction = "down") %>%
  filter(str_detect(...2, "^[0-9]")) %>%
  select(
    LINK_SUCIO = radio_temp,
    EDAD = ...2,
    MUJER = ...3,
    VARON = ...4
  ) %>%
  mutate(
    LINK = str_extract(LINK_SUCIO, "[0-9]+"),
    MUJER = str_replace(MUJER, "-", "0"),
    VARON = str_replace(VARON, "-", "0"),
    MUJER = as.numeric(MUJER),
    VARON = as.numeric(VARON)
  ) %>%
  select(LINK, EDAD, MUJER, VARON)%>%
  mutate(LINK = as.character(LINK))

###MAPA

mapa_completo <- carto_caba %>%
  left_join(df_radios_final, by = c("cod_indec" = "LINK"))
print(paste("Fracciones sin datos unidos:", sum(is.na(mapa_completo$EDAD))))


datos_agrupados <- mapa_completo %>%
  st_drop_geometry() %>%
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
mapa_indice <- carto_caba %>%
  left_join(datos_agrupados, by = "cod_indec")


mapa_frac <- ggplot(mapa_indice) +
  geom_sf(aes(fill = indice_envejecimiento), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    name = "Índice Env.",
    na.value = "grey80"
  ) +

  theme_void() +
  labs(
    title = "Envejecimiento en CABA por Fracción Censal",
    subtitle = "Datos: Censo 2022 (INDEC)",
    caption = "Fuente: Elaboración propia"
  )+
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) # Limpiar líneas de fondo


ggsave("output/mapa_frac.png",mapa_frac)





######################
#Version Leaflet
# 1. Definir la Paleta de Colores
# Usamos 'magma' igual que antes, pero adaptada a Leaflet
paleta <- colorNumeric(
  palette = "magma",
  domain = mapa_indice$indice_envejecimiento,
  reverse = TRUE # Invertimos para que oscuro = más envejecido (igual que ggplot)
)

# 2. Crear el contenido del Popup (HTML básico)
mapa_indice$popup_info <- paste0(
  "<b>Fracción ID:</b> ", mapa_indice$cod_indec, "<br>",
  "<b>Índice Envejecimiento:</b> ", round(mapa_indice$indice_envejecimiento, 1), "<br>",
  "<hr>", # línea separadora
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
    weight = 1,            # Grosor del borde

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
saveWidget(mi_mapa, file = "index.html", selfcontained = TRUE)




