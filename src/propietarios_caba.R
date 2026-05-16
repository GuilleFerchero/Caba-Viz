rm(list = ls())

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)



archivo_radio <-"data/censo/edad_radio.xlsX"
carto <- st_read("data/censo/fracciones_censales/fracciones_censales.shp")
fracciones_caba <- carto %>%
  filter(cpr == "02") %>%
  mutate(cod_indec = as.character(cod_indec))

df_tenencia_raw <- read_excel("data/censo/propiedad.xlsX", col_names = FALSE)
df_tenencia_clean <- df_tenencia_raw %>%
  mutate(
    fraccion_temp = if_else(str_detect(...1, "AREA #") | str_detect(...2, "AREA #"), ...2, NA_character_)
  ) %>%
  fill(fraccion_temp, .direction = "down") %>%
  filter(str_detect(...2, "Propia|Alquilada|Cedida por trabajo|Prestada|Otra situación")) %>%
  select(
    LINK_SUCIO = fraccion_temp,
    TIPO_TENENCIA = ...2,
    CANTIDAD = ...3
  ) %>%


  mutate(
    LINK = str_extract(LINK_SUCIO, "[0-9]+"),
    CANTIDAD = as.numeric(str_replace(CANTIDAD, "-", "0"))
  )

table(df_tenencia_clean$TIPO_TENENCIA)


df_tenencia_pivot <- df_tenencia_clean %>%
  mutate(categoria_simple = case_when(
    str_detect(TIPO_TENENCIA, "Propia") ~ "PROPIETARIOS",
    str_detect(TIPO_TENENCIA, "Alquilada") ~ "INQUILINOS",
    TRUE ~ "OTROS" # Cedida, prestada, etc.
  )) %>%

  group_by(LINK, categoria_simple) %>%
  summarise(hogares = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = categoria_simple, values_from = hogares, values_fill = 0) %>%
  mutate(
    total_hogares = PROPIETARIOS + INQUILINOS + OTROS,
    porc_inquilinos = (INQUILINOS / total_hogares) * 100
  )

head(df_tenencia_pivot)


#Mapa


fracciones_caba <- fracciones_caba %>%
  mutate(cod_indec = as.character(cod_indec))

mapa_inquilinos <- fracciones_caba %>%
  left_join(df_tenencia_pivot, by = c("cod_indec" = "LINK")) %>%
  st_as_sf(wkt = "geometry", crs = 4326)
pal_alquiler <- colorNumeric("YlOrRd", domain = mapa_inquilinos$porc_inquilinos)

leaflet(mapa_inquilinos) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_alquiler(porc_inquilinos),
    fillOpacity = 0.8,
    color = "white", weight = 1, opacity = 1,

    # Popup interactivo
    popup = ~paste0(
      "<b>Fracción ID:</b> ", cod_indec, "<br>",
      "<b>% Inquilinos:</b> ", round(porc_inquilinos, 1), "%<br>",
      "<hr>",
      "🏠 Propietarios: ", PROPIETARIOS, "<br>",
      "🔑 Inquilinos: ", INQUILINOS
    ),

    highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
  ) %>%
  addLegend(
    pal = pal_alquiler,
    values = ~porc_inquilinos,
    title = "% Inquilinos",
    position = "bottomright"
  )




##Grafico y nuevo mapa


library(tidyverse)

# 1. Agregamos los datos, calculamos la proporción y reordenamos
df_tenencia_comuna <- mapa_inquilinos %>%
  st_drop_geometry() %>% # Quitamos la geometría para cálculos rápidos
  group_by(dpto) %>%
  summarise(
    Propietarios = sum(PROPIETARIOS, na.rm = TRUE),
    Inquilinos = sum(INQUILINOS, na.rm = TRUE),
    Otros = sum(OTROS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # A. Calculamos la proporción de propietarios sobre el total
  mutate(
    total_hogares = Propietarios + Inquilinos + Otros,
    prop_propietarios = Propietarios / total_hogares,

    # B. Reordenamos 'dpto' según esa proporción.
    # Al estar en orden ascendente por defecto, coord_flip() pondrá el mayor arriba.
    dpto = fct_reorder(dpto, prop_propietarios)
  ) %>%
  # C. Descartamos las columnas auxiliares que ya no necesitamos
  select(-total_hogares, -prop_propietarios) %>%
  # D. Pasamos a formato largo para ggplot
  pivot_longer(cols = c(Propietarios, Inquilinos, Otros),
               names_to = "Condicion", values_to = "Hogares") %>%
  # E. (Opcional pero recomendado) Fijamos el orden interno de las barras
  mutate(Condicion = factor(Condicion, levels = c("Propietarios", "Inquilinos", "Otros")))

# 2. Gráfico de barras apiladas al 100% (Proporción)
ggplot(df_tenencia_comuna, aes(x = dpto, y = Hogares, fill = Condicion)) +
  # geom_col() es la versión simplificada y moderna de geom_bar(stat = "identity")
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Inquilinos" = "#e34a33",
                               "Propietarios" = "#2b8cbe",
                               "Otros" = "#bdbdbd")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Régimen de Tenencia por Comuna",
       subtitle = "Proporción de hogares propietarios vs inquilinos",
       x = "Comuna", y = "Porcentaje", fill = "Condición")

######VER




mapa_comunas_tenencia <- df_tenencia_comuna %>%
  pivot_wider(names_from = Condicion, values_from = Hogares) %>%
  mutate(porc_inquilinos = (Inquilinos / (Inquilinos + Propietarios + Otros)) * 100) %>%
  # Es más seguro poner el mapa a la izquierda en el join para no perder la geometría
  right_join(fracciones_caba, by = "dpto") %>%
  st_as_sf()

pal_comuna <- colorNumeric("YlOrRd", domain = mapa_comunas_tenencia$porc_inquilinos)
pal_fraccion <- colorNumeric("YlOrRd", domain = mapa_inquilinos$porc_inquilinos)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # CAPA 1: NIVEL COMUNAL
  addPolygons(
    data = mapa_comunas_tenencia,
    group = "Ver por Comuna",
    fillColor = ~pal_comuna(porc_inquilinos),
    fillOpacity = 0.7, color = "white", weight = 2,
    popup = ~paste0("<b>", dpto, "</b><br>",
                    "<b>% Inquilinos: </b>", round(porc_inquilinos, 1), "%")
  ) %>%

  # CAPA 2: NIVEL FRACCIÓN (Detalle)
  addPolygons(
    data = mapa_inquilinos,
    group = "Ver por Fracción (Detalle)",
    fillColor = ~pal_fraccion(porc_inquilinos),
    fillOpacity = 0.8, color = "white", weight = 0.5,
    popup = ~paste0("<b>Fracción: </b>", cod_indec, "<br>",
                    "<b>% Inquilinos: </b>", round(porc_inquilinos, 1), "%<br>",
                    "<hr>",
                    "🏠 Propietarios: ", PROPIETARIOS, "<br>",
                    "🔑 Inquilinos: ", INQUILINOS)
  ) %>%

  # CONTROL DE CAPAS
  addLayersControl(
    baseGroups = c("Ver por Comuna", "Ver por Fracción (Detalle)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # LEYENDA
  addLegend(pal = pal_fraccion, values = mapa_inquilinos$porc_inquilinos,
            title = "% Inquilinos", position = "bottomright")
