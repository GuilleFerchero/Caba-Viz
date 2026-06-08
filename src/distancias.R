rm(list = ls())

##ENFOQUE SIN COLECTIVOS

# 1. Configurar los parámetros de Java ANTES de cargar nada en memoria
options(java.parameters = "-Xmx8g")
Sys.setenv(JAVA_HOME = "C:/Program Files/Microsoft/jdk-21.0.3.9-hotspot/")

# 2. Ahora sí, cargamos las librerías por primera vez en esta sesión
library(sf)
library(r5r)
library(tidyverse)

# 3. Definir la ruta a tus archivos de CABA
path_data <- "data/transporte/"

# 4. Lanzar el ruteador (Ahora sí tiene que arrancar)
r5r_core <- setup_r5(data_path = path_data, verbose = TRUE)

# Cargamos el archivo de fracciones (asegurate de poner tu ruta real)
fracciones <- st_read("data/censo/Codgeo_CABA_con_datos/cabaxrdatos.shp") %>%
  st_transform(4326) # R5 exige coordenadas geográficas WGS84

# r5r requiere un formato estricto: columnas 'id' (character), 'lon' y 'lat' (numeric)
origenes <- fracciones %>%
  st_centroid() %>%
  mutate(id = as.character(LINK),
         lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id, lon, lat) %>%
  filter(!is.na(lon) & !is.na(lat))

# 4. Preparar tus Destinos
destinos <- data.frame(
  id = c("Microcentro", "Retiro", "Estacion_Flores"),
  lon = c(-58.3816, -58.3747, -58.4635),
  lat = c(-34.6037, -34.5915, -34.6297)
)

# ==============================================================================
# 5. CÁLCULO DE MATRICES (Sin depender de fechas ni horarios)
# ==============================================================================

# A) TIEMPOS Y DISTANCIAS EN AUTO
print("Calculando Auto...")
ttm_auto <- travel_time_matrix(
  r5r_network = r5r_core, origins = origenes, destinations = destinos,
  mode = "CAR", max_trip_duration = 90, progress = TRUE
) %>% mutate(modo = "Auto")

# B) TIEMPOS Y DISTANCIAS EN BICICLETA
print("Calculando Bicicleta...")
ttm_bici <- travel_time_matrix(
  r5r_network = r5r_core, origins = origenes, destinations = destinos,
  mode = "BICYCLE", max_trip_duration = 90, progress = TRUE
) %>% mutate(modo = "Bicicleta")

# C) TIEMPOS Y DISTANCIAS CAMINANDO (Accesibilidad Peatonal Pura)
print("Calculando Caminata...")
ttm_caminata <- travel_time_matrix(
  r5r_network = r5r_core, origins = origenes, destinations = destinos,
  mode = "WALK", max_trip_duration = 120, progress = TRUE
) %>% mutate(modo = "Caminata")

# ==============================================================================
# 6. CONSOLIDACIÓN
# ==============================================================================
matriz_vial_final <- bind_rows(ttm_auto, ttm_bici, ttm_caminata) %>%
  rename(LINK = from_id, id_destino = to_id, tiempo_min = travel_time_p50)

# Guardar los resultados en la carpeta del proyecto
write_csv(matriz_vial_final, "data/transporte/matriz_tiempos_viales.csv")

# Cerrar el motor para liberar la RAM
stop_r5(r5r_core)
rJava::.jgc()

print("¡Matriz calculada con éxito basándose puramente en la red de calles!")

###################################################################################################################

library(sf)
library(tidyverse)
library(leaflet)

library(tidyverse)
library(sf)
library(leaflet)

library(tidyverse)

# 1. Cargamos tu matriz con las columnas invertidas
matriz_rota <- read_csv("data/transporte/matriz_tiempos_viales.csv")

# 2. Enderezamos las columnas usando los nombres que ya existen (LINK e id_destino)
matriz_corregida <- matriz_rota %>%
  mutate(
    # Si en la columna LINK hay letras (ej: Estacion_Flores), el código real está en id_destino
    LINK_real = if_else(str_detect(LINK, "^[A-Za-z]"), id_destino, LINK),
    destino_real = if_else(str_detect(LINK, "^[A-Za-z]"), LINK, id_destino)
  ) %>%
  # Limpiamos y dejamos la estructura definitiva
  select(
    LINK = LINK_real,
    id_destino = destino_real,
    tiempo_min,
    modo
  ) %>%
  mutate(LINK = as.character(LINK))

# 3. Guardamos el archivo corregido
write_csv(matriz_corregida, "data/transporte/matriz_tiempos_viales_CORREGIDA.csv")

print("¡Matriz enderezada con éxito!")

# ==============================================================================
# 1. CARGA Y CRUZE DE DATOS
# ==============================================================================

# 1.1. Cargamos la geometría de las fracciones de CABA
# (Asegurate de que la ruta coincida con tu archivo .shp o .geojson)
fracciones_caba <- st_read("data/censo/Codgeo_CABA_con_datos/cabaxrdatos.shp") %>%
  st_transform(4326) %>%
  mutate(LINK = as.character(LINK)) # Convertimos a texto puro

# 1.2. Cargamos la matriz de tiempos que guardaste en el paso anterior
matriz_tiempos <- read_csv("data/transporte/matriz_tiempos_viales_CORREGIDA.csv")%>%
  mutate(LINK = as.character(LINK)) # Aseguramos que se llame LINK y sea texto

# ==============================================================================
# 2. CRUCE GEOGRÁFICO MULTICAPA
# ==============================================================================
# Al hacer el join con ambas columnas formateadas como texto, NO se va a perder nada
mapa_datos <- fracciones_caba %>%
  left_join(matriz_tiempos, by = "LINK") %>%
  filter(!is.na(modo)) # Barremos celdas vacías de seguridad

# Verificación de control en la consola (Ahora tiene que decir: Auto, Bicicleta, Caminata)
print("Modos cruzados con éxito en el mapa:")
print(unique(mapa_datos$modo))


# Forzamos que LINK sea tratado como texto idéntico en ambos lados para evitar baches en el join
fracciones_caba <- fracciones_caba %>% mutate(LINK = as.character(LINK))
matriz_tiempos  <- matriz_tiempos %>% mutate(LINK = as.character(LINK))

mapa_datos <- fracciones_caba %>%
  left_join(matriz_tiempos, by = "LINK") %>%
  filter(!is.na(modo)) %>%
  filter(LINK != "020011314")

# 2. Extraer vectores limpios de lo que REALMENTE hay en la base
vect_modos    <- unique(mapa_datos$modo)
vect_destinos <- unique(mapa_datos$id_destino)

print("Modos detectados para el mapa:")
print(vect_modos)


# ==============================================================================
# 2. CONFIGURACIÓN DE LA PALETA DE COLORES Y TOOLTIPS
# ==============================================================================
# Definimos una paleta secuencial que vaya de amarillo (rápido) a rojo oscuro (lento)
# Acotamos el dominio entre 0 y 90 minutos para mantener un contraste realista en CABA
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, 90),
  na.color = "#808080" # Gris para zonas sin datos o que exceden el max_trip_duration
)

# ==============================================================================
# 3. CONSTRUCCIÓN DEL MAPA INTERACTIVO MULTICAPA
# ==============================================================================

# Inicializamos el mapa base
mapa_interactivo <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) # Fondo claro ideal para mapas temáticos

# Obtenemos las listas únicas de destinos y modos para iterar
vect_destinos <- unique(mapa_datos$id_destino)
vect_modos    <- unique(mapa_datos$modo)

# Usamos un doble bucle (Loop) para inyectar cada capa de polígonos al mapa
for (m in vect_modos) {
  for (d in vect_destinos) {

    # Filtrar el subset específico para esta combinación de capas
    capa_sub <- mapa_datos %>% filter(modo == m & id_destino == d)

    # Nombre de grupo único que aparecerá en el control flotante
    nombre_grupo <- paste0(m, " hacia ", d)

    # Agregamos los polígonos correspondientes a este grupo
    mapa_interactivo <- mapa_interactivo %>%
      addPolygons(
        data = capa_sub,
        fillColor = ~pal(tiempo_min),
        weight = 0.5,
        color = "#FFFFFF", # Líneas divisorias de fracciones en blanco fino
        fillOpacity = 0.75,
        group = nombre_grupo, # <--- CLAVE: Vincula el polígono a este grupo
        label = ~paste0(
          "Fracción (LINK): ", LINK, "<br>",
          "Modo: ", modo, "<br>",
          "Destino: ", id_destino, "<br>",
          "Tiempo estimado: ", round(tiempo_min, 1), " min"
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  }
}

# ==============================================================================
# 4. AGREGAR EL PANEL DE CONTROL INTERACTIVO (SELECTOR)
# ==============================================================================
# Generamos la lista de todos los nombres de grupos creados
todos_los_grupos <- outer(vect_modos, vect_destinos, function(m, d) paste0(m, " hacia ", d)) %>% as.vector()

mapa_interactivo <- mapa_interactivo %>%
  # Agregamos la leyenda fija abajo a la derecha
  addLegend(
    pal = pal,
    values = c(0, 90),
    title = "Tiempo de Viaje<br>(Minutos)",
    position = "bottomright"
  ) %>%
  # Agregamos el selector de capas (tipo botones de radio para elegir de a una combinación)
  addLayersControl(
    baseGroups = todos_los_grupos, # Usamos baseGroups para que solo se pueda activar UNO a la vez
    options = layersControlOptions(collapsed = FALSE), # Mantiene el panel desplegado y cómodo
    position = "topright"
  )

# ==============================================================================
# 5. MOSTRAR EL MAPA
# ==============================================================================
mapa_interactivo
