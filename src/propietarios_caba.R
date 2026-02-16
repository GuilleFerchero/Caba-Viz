rm(list = ls())

library(tidyverse)
library(readxl)

# 1. Cargar el Excel nuevo
df_tenencia_raw <- read_excel("censo/propiedad.xlsX", col_names = FALSE)

# 2. Limpieza
df_tenencia_clean <- df_tenencia_raw %>%
  # A. Identificar Fracción (Misma lógica: buscar "AREA #")
  mutate(
    fraccion_temp = if_else(str_detect(...1, "AREA #") | str_detect(...2, "AREA #"), ...2, NA_character_)
  ) %>%
  fill(fraccion_temp, .direction = "down") %>%
  
  # B. FILTRO CLAVE: Aquí cambiamos la estrategia
  # En lugar de buscar números, nos quedamos con las filas que mencionan las categorías
  # Ajusta "Propia" o "Alquilada" según cómo esté escrito exactamente en tu Excel
  filter(str_detect(...2, "Propia|Alquilada|Cedida por trabajo|Otra situación")) %>%
  
  # C. Seleccionar columnas
  # Asumo: Col 1 = Categoría (Dueño/Inquilino), Col 2 = Cantidad de Hogares
  select(
    LINK_SUCIO = fraccion_temp,
    TIPO_TENENCIA = ...2,
    CANTIDAD = ...3
  ) %>%
  
  # D. Limpieza final
  mutate(
    # Extraer ID limpio (los 7 dígitos de la fracción)
    LINK = str_extract(LINK_SUCIO, "[0-9]+"),
    
    # Limpiar números (sacar guiones si los hay)
    CANTIDAD = as.numeric(str_replace(CANTIDAD, "-", "0"))
  )

# Verificar qué categorías capturaste
table(df_tenencia_clean$TIPO_TENENCIA)




