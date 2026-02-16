
#funcion para mensajes

log_msg <- function(msg) {
  cat(sprintf("[LOG] %s\n", msg))
  flush.console()
}
########################################################


#funcion para trabajar zips mediante descarga

download_and_extract_zip <- function(url) {
  log_msg(paste("Descargando ZIP desde:", url))
  
  # Crear un archivo temporal
  temp_file <- tempfile(fileext = ".zip")
  
  # Descargar el archivo
  res <- GET(url, write_disk(temp_file, overwrite = TRUE), timeout = 120)
  stop_for_status(res, paste("descargar", url))
  
  # Crear un directorio temporal para la extracción
  tmp_dir <- tempfile(pattern = "eph_")
  dir.create(tmp_dir, recursive = TRUE)
  
  # Descomprimir el ZIP
  zip::unzip(zipfile = temp_file, exdir = tmp_dir)
  
  log_msg(paste("ZIP extraído en carpeta temporal:", tmp_dir))
  
  # Eliminar el archivo temporal .zip
  unlink(temp_file) 
  
  return(tmp_dir)
}
########################################################

#funcion para trabajar zips mediante descarga

# Define la ruta a la carpeta donde están los archivos .zip
ruta_carpeta_zip <- "data/eph_zip/" 

# Define la ruta donde quieres guardar los archivos descomprimidos
# Es buena práctica descomprimir en una carpeta diferente a la original.
ruta_carpeta_destino <- "data/eph_unzip/"  

# Si la carpeta destino no existe, la creamos (opcional pero recomendado)
if (!dir.exists(ruta_carpeta_destino)) {
  dir.create(ruta_carpeta_destino)
}


descomprimir_INDEC <- function(directorio_origen, directorio_destino) {
  
# 1. Listar todos los archivos .zip en el directorio de origen
# La función 'list.files' busca archivos con la extensión ".zip"
# 'full.names = TRUE' asegura que obtenemos la ruta completa de cada archivo


# directorio_origen = ruta_carpeta_zip
# directorio_destino = ruta_carpeta_destino



archivos_zip <- list.files(
  path = directorio_origen, 
  pattern = "\\.zip$", # El patrón busca archivos que terminen en .zip
  full.names = TRUE, 
  ignore.case = TRUE # Ignora mayúsculas/minúsculas, por si hay ".ZIP"
)

# 2. Verificar si se encontraron archivos .zip
if (length(archivos_zip) == 0) {
  message("⚠️ No se encontraron archivos .zip en el directorio especificado.")
  return(invisible(NULL)) # Sale de la función si no hay archivos
}

message(paste("✅ Se encontraron", length(archivos_zip), "archivos .zip para descomprimir."))

# 3. Iterar sobre cada archivo .zip y descomprimirlo
# Usamos 'lapply' para aplicar la función 'unzip' a cada elemento de 'archivos_zip'
resultados_descompresion <- lapply(archivos_zip, function(archivo) {
  
  # 'unzip' es la función clave de la librería 'utils'
  # 'exdir' especifica el directorio de extracción (destino)
  tryCatch({
    utils::unzip(zipfile = archivo, exdir = directorio_destino)
    return(paste("🎉 Descomprimido con éxito:", basename(archivo)))
  }, error = function(e) {
    return(paste("❌ Error al descomprimir", basename(archivo), ":", e$message))
  })
  
})

# 4. Mostrar un resumen del resultado
message("\n--- Resumen de la Descompresión ---")
print(unlist(resultados_descompresion))
message("\n✨ Proceso de descompresión completado.")

# La función devuelve un valor invisible, el efecto es la descompresión.
return(invisible(NULL))
}

###########################################################################

read_eph_from_dir <- function(root_dir) {
  log_msg(paste("Buscando archivos .xls/.xlsx en:", root_dir))
  excel_files <- list.files(
    path = root_dir,
    pattern = "\\.xls$|\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  log_msg(paste("Encontrados", length(excel_files), "archivos Excel"))
  
  if (length(excel_files) < 2) {
    stop(paste("Esperaba al menos 2 .xls/.xlsx en", root_dir, ", encontré", length(excel_files)))
  }
  
  # Identificar hogar / individual
  filenames_lower <- basename(excel_files) %>% tolower()
  
  hogar_candidates <- excel_files[str_detect(filenames_lower, "hogar")]
  indiv_candidates <- excel_files[str_detect(filenames_lower, "individual")]
  
  if (length(hogar_candidates) == 1 && length(indiv_candidates) == 1) {
    hogar_file <- hogar_candidates[1]
    indiv_file <- indiv_candidates[1]
    log_msg(paste("Identificado archivo hogar:", basename(hogar_file)))
    log_msg(paste("Identificado archivo individual:", basename(indiv_file)))
  } else {
    excel_files_sorted <- sort(excel_files)
    indiv_file <- excel_files_sorted[1]
    hogar_file <- excel_files_sorted[2]
    log_msg("No pude identificar hogar/individual por nombre, uso primeros dos ordenados:")
    log_msg(paste("  individual (supuesto):", basename(indiv_file)))
    log_msg(paste("  hogar (supuesto):", basename(hogar_file)))
  }
  
  # Leer los DataFrames
  df_hogar <- read_excel(hogar_file, sheet = 1, col_types = "text", na = c(".", "NA"))
  df_ind <- read_excel(indiv_file, sheet = 1, col_types = "text", na = c(".", "NA"))
  
  log_msg(sprintf("Leído hogar: %d filas x %d columnas", nrow(df_hogar), ncol(df_hogar)))
  log_msg(sprintf("Leído individual: %d filas x %d columnas", nrow(df_ind), ncol(df_ind)))
  
  # Nombres en minúsculas y limpios (usa janitor::clean_names)
  df_hogar <- df_hogar %>% clean_names()
  df_ind <- df_ind %>% clean_names()
  
  # Merge de DataFrames (claves como texto ya están aseguradas)
  merge_keys <- c("codusu", "trimestre", "nro_hogar") %>%
    keep(~ . %in% names(df_hogar) & . %in% names(df_ind))
  
  log_msg(paste("Claves de merge encontradas:", paste(merge_keys, collapse = ", ")))
  
  if (length(merge_keys) == 0) {
    stop("No encontré llaves comunes para hacer el merge hogar/individual.")
  }
  
  df <- left_join(df_ind, df_hogar, by = merge_keys, suffix = c("", "_hog"))
  log_msg(sprintf("Resultado del merge: %d filas x %d columnas", nrow(df), ncol(df)))
  
  # Seleccionar y limpiar variables
  desired_vars <- c(
    "codusu", "ano4", "trimestre", "nro_hogar",
    "componente", "ipcf", "itf", "pondera", "pondih",
    "region", "aglomerado", "ch03", "ch04", "ch06"
  )
  
  # Copiar variables de _hog si no existen en la individual
  for (col in desired_vars) {
    if (!(col %in% names(df)) && paste0(col, "_hog") %in% names(df)) {
      df[[col]] <- df[[paste0(col, "_hog")]]
    }
  }
  
  df <- df %>% select(intersect(desired_vars, names(.)))
  log_msg(paste("Variables disponibles después del merge:", paste(names(df), collapse = ", ")))
  
  # Convertir a numérico (como 'destring' en Stata)
  num_cols <- names(df) %>% keep(~ . != "codusu")
  for (col in num_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  # Detectar y estandarizar año / trimestre dominantes
  year <- NULL
  trim <- NULL
  
  if ("ano4" %in% names(df)) {
    year_mode <- df$ano4 %>% na.omit() %>% table() %>% sort(decreasing = TRUE) %>% head(1) %>% names() %>% as.integer()
    if (length(year_mode) > 0) {
      year <- year_mode
      df$ano4[is.na(df$ano4)] <- year
    }
  }
  
  if ("trimestre" %in% names(df)) {
    trim_mode <- df$trimestre %>% na.omit() %>% table() %>% sort(decreasing = TRUE) %>% head(1) %>% names() %>% as.integer()
    if (length(trim_mode) > 0) {
      trim <- trim_mode
      df$trimestre[is.na(df$trimestre)] <- trim
    }
  }
  
  log_msg(sprintf("Año y trimestre detectados en este archivo: (ano4=%s, trimestre=%s)", 
                  ifelse(is.null(year), "NA", year), 
                  ifelse(is.null(trim), "NA", trim)))
  
  return(list(df = df, key = list(year, trim)))
}


load_eph_trim <- function(url) {
  tmp_dir <- download_and_extract_zip(url)
  
  result <- tryCatch({
    read_eph_from_dir(tmp_dir)
  }, finally = {
    # Borrar la carpeta temporal
    unlink(tmp_dir, recursive = TRUE)
    log_msg(paste("Carpeta temporal eliminada:", tmp_dir))
  })
  
  return(result)
}

read_canastas_excel <- function(url, preferred_sheets = NULL) {
  if (is.null(preferred_sheets)) {
    preferred_sheets <- c("Series canastas anexo", "CBA y CBT")
  }
  
  log_msg(paste("Descargando canastas desde:", url))
  
  # Descargar el archivo Excel
  temp_file <- tempfile(fileext = ".xls")
  res <- GET(url, write_disk(temp_file, overwrite = TRUE), timeout = 60)
  stop_for_status(res, paste("descargar", url))
  
  # Obtener los nombres de las hojas
  sheet_names <- excel_sheets(temp_file)
  log_msg(paste("Hojas disponibles en el archivo:", paste(sheet_names, collapse = ", ")))
  
  sheet_to_use <- NULL
  for (s in preferred_sheets) {
    if (s %in% sheet_names) {
      sheet_to_use <- s
      break
    }
  }
  
  if (is.null(sheet_to_use)) {
    sheet_to_use <- sheet_names[1]
    log_msg(paste("Ninguna hoja preferida encontrada, uso la primera:", sheet_to_use))
  } else {
    log_msg(paste("Usando hoja:", sheet_to_use))
  }
  
  # Leer la hoja como texto (dtype=str en Python)
  df_raw <- read_excel(temp_file, sheet = sheet_to_use, col_types = "text", na = c(".", "NA")) %>%
    clean_names()
  
  # Eliminar el archivo temporal
  unlink(temp_file)
  
  return(df_raw)
}


# Microdatos EPH 2016-2 A 2025-2
build_microdata <- function() {
  log_msg("===== INICIO build_microdata() =====")
  frames <- list()
  
  # 2016-2 a 2017-1 
  trims <- c("2doTrim_2016", "3erTrim_2016", "4toTrim_2016", "1er_Trim_2017")
  for (trim_name in trims) {
    url <- paste0(BASE_EPH_URL, "/EPH_usu_", trim_name, "_xls.zip")
    log_msg(paste("Descargando bloque:", trim_name))
    
    result <- load_eph_trim(url)
    df <- result$df
    key <- result$key
    log_msg(sprintf("  -> detectado en datos: (ano4=%s, trimestre=%s), filas=%d", 
                    key[[1]], key[[2]], nrow(df)))
    frames[[length(frames) + 1]] <- df
  }
  
  # Vamos a generar la lista de trimestres año-trimestre a descargar:
  anos <- 2017:2025
  trimestres_a_descargar <- list()
  
  for (ano in anos) {
    for (trim in 1:4) {
      is_excluded <- 
        (ano == 2017 && trim == 1) | 
        (ano == 2021 && trim != 2) | 
        (ano %in% c(2022, 2023)) |   
        (ano == 2024 && trim != 4) | 
        (ano == 2025 && trim != 2)   
      
      if (ano >= 2017 && ano <= 2020) {
        if (trim >= 2 || ano > 2017) {
          is_excluded <- FALSE 
        } else {
          is_excluded <- TRUE
        }
      }
      
      if (ano %in% c(2021, 2024, 2025) && (trim == 2 || trim == 4)) {
        if ((ano == 2021 && trim == 2) || (ano == 2024 && trim == 4) || (ano == 2025 && trim == 2)) {
          is_excluded <- FALSE
        } else {
          is_excluded <- TRUE
        }
      }
      
      # Generar el URL y descargar
      if (!is_excluded) {
        url <- paste0(BASE_EPH_URL, "/EPH_usu_", trim, "_Trim_", ano, "_xls.zip")
        log_msg(sprintf("Descargando bloque: año %d, trim %d", ano, trim))
        
        result <- load_eph_trim(url)
        df <- result$df
        key <- result$key
        log_msg(sprintf("  -> detectado en datos: (ano4=%s, trimestre=%s), filas=%d", 
                        key[[1]], key[[2]], nrow(df)))
        frames[[length(frames) + 1]] <- df
      } else {
        log_msg(sprintf("Saltando año %d, trim %d (excluido)", ano, trim))
      }
    }
  }
  
  # 2021-1 a 2025-1
  
  for (year in 2021:2025) {
    for (t in 1:4) {
      if ((year == 2021 && t == 2) || 
          (year == 2024 && t == 4) || 
          (year == 2025 && t > 1) ||
          (year %in% 2017:2020) 
      ) {
        log_msg(sprintf("Saltando año %d, trim %d (ya tratado o excluido)", year, t))
        next
      }
      
      url <- paste0(BASE_EPH_URL, "/EPH_usu_", t, "_Trim_", year, "_xls.zip")
      log_msg(sprintf("Descargando bloque: año %d, trim %d", year, t))
      
      result <- load_eph_trim(url)
      df <- result$df
      key <- result$key
      log_msg(sprintf("  -> detectado en datos: (ano4=%s, trimestre=%s), filas=%d", 
                      key[[1]], key[[2]], nrow(df)))
      frames[[length(frames) + 1]] <- df
    }
  }
  
  # Concatenar todos los trimestres
  micro <- bind_rows(frames)
  log_msg(sprintf("Microdatos concatenados: %d filas x %d columnas", nrow(micro), ncol(micro)))
  
  # Asegurar tipos numéricos básicos
  num_cols_to_check <- c(
    "ano4", "trimestre", "nro_hogar", "componente", "ipcf", "itf",
    "pondera", "pondih", "region", "aglomerado", "ch03", "ch04", "ch06"
  )
  
  for (col in num_cols_to_check) {
    if (col %in% names(micro)) {
      micro[[col]] <- suppressWarnings(as.numeric(micro[[col]]))
    }
  }
  
  log_msg("Distribución de (ano4, trimestre) en microdatos:")
  print(micro %>% 
          select(ano4, trimestre) %>% 
          na.omit() %>% 
          distinct() %>% 
          arrange(ano4, trimestre) %>% 
          head(20))
  
  log_msg("===== FIN build_microdata() =====")
  return(micro)
}

# Canastas (CBA / CBT)
parse_canastas_sheet <- function(df_raw, ano, trimestre1, trimestre2) {
  log_msg(sprintf("Parseando canastas para año %d, trimestres %d y %d", ano, trimestre1, trimestre2))
  df <- df_raw
  
  # Identificar la columna de "Region"
  region_col_name <- names(df)[1]
  df <- df %>% 
    rename(Region = !!region_col_name) %>%
    mutate(Region = as.character(Region))
  
  # Sacar filas vacías o encabezados
  df <- df %>%
    filter(
      !is.na(Region) & 
        str_trim(Region) != "" & 
        tolower(str_trim(Region)) != "región"
    )
  
  # Codificar regiones
  region_map <- c(
    "Gran Buenos Aires" = 1,
    "Pampeana" = 2,
    "Cuyo" = 3,
    "Noroeste" = 4,
    "Noreste" = 6
  )
  df <- df %>%
    mutate(
      region = recode(Region, !!!region_map),
      region = replace_na(region, 5) %>% as.integer()
    )
  
  # Columnas numéricas (excluimos Region y region)
  value_cols <- names(df) %>% keep(~ !(. %in% c("Region", "region")))
  
  # Convertir a numérico
  for (col in value_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  # Equivalente a "drop if B<3 | B==." (tomamos la primera columna de valores)
  first_col <- value_cols[1]
  df <- df %>% filter(!!sym(first_col) >= 3 & !is.na(!!sym(first_col)))
  
  # Tomamos sólo las 6 primeras columnas numéricas
  value_cols <- head(value_cols, 6)
  BCD_cols <- head(value_cols, 3) # Trimestre 1
  EFG_cols <- tail(value_cols, 3) # Trimestre 2
  
  # Extrema: filas 1-6
  extrema_df <- df %>% slice(1:6)
  
  # Cálculo para trimestre 1
  extrema1 <- extrema_df %>%
    mutate(lp_extrema = rowMeans(select(., all_of(BCD_cols)))) %>%
    select(region, lp_extrema) %>%
    mutate(ano = ano, trimestre = trimestre1)
  
  # Cálculo para trimestre 2
  extrema2 <- extrema_df %>%
    mutate(lp_extrema = rowMeans(select(., all_of(EFG_cols)))) %>%
    select(region, lp_extrema) %>%
    mutate(ano = ano, trimestre = trimestre2)
  
  extrema <- bind_rows(extrema1, extrema2)
  
  # Moderada: filas 7-12
  moderada_df <- df %>% slice(7:12)
  
  # Cálculo para trimestre 1
  moderada1 <- moderada_df %>%
    mutate(lp_moderada = rowMeans(select(., all_of(BCD_cols)))) %>%
    select(region, lp_moderada) %>%
    mutate(ano = ano, trimestre = trimestre1)
  
  # Cálculo para trimestre 2
  moderada2 <- moderada_df %>%
    mutate(lp_moderada = rowMeans(select(., all_of(EFG_cols)))) %>%
    select(region, lp_moderada) %>%
    mutate(ano = ano, trimestre = trimestre2)
  
  moderada <- bind_rows(moderada1, moderada2)
  
  # Merge de extrema y moderada
  canastas <- inner_join(
    moderada, extrema, 
    by = c("ano", "trimestre", "region")
  ) %>%
    select(ano, trimestre, region, lp_extrema, lp_moderada)
  
  log_msg(sprintf("Canastas generadas (año=%d): %d filas", ano, nrow(canastas)))
  return(canastas)
}

build_canastas <- function() {
  log_msg("===== INICIO build_canastas() =====")
  frames <- list()
  
  # Semestre 1 
  for (t in 17:25) {
    fname <- sprintf("cuadros_informe_pobreza_09_%d.xls", t)
    url <- paste0(BASE_CANASTAS_URL, "/", fname)
    
    preferred <- if (t == 17) c("CBA y CBT", "Series canastas anexo") else c("Series canastas anexo", "CBA y CBT")
    
    log_msg(sprintf("Descargando canastas semestre 1, archivo %s", fname))
    df_raw <- read_canastas_excel(url, preferred_sheets = preferred)
    
    ano <- 2000 + t
    can_t <- parse_canastas_sheet(df_raw, ano, trimestre1 = 1, trimestre2 = 2)
    frames[[length(frames) + 1]] <- can_t
  }
  
  semestre1 <- bind_rows(frames)
  log_msg(sprintf("Semestre 1 canastas: %d filas", nrow(semestre1)))
  
  # Semestre 2 
  frames <- list()
  for (t in 18:25) { # OJO: acá arrancamos en 18
    if (t == 20) {
      fname <- sprintf("cuadros_informe_pobreza_04_%d.xls", t)
    } else {
      fname <- sprintf("cuadros_informe_pobreza_03_%d.xls", t)
    }
    
    url <- paste0(BASE_CANASTAS_URL, "/", fname)
    log_msg(sprintf("Descargando canastas semestre 2, archivo %s", fname))
    
    preferred <- c("Series canastas anexo", "CBA y CBT")
    df_raw <- read_canastas_excel(url, preferred_sheets = preferred)
    
    ano <- 2000 + t - 1 
    can_t <- parse_canastas_sheet(df_raw, ano, trimestre1 = 3, trimestre2 = 4)
    frames[[length(frames) + 1]] <- can_t
  }
  
  semestre2 <- bind_rows(frames)
  log_msg(sprintf("Semestre 2 canastas: %d filas", nrow(semestre2)))
  
  # Combinar ambos semestres
  canastas <- bind_rows(semestre1, semestre2) %>%
    arrange(ano, trimestre, region) %>%
    distinct() # chequeo
  
  log_msg(sprintf("Canastas totales: %d filas x %d columnas", nrow(canastas), ncol(canastas)))
  log_msg("Ejemplo de combinaciones (ano, trimestre, region) en canastas:")
  print(canastas %>% 
          select(ano, trimestre, region) %>% 
          distinct() %>% 
          head(20))
  
  log_msg("===== FIN build_canastas() =====")
  return(canastas)
}

# Ingresos, AE, Pobreza e Indigencia
compute_ingresos_equivalentes <- function(micro) {
  log_msg("Calculando ingresos equivalentes y demás variables (compute_ingresos_equivalentes)")
  df <- micro %>% rename(ano = ano4) # renombrar! ano4 -> ano
  
  # Semestre y Periodo
  df <- df %>%
    mutate(
      semestre = ifelse(trimestre %in% c(1, 2), 1, 2),
      period = paste0(as.integer(ano), "-", as.integer(semestre))
    )
  
  # id = group(codusu nro_hogar trimestre ano)
  key_cols <- c("codusu", "nro_hogar", "trimestre", "ano") %>% keep(~ . %in% names(df))
  df <- df %>%
    group_by(across(all_of(key_cols))) %>%
    mutate(id = cur_group_id()) %>%
    ungroup()
  
  # Recode región 
  if ("region" %in% names(df)) {
    df <- df %>%
      mutate(
        region = case_when(
          region == 43 ~ 2,
          region == 42 ~ 3,
          region == 40 ~ 4,
          region == 44 ~ 5,
          region == 41 ~ 6,
          TRUE ~ region 
        )
      )
  }
  
  df <- df %>% rename(edad = ch06) # renombrar! ch06 -> edad
  
  # hombre (1 varón, 0 mujer)
  if ("ch04" %in% names(df)) {
    df <- df %>%
      mutate(
        hombre = case_when(
          ch04 == 2 ~ 0, # Mujer
          ch04 == 1 ~ 1, # Varón
          TRUE ~ NA_real_
        )
      )
  }
  
  # Escala de adulto equivalente (ae) 
  df <- df %>%
    mutate(
      ae = 1.0, 
      ae = case_when(
        edad < 1 ~ 0.35, edad == 1 ~ 0.37, edad == 2 ~ 0.46, 
        edad == 3 ~ 0.51, edad == 4 ~ 0.55, edad == 5 ~ 0.60, 
        edad == 6 ~ 0.64, edad == 7 ~ 0.66, edad == 8 ~ 0.68, edad == 9 ~ 0.69,
        # 10-17 varones
        hombre == 1 & edad == 10 ~ 0.79, hombre == 1 & edad == 11 ~ 0.82,
        hombre == 1 & edad == 12 ~ 0.85, hombre == 1 & edad == 13 ~ 0.90,
        hombre == 1 & edad == 14 ~ 0.96, hombre == 1 & edad == 15 ~ 1.00,
        hombre == 1 & edad == 16 ~ 1.03, hombre == 1 & edad == 17 ~ 1.04,
        # 10-17 mujeres
        hombre == 0 & edad == 10 ~ 0.70, hombre == 0 & edad == 11 ~ 0.72,
        hombre == 0 & edad == 12 ~ 0.74, hombre == 0 & edad == 13 ~ 0.76,
        hombre == 0 & edad == 14 ~ 0.76, hombre == 0 & edad == 15 ~ 0.77,
        hombre == 0 & edad == 16 ~ 0.77, hombre == 0 & edad == 17 ~ 0.77,
        # Adultos varones
        hombre == 1 & edad >= 18 & edad <= 29 ~ 1.02,
        hombre == 1 & edad >= 30 & edad <= 45 ~ 1.00,
        hombre == 1 & edad >= 46 & edad <= 60 ~ 1.00,
        hombre == 1 & edad >= 61 & edad <= 75 ~ 0.83,
        hombre == 1 & edad >= 76 & edad < 110 ~ 0.74,
        # Adultas mujeres
        hombre == 0 & edad >= 18 & edad <= 29 ~ 0.76,
        hombre == 0 & edad >= 30 & edad <= 45 ~ 0.77,
        hombre == 0 & edad >= 46 & edad <= 60 ~ 0.76,
        hombre == 0 & edad >= 61 & edad <= 75 ~ 0.67,
        hombre == 0 & edad >= 76 & edad < 110 ~ 0.63,
        # Mantener 1.0 si no se cumple ninguna condición (como en Python)
        TRUE ~ 1.0 
      )
    )
  
  # aef = suma de ae por id
  df <- df %>%
    group_by(id) %>%
    mutate(aef = sum(ae, na.rm = FALSE)) %>% # na.rm=FALSE replica el comportamiento de Pandas
    ungroup()
  
  # ingreso_oficial = itf / aef
  df <- df %>%
    mutate(ingreso_oficial = itf / aef)
  
  log_msg("Ejemplo de distribución de 'period':")
  print(df %>% 
          count(period) %>% 
          arrange(period) %>% 
          head(10))
  
  return(df)
}

merge_micro_canastas <- function(micro_eq, canastas) {
  log_msg("Haciendo merge micro-canastas (ano, trimestre, region)")
  
  merged <- left_join(
    micro_eq, canastas, 
    by = c("ano", "trimestre", "region"),
    suffix = c("", "_canastas"),
    relationship = "many-to-one"
  )
  
  # Verificar filas sin match 
  sin_match <- merged %>% 
    filter(is.na(lp_moderada) | is.na(lp_extrema)) %>%
    select(ano, trimestre, region) %>%
    distinct()
  
  if (nrow(sin_match) > 0) {
    log_msg("Advertencia: hay combinaciones año-trimestre-región sin canasta asociada:")
    print(sin_match)
  }
  
  # Variables 0/100 de pobreza e indigencia
  merged <- merged %>%
    mutate(
      pobre = ifelse(
        !is.na(ingreso_oficial) & !is.na(lp_moderada) & (ingreso_oficial < lp_moderada),
        100, 
        0
      ),
      indigente = ifelse(
        !is.na(ingreso_oficial) & !is.na(lp_extrema) & (ingreso_oficial < lp_extrema),
        100, 
        0
      )
    )
  
  log_msg(sprintf("Base final tras merge: %d filas x %d columnas", nrow(merged), ncol(merged)))
  return(merged)
}


tabla_pobreza_indigencia <- function(df) {
  
  # Función para calcular la media ponderada (Weighted Mean)
  wmean <- function(x, w) {
    mask <- !is.na(x) & !is.na(w)
    if (sum(mask) == 0) {
      return(NA_real_)
    }
    return(sum(x[mask] * w[mask]) / sum(w[mask]))
  }
  
  tabla <- df %>%
    group_by(period) %>%
    summarise(
      pobreza = wmean(pobre, pondih),
      indigencia = wmean(indigente, pondih)
    ) %>%
    arrange(period) %>%
    ungroup()
  
  log_msg(sprintf("Tabla pobreza/indigencia generada: %d filas", nrow(tabla)))
  return(tabla)
}



