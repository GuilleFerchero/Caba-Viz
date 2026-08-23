rm(list = ls())


CABA_API_CLIENT_ID="9e47fd01a0c34c3ab5662b89ff5da905"
CABA_API_CLIENT_SECRET="67137cFb4Bc84F2890094d3947456539"

library(httr2)
install.packages("gtfsrealtime")
library(gtfsrealtime)

#' Obtener Token de Acceso OAuth2 para la API de CABA
#' @return Un string con el access token.
get_caba_token <- function() {
  # 1. Recuperar credenciales seguras
  client_id <- Sys.getenv("CABA_API_CLIENT_ID")
  client_secret <- Sys.getenv("CABA_API_CLIENT_SECRET")

  if (client_id == "" || client_secret == "") {
    stop("Error: No se encontraron las credenciales CABA_API_CLIENT_ID o CABA_API_CLIENT_SECRET en el .Renviron.")
  }

  # Endpoint de autenticación (AWS Cognito / OAuth2 de CABA)
  # Nota: Validar si el endpoint exacto es /oauth2/token o el provisto en tu documentación
  auth_url <- "https://api-transporte.buenosaires.gob.ar/token"

  # 2. Construir y ejecutar la petición POST
  req <- request(auth_url) %>%
    req_method("POST") %>%
    req_body_form(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret
    ) %>%
    req_retry(max_tries = 3) # Robustez ante microcortes

  # 3. Realizar el request y procesar la respuesta JSON
  resp <- req_perform(req)
  token_data <- resp_body_json(resp)

  return(token_data$access_token)
}

#' Consumir el Feed de Vehicle Positions (GTFS-RT) de CABA
#' @param token String. Token de acceso válido obtenido con get_caba_token().
#' @param transport_type String. Tipo de transporte ('colectivos' o 'subtes'). Por defecto 'colectivos'.
#' @return Un objeto de tipo GTFS-RT Message (parseado por gtfsrealtime).
get_caba_vehicle_positions <- function(token, transport_type = "colectivos") {

  # Base URL del feed de tiempo real
  # Ajustar el path exacto según la documentación oficial de CABA (ej: /gtfs/realtime o similar)
  base_feed_url <- "https://api-transporte.buenosaires.gob.ar/v2/gtfsrealtime/vehiclePositions"

  # Construir la request añadiendo el Token Bearer en los Headers
  req <- request(base_feed_url) %>%
    req_url_query(x_api_key = transport_type) %>% # O el parámetro de query correspondiente si aplica
    req_headers(
      Authorization = paste("Bearer", token),
      Accept = "application/x-protobuf"
    )

  # Ejecutar la descarga del archivo binario (.pb)
  message(paste("Consumiendo posiciones en tiempo real para:", transport_type))
  resp <- req_perform(req)

  # El cuerpo de la respuesta es binario (Protocol Buffers)
  raw_pb <- resp_body_raw(resp)

  # 4. Decodificación usando la librería 'gtfsrealtime' (requiere Java en el backend)
  # gtfsrealtime::readGTFSRealTime maneja la lectura del raw vector o de una ruta temporal
  # Pasamos el vector de bytes directamente si la función lo soporta, o escribimos un archivo temporal:

  tmp_file <- tempfile(fileext = ".pb")
  writeBin(raw_pb, tmp_file)
  on.exit(unlink(tmp_file)) # Limpieza del archivo temporal

  feed_message <- gtfsrealtime::readGTFSRealTime(tmp_file)

  return(feed_message)
}


# 1. Autenticar
token <- get_caba_token()

# 2. Traer posiciones de colectivos
feed_colectivos <- get_caba_vehicle_positions(token, transport_type = "colectivos")

# 3. Inspeccionar y aplanar el feed a un data.frame explotable
# La estructura devuelta por 'gtfsrealtime' suele ser un objeto con listas anidadas ($entity)
if (!is.null(feed_colectivos$entity)) {

  # Dependiendo de la versión de gtfsrealtime, se puede iterar o aplanar directamente:
  posiciones_df <- do.call(rbind, lapply(feed_colectivos$entity, function(e) {
    # Extraer campos de manera segura manejando nulos
    v <- e$vehicle
    data.frame(
      id          = e$id %||% NA,
      trip_id     = v$trip$trip_id %||% NA,
      route_id    = v$trip$route_id %||% NA,
      latitude    = v$position$latitude %||% NA,
      longitude   = v$position$longitude %||% NA,
      bearing     = v$position$bearing %||% NA,
      speed       = v$position$speed %||% NA,
      timestamp   = as.POSIXct(v$timestamp, origin="1970-01-01", tz="America/Argentina/Buenos_Aires"),
      stringsAsFactors = FALSE
    )
  }))

  print(head(posiciones_df))
}

