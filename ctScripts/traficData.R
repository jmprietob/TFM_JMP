library(jsonlite)
require(httr)
library(XML)
library(sf)

############# Enlace a datos ##############################################
#ayto
datosBrutosTrafico <- 'http://informo.munimadrid.es/informo/tmadrid/pm.xml'
datosBrutosIncidencias <- 'http://informo.munimadrid.es/informo/tmadrid/incid_aytomadrid.xml'
#dgt
url <- 'http://infocar.dgt.es/etraffic/BuscarElementos?latNS=45.82599&longNS=86.20972&latSW=-26.43483&longSW=-93.96606&zoom=3&accion=getElementos&Camaras=true&SensoresTrafico=true&SensoresMeteorologico=true&Paneles=true&Radares=true&IncidenciasRETENCION=true&IncidenciasOBRAS=true&IncidenciasMETEOROLOGICA=true&IncidenciasPUERTOS=true&IncidenciasOTROS=true&IncidenciasEVENTOS=true&IncidenciasRESTRICCIONES=true&niveles=true&caracter=acontecimiento'
iconosBase <- 'http://infocar.dgt.es/etraffic/img/iconosIncitar/'

# funcion lee y limpia incidencias de la DGT
readDGT <- function(url){
  df <- read_json(url, simplifyVector = TRUE)
  df$descripcion <- gsub("<span style=\"margin-top:10px; float:left; clear:both\">", "", df$descripcion)
  df$descripcion <- gsub("</span>", "", df$descripcion)
  id <- as.numeric(rownames(df))
  df <- cbind(id=id, df)
  df
}

# funcion lee y limpia datos trafico ayuntamiento
procesaTrafico <- function(datos){
  datosGET <- GET(datos)
  datosXML <- xmlParse(datosGET)
  df <- xmlToDataFrame((nodes=getNodeSet(datosXML,"//pm")))
  df$st_x <- as.numeric(gsub(",", ".", gsub("\\.", "", df$st_x)))
  df$st_y <- as.numeric(gsub(",", ".", gsub("\\.", "", df$st_y)))
  df$intensidad <- as.numeric(gsub(",", ".", gsub("\\.", "", df$intensidad)))
  df$ocupacion <- as.numeric(gsub(",", ".", gsub("\\.", "", df$ocupacion)))
  df<-df[complete.cases(df[ , c(1,2,4,5,6,7,8,11,12)]),]
  sfdf = st_as_sf(df, coords = c("st_x", "st_y"), crs = 25830)
  sfdf <- st_transform(sfdf, 4326)
  return(sfdf) 
}

# funcion lee y limpia incidencias de tráfico del ayuntamiento
procesaIncidencias <- function(datos){
  datosGET <- GET(datos)
  datosXML <- xmlParse(datosGET)
  dfi <- xmlToDataFrame((nodes=getNodeSet(datosXML,"//Incidencia")), stringsAsFactors=FALSE)
  dfi$lng <- as.numeric(dfi$longitud)
  dfi$lat <- as.numeric(dfi$latitud)
  sfdf = st_as_sf(dfi, coords = c("lng", "lat"), crs = 4326)
  sfdf$lng <- as.numeric(dfi$longitud)
  sfdf$lat <- as.numeric(dfi$latitud)
  sfdf$popup <- paste('<div><b>',sfdf$nom_tipo_incidencia,'</b><br>','<b>Inicio:</b> ',strptime(sfdf$fh_inicio,"%Y-%m-%dT%H:%M:%S"),'<br>' ,'<b>Final:</b> ',strptime(sfdf$fh_final,"%Y-%m-%dT%H:%M:%S"),'<br>' ,'<b>Descripción:</b> ',sfdf$descripcion,'</div>')
  return(sfdf) 
}

# Ejecuta funciones
df <- readDGT(url)
datos <- procesaTrafico(datosBrutosTrafico)
incidencias <- procesaIncidencias(datosBrutosIncidencias)
# Guarda datos para uso de la aplicación
save(df,incidencias, datos, file = "data/trafico.RData")

