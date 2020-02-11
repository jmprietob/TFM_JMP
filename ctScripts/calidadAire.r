library(data.table)
library(ggplot2)
library(sp)
########### calidad del aire##############################

generaGrafico <- function(H01,H02,H03,H04,H05,H06,H07,H08,H09,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23,H24)
{
  v <- c(H01,H02,H03,H04,H05,H06,H07,H08,H09,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23,H24)
  r <- c(0)
  v <- v[!v %in% r]
  h <- c(1:length(v))
  df <- data.frame(h,v)
  names(df) <- c("hora","valor")
  #xyplot(y~x, type="l", data=df)
  p <- ggplot(df, aes(x=hora, y=valor)) +
    geom_line(color="#69b3a2", size=2, alpha=0.9)+
    #geom_hline(yintercept=180, linetype="dashed", color = "red")+
    theme_minimal() +
    ggtitle("NO2 durante el día de hoy")
  ggsave('imagen.png',p, width =4.5, height =3, dpi = 96 )# width=300 ,height =200 ,units = 'mm', 
  img_uri('imagen.png')
}

img_uri <- function(x) { sprintf('<img src="%s" width="300px"/>', knitr::image_uri(x)) }

stationsAyto <- 'https://datos.madrid.es/egob/catalogo/212629-1-estaciones-control-aire.csv'
dayAyto <- 'https://datos.madrid.es/egob/catalogo/212531-10515086-calidad-aire-tiempo-real.csv'
stationsCM <- 'https://datos.comunidad.madrid/catalogo/dataset/3a1ab315-c3a5-405c-b2c8-8586e272d97b/resource/132cd18b-c81e-45c1-a358-baa930252353/download/calidad_aire_estaciones.csv'
dayCM <- 'https://datos.comunidad.madrid/catalogo/dataset/cb5b856f-71a4-4e34-8539-84a7e994c972/resource/9fd86617-370a-4770-8a92-0c42ea02d6a1/download/calidad_aire_datos_dia.csv'

stationsData <- fread(stationsCM)
setkey(stationsData,estacion_codigo)
dayData <- fread(dayCM)
dayDataNO2 <-  dayData[magnitud == 8]
dayDataNO2$punto_muestreo = as.integer(sub("_8_8", "", dayDataNO2$punto_muestreo))
setkey(dayDataNO2,punto_muestreo)
dataCM <- stationsData[dayDataNO2]

stationsAyto <- fread(stationsAyto)
setkey(stationsAyto,CODIGO_CORTO)
dayDataAyto <- fread(dayAyto)
dataAytoNO2 <-  dayDataAyto[MAGNITUD == 8]

setkey(dataAytoNO2,ESTACION)
data <- stationsAyto[dataAytoNO2]
columnas <- paste('H',seq(1,24,1),sep='')
data[, img := generaGrafico(H01,H02,H03,H04,H05,H06,H07,H08,H09,H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23,H24), by = seq_len(nrow(data))]
dataCM[, img := generaGrafico(h01,h02,h03,h04,h05,h06,h07,h08,h09,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23,h24), by = seq_len(nrow(dataCM))]

data[, popup := paste(sep = "<br/>",ESTACION, img)]
dataCM[, popup := paste(sep = "<br/>",estacion_municipio, img)]

coordinates(data)=~LONGITUD + LATITUD
proj4string(data) <- CRS("+init=epsg:4326")
data@data <- data@data[,-(2:79)]
coordinates(dataCM)=~estacion_coord_UTM_ETRS89_x + estacion_coord_UTM_ETRS89_y
proj4string(dataCM) <- CRS("+init=epsg:25830")
dataCM.ll <- spTransform(dataCM, CRS=CRS("+init=epsg:4326"))
dataCM.ll@data <- dataCM.ll@data[,-(2:78)]
names(dataCM.ll) <- names(data)
data <- rbind(data, dataCM.ll)
save(data, file = "data/caire.RData")