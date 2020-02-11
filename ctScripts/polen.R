library(sf)
library(httr)
library(rvest)
library(jsonlite)
library(kableExtra)

polen <- st_read("data/captadores.csv", options=c("X_POSSIBLE_NAMES=lon","Y_POSSIBLE_NAMES=lat"))

polen$popup <- ""
for(i in 1:length(polen)) {
  response <- GET(as.character(polen[i,]$tabla))
  contenido <- content(response, 'text')
  df.sp<-fromJSON(contenido)$features$properties[8:10]
  colnames(df.sp) <- c("Fecha", "Medida", "Especie")
  polen[i,]$popup <- kable_styling(kable(df.sp),bootstrap_options =c("striped","hover"), font_size = 8)#htmlTable(fromJSON(contenido)$features$properties[8:10]) 
}
save(polen, file = "data/polen.RData")