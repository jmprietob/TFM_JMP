#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
## app.R ##
library(shinydashboard)
library(sf)
library(XML)
require(httr)
require(ggplot2)
library(ggmap)
library(caRtociudad)
library(rgbif)
library(sp)
library(scales)
library(gridExtra)
library(leaflet)
library(leaflet.extras)
library(markdown)
library(meteoForecast)
library(gtable)
library(dplyr)
library(data.table)
library(jsonlite)
library(DT)
library(magrittr)
library(kableExtra)

ui <- dashboardPage(skin = "blue",
  
  dashboardHeader(title = "CMIC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tráfico", tabName = "traffic", icon = icon("traffic-light", class = NULL, lib = "font-awesome")), #icon = icon("globe")
      menuItem("Meteorología", tabName = "meteo", icon = icon("thermometer-half", class = NULL, lib = "font-awesome")),
      menuItem("Calidad del Aire", tabName = "calidad", icon = icon("chart-area", class = NULL, lib = "font-awesome"))
    )
    
  ),
  ## Body content
  dashboardBody(tabItems(
    
    # First tab content
    tabItem(tabName = "traffic",
            fluidRow(
              column(12, leafletOutput("traficMap")),
              column(12, dataTableOutput("tbl")),
              column(12, dataTableOutput("tbl_incidencias"))
            )
            ),
    
    # Second tab content
    tabItem(tabName = "meteo",
             # column(7,textInput("text", "Location:", "Madrid")),
             # column(1,""),
             # column(3,submitButton("Actualizar", icon("refresh"))),
             # column(1,""),
             fluidRow(
               leafletOutput("meteoMap", width = "100%")
             ,
               plotOutput("meteogalicia", width = "100%")
    )
    ),
    tabItem(tabName = "calidad",fluidRow(tags$style(type = "text/css", "#map {height: calc(100vh - 10px) !important;}"),
                                         leafletOutput("airQualityMap")),
								fluidRow(
								  valueBox(value = NULL, subtitle = "Calidad del aire",
								  icon = NULL, color = "aqua", 
								  href = "http://gestiona.madrid.org/azul_internet/run/j/AvisosAccion.icm"),
								  valueBox(value = NULL, subtitle = "Captadores de polen",
								  icon = NULL, color = "yellow", 
								  href = "http://www.comunidad.madrid/servicios/salud/polen")
									)					
								)
    
    )))

server <- function(input, output) {
  

  iconosBase <- 'http://infocar.dgt.es/etraffic/img/iconosIncitar/'
   
  in_bounding_box <- function(data, lat, lng, bounds) {
    data %>%
      dplyr::filter(
        lat > bounds$south &
          lat < bounds$north &
          lng < bounds$east &
          lng > bounds$west
      )
  }
  
  load('data/trafico.RData')
  
  popup <- leafpop::popupTable(datos, zcol="intensidad")
  camaras <- st_read('data/CCTV.kml')

  qpal <- colorQuantile("Blues", datos$intensidad, n = 7)
  pal <- colorNumeric(c("green", "blue", "red"), 0:10000)
  
  cameraIcon <- makeIcon(
    iconUrl = "icons/camara.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 20, iconAnchorY = 20
  )
  
  incidenciaIcon <- makeIcon(
    iconUrl = "icons/incidencia.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 20, iconAnchorY = 20
  )
  
  observe({
    
    row_selected = data_map()[input$tbl_rows_selected,]
    
    leafletProxy('traficMap') %>% 
      setView(lng = row_selected$lng, lat = row_selected$lat, zoom = 12)
  })
  
  observe({
    
    row_selected = incidencias_map()[input$tbl_incidencias_rows_selected,]
    
    leafletProxy('traficMap') %>% 
      setView(lng = row_selected$lng, lat = row_selected$lat, zoom = 15)
  })
  
  output$traficMap <- renderLeaflet({
    leaflet() %>% setView(-3.6994602,40.4274167, 11) %>%
      addProviderTiles("Stamen.TonerLite",group = "OSM") %>%
      addProviderTiles("Esri", group = "Esri") %>%
      addLayersControl(baseGroups = c("OSM", "Esri"), overlayGroups = c("Tráfico", "Cámaras de tráfico", "Incidencias", "DGT")) %>%
      addCircleMarkers(data=datos,radius = 3, stroke = FALSE,
                       fillOpacity = 0.75, color = ~pal(intensidad), group = "Tráfico")  %>% 
      addMarkers(data=camaras, popup = camaras$Description, icon = cameraIcon, group = "Cámaras de tráfico") %>%
      addMarkers(data=incidencias, popup = incidencias$popup, icon = incidenciaIcon, group = "Incidencias") %>%
      addLegend(pal = pal,
                values = c(0,1000,5000,10000),
                opacity = 0.75,
                title = "vehículos/hora",
                position = "topleft")%>%
      addMarkers(data = df ,
                 ~ lng ,
                 ~ lat,
                 popup = df$descripcion, icon = makeIcon(
                   iconUrl = paste(iconosBase,df$icono,sep=''),
                   iconWidth = 25, iconHeight = 25,
                   iconAnchorX = 24, iconAnchorY = 24
                 ),group = "DGT"
      )
    
  })
  
  data_map <- reactive({
    if (is.null(input$traficMap_bounds)) {
      df
    } else {
      bounds <- input$traficMap_bounds
      in_bounding_box(df, lat, lng, bounds)
    }
  })
  
  incidencias_map <- reactive({
    if (is.null(input$traficMap_bounds)) {
      incidencias
    } else {
      bounds <- input$traficMap_bounds
      in_bounding_box(incidencias, lat, lng, bounds)
    }
  })
  
  # table
  output$tbl <- renderDataTable({
    datatable(
      data = data_map()[,c(2,3,4, 23)],
      selection = "single",options=list(stateSave = TRUE)
    )
  })
  
  output$tbl_incidencias <- renderDataTable({
    datatable(
      data = incidencias_map()[,c(4,10, 25,26)],
      selection = "single",options=list(stateSave = TRUE)
    )
  })
  
  #######################################################################
  
  ############ Meteo

  punto <- reactive({
    
    coor <- cartociudad_geocode("MADRID")
    p <- select(coor,lat, lng)
    names(p) <- c("lat", "lon")
    p
  })
  
  dataJsonOWM <- reactive({
    if(is.null(punto())){return()} 
    apiKey='b3bd74e99f3794b5ef30b092f083f223'
    url <- paste('http://api.openweathermap.org/data/2.5/find?lat=',punto()$lat,'&lon=',punto()$lon,'&cnt=25&units=metric&lang=es&appid=',apiKey,sep="")
    weather.entry <- jsonlite::fromJSON(url,simplifyMatrix = F,simplifyDataFrame = F,flatten = T)

    weather.es<-NULL

    for (i in 1:length(weather.entry$list)){

      longitud<-length(weather.entry$list[[i]]$main)

      if (longitud<7) {
        name<-weather.entry$list[[i]]$name
        lon<-weather.entry$list[[i]]$coord$lon
        lat<-weather.entry$list[[i]]$coord$lat
        temperature<-weather.entry$list[[i]]$main$temp
        humidity<-weather.entry$list[[i]]$main$humidity
        wind.speed<-weather.entry$list[[i]]$wind$speed
        wind.deg<-weather.entry$list[[i]]$wind$deg
        desc<-weather.entry$list[[i]]$weather[[1]]$description
        icon<-weather.entry$list[[i]]$weather[[1]]$icon
        if (is.null(wind.speed)){ wind.speed<-NA}
        if (is.null(wind.deg)){ wind.deg<-NA}
        if (is.null(humidity)){ humidity<-NA}
        if (is.null(temperature)){ temperature<-NA}
        weather.es<-rbind(data.frame(name,lon,lat,temperature,humidity,wind.speed,wind.deg,desc,longitud, icon),weather.es)
      }
    }

    weather.es$content <- paste0("<div><h4>",weather.es$name,"</h4>",
                                 "Temperatura: ", weather.es$temperature," ºC</br>",
                                 "Humedad: ", weather.es$humidity, " %</br>",
                                 "Descripción: ", weather.es$desc, "</br>",
                                 "<img src='http://openweathermap.org/img/w/",weather.es$icon,".png' alt=''></br></div>")

    weather.es
  })
  # 
  output$meteogalicia <- renderPlot({

    load("data/meteo.RData")
    
    grid::grid.newpage()
    grid::grid.draw(g)

  })

  output$meteoMap <- renderLeaflet({
    
    df<-dataJsonOWM()
    coordinates(df)=~lon + lat

    greenLeafIcon <- makeIcon(
      iconUrl = paste0("http://openweathermap.org/img/w/",df[1,]$icon,".png"),
      iconWidth = 35, iconHeight = 35,
      iconAnchorX = 34, iconAnchorY = 34
    )

    leaflet(df) %>%
      #addTiles()%>%
      setView(lat=punto()$lat,lng=punto()$lon,zoom=12)%>%
      addWMSTiles(
        "http://www.ign.es/wms-inspire/ign-base",
        layers = "IGNBaseTodo",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "IGN spain"
      ) %>%
      addMarkers(data = df, popup = df$content , icon= greenLeafIcon)
  })
  
  #############################################################
  
  ########### calidad del aire##############################

  load("data/caire.RData")
  load("data/polen.RData")
  
  captadorIcon <- makeIcon(
    iconUrl = "icons/captador.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 20, iconAnchorY = 20
  )
  greenLeafIcon <- makeIcon(
    iconUrl = "icons/aqi-icon-airdata.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 20, iconAnchorY = 20
  )
  
  output$airQualityMap <- renderLeaflet({
  leaflet(data) %>%
    #addTiles()%>%
    addWMSTiles(
      "http://www.ign.es/wms-inspire/ign-base",
      layers = "IGNBaseTodo-gris",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      attribution = "IGN spain",
      group = "Mapa base IGN"
    ) %>%
      addLayersControl(baseGroups = c("Mapa base IGN"), overlayGroups = c("NO2", "Captadores Polen")) %>%
    addMarkers(data = data ,popup = data$popup, icon = greenLeafIcon, group = "NO2")%>%
    addMarkers(data = polen, popup = polen$popup, icon = captadorIcon,group = "Captadores Polen")
  })
  
  ###########################################################
  
}

shinyApp(ui, server)