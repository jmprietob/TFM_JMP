require(ggplot2)
library(ggmap)
library(caRtociudad)
library(rgbif)
library(sp)
library(scales)
library(gridExtra)
library(meteoForecast)
library(gtable)
library(dplyr)


coor <- cartociudad_geocode("MADRID")
p <- select(coor,lat, lng)
names(p) <- c("lat", "lon")

if(is.null(p)){return()}

tempK <- getPoint(c(p$lon, p$lat), vars = 'temp')
prec <- getPoint(c(p$lon, p$lat), vars = 'prec')
sl <- getPoint(c(p$lon, p$lat), vars = 'snowlevel')
sn <- getPoint(c(p$lon, p$lat), vars = 'snow_prec')
mod <- getPoint(c(p$lon, p$lat), vars = 'mod')
dir <- getPoint(c(p$lon, p$lat), vars = 'dir')
rh <- getPoint(c(p$lon, p$lat), vars = 'rh')
mslp <- getPoint(c(p$lon, p$lat), vars = 'mslp')
cft <- getPoint(c(p$lon, p$lat), vars = 'cft')
cfl <- getPoint(c(p$lon, p$lat), vars = 'cfl')
cfm <- getPoint(c(p$lon, p$lat), vars = 'cfm')
cfh <- getPoint(c(p$lon, p$lat), vars = 'cfh')
conv_prec <- getPoint(c(p$lon, p$lat), vars = 'conv_prec')
## Cell does not coincide exactly with request
attr(tempK, 'lat')
attr(tempK, 'lon')
attr(prec, 'lat')
attr(prec, 'lon')
attr(sl, 'lat')
attr(sl, 'lon')
attr(sn, 'lat')
attr(sn, 'lon')
attr(mod, 'lat')
attr(mod, 'lon')
attr(dir, 'lat')
attr(dir, 'lon')
attr(rh, 'lat')
attr(rh, 'lon')
attr(mslp, 'lat')
attr(mslp, 'lon')
attr(cft, 'lat')
attr(cft, 'lon')
attr(conv_prec, 'lat')
attr(conv_prec, 'lon')
# Nubes bajas,medias, altas
# attr(cfl, 'lat')
# attr(cfl, 'lon')
# attr(cfm, 'lat')
# attr(cfm, 'lon')
# attr(cfh, 'lat')
# attr(cfh, 'lon')
## Units conversion
tempC <- tempK - 273

# funcion conversion zoo a df
zooToDf<-function(z) {
  df <- as.data.frame(z)
  df$Date <- time(z) #create a Date column
  rownames(df) <- NULL #so row names not filled with dates
  df <- df[,c(ncol(df), 1:(ncol(df)-1))] #reorder columns so Date first
  return(df)
}

nubes <- zooToDf(cft)
nubes$x <- row.names(nubes)
nubes$y <- 10
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('white','grey20'))
nubes$Col <- rbPal(10)[as.numeric(cut(nubes$z,breaks = 10))]
tt <- time(prec)
# The following is just the sequence 1:36.
#   If you wanted only every third month plotted,
#   use a sequence like ix <- seq(1, length(tt), 3)
ix <- seq_along(tt)
ixp<- seq(3,96,3)
# What format do you want for your labels.
#   This yields abbreviated month - abbreviated year
fmt <- "%A-%H:%M"
labs <- format(tt, fmt) # Generate the vector of your labels

labs.p <- labs
labs.p[-ixp]<-""

qplot.zoo <- function(x,tipo,color,laby) {
  if(all(class(x) != 'zoo')) stop('x must be a zoo object')
  x.df <- data.frame(Date=index(x), Value=as.numeric(coredata(x)))
  x.df$dir <- as.numeric(coredata(dir))
  maximo <- max(x.df$Value)
  p <- ggplot(x.df, aes(x=Date, y=Value))+xlab('')+ylab(laby)+
    #theme_minimal() +#theme_grey()+#theme_hc()+#theme_fivethirtyeight()#theme_tufte()#theme_wsj()#theme_gdocs()#theme_few()#theme_pander()#theme_bw()
    theme(axis.text = element_text(size = 8.5),
          axis.title.x = element_text(size=8.5,face="bold"),
          axis.title.y = element_text(size=8.5,face="bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "gray60", size = 0.2),
          panel.grid.minor.x = element_line(color = "gray90", size = 0.1),
          panel.grid.major.y = element_line(color = "gray90", size = 0.1),
          plot.margin = unit(c(0.2,0.2,0.2,0.5), "cm"))
  if (tipo == 'barra'){
    y.df <- data.frame(Date=index(sn), Value=as.numeric(coredata(sn)))
    p + geom_bar(stat="identity", fill=color)+
      geom_bar(aes(Date, Value), data = y.df,stat="identity" )+
      geom_point(data=nubes, aes(Date,maximo+1),alpha=nubes$z, size=3, shape=15)+
      scale_color_manual(values=c("blue", "red"))
  }else{
    if (tipo == 'wind'){
      p <- ggplot(x.df, aes(x=Date, y=Value, label= sprintf('\u2193')))+xlab('')+ylab(laby)+
        #theme_minimal() +#theme_grey()+#theme_hc()+#theme_fivethirtyeight()#theme_tufte()#theme_wsj()#theme_gdocs()#theme_few()#theme_pander()#theme_bw()
        theme(axis.text = element_text(size = 8.5),
              axis.title.x = element_text(size=8.5,face="bold"),
              axis.title.y = element_text(size=8.5,face="bold"),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(color = "gray60", size = 0.2),
              panel.grid.minor.x = element_line(color = "gray90", size = 0.1),
              panel.grid.major.y = element_line(color = "gray90", size = 0.1),
              plot.margin = unit(c(0.2,0.2,0.2,0.5), "cm"))
      p + geom_line(colour=color, size=2)+
        geom_text(fontface =  "bold",size = 3.5, angle=dir, color="gray20")
      
      
    }else{
      p + geom_line(colour=color, size=1)+geom_hline(yintercept = 0,colour ="steelblue", size=0.5, alpha=0.75)
      
    }
  }
}
p1 <- qplot.zoo(prec,"barra","steelblue","Lluvia mm")
p3 <- qplot.zoo(tempC,"lin","red","Temperatura ºC")
p4 <- qplot.zoo(sl,"sn","orange","Snow level")
p6 <- qplot.zoo(mod,"wind","cadetblue2","Velodidad del viento m/s")


gl = lapply(list(p1, p3, p4, p6), ggplotGrob)

g = do.call(rbind, c(gl, size="first"))
g$widths = do.call(grid::unit.pmax, lapply(gl, "[[", "widths"))
save(g, file = "data/meteo.RData")