setwd('~/Downloads/Limiti01012021_g/Reg01012021_g')
library(maps)
library(maptools)
library(rgdal)
library(sp)

library(shp2graph)
library(lubridate)

italy = readOGR("Reg01012021_g_WGS84.shp", GDAL1_integer64_policy = TRUE)
plot(italy)
summary(italy)
typeof(italy)

# prova per selezionare solo alcune regioni: poi va finita
nord_italia = italy
nord_italia$DEN_REG
nord_italia = nord_italia[which(nord_italia@data$DEN_REG=='Veneto' | nord_italia$DEN_REG=='Lombardia' | 
                                  nord_italia$DEN_REG=='Piemonte' | nord_italia$DEN_REG=='Emilia-Romagna'), ]
typeof(nord_italia)
plot(nord_italia)

# per plottare stazioni con lat e long
lat_long_point = cbind(as.numeric(as.character(dati$lat)), as.numeric(as.character(dati$long)))
head(lat_long_point)
x_y = lat_long_point
x_y = data.frame(lat=x_y[,1], long=x_y[,2])
head(x_y) 
coordinates(x_y) <- ~lat+long
# 
x_y@proj4string <- nord_italia@proj4string
head(x_y)

quartz()
plot(nord_italia ,main="Stations")
points(x_y, col = "cornflowerblue", cex = 0.1,pch=20)


  
