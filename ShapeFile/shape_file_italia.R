###########################################################################
#                      SHAPE NORTHEN ITALY                                #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

rm(list = ls())

# Packages ----------------------------------------------------------------

library(maps)
library(maptools)
library(rgdal)
library(sp)
library(shp2graph)
library(lubridate)
library(raster)
library(gganimate)
library(animation)

# Plot delle stazioni -----------------------------------------------------

italy = readOGR("Reg01012021_g/Reg01012021_g_WGS84.shp", GDAL1_integer64_policy = TRUE)
italy <- spTransform(italy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

nord_italia = italy[which(italy@data$DEN_REG=='Veneto' | italy@data$DEN_REG=='Lombardia' | 
                          italy@data$DEN_REG=='Piemonte' | italy@data$DEN_REG=='Emilia-Romagna'), ]

nord_italia <- spTransform(nord_italia, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# per plottare stazioni con lat e long

load("../Data/lat_long.Rda")

# ALL
lat_long_point = cbind(as.numeric(as.character(Stazioni[,"Lat"])), as.numeric(as.character(Stazioni[,"Long"])))
x_y = data.frame(lat=lat_long_point[,1], long=lat_long_point[,2])
coordinates(x_y) <- ~long+lat
x_y_lomb@proj4string <- nord_italia@proj4string

# LOMBARDIA
lat_long_point_lom = cbind(as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Lombardia" ),"Lat"])), as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Lombardia" ),"Long"])))
x_y_lom = data.frame(lat=lat_long_point_lom[,1], long=lat_long_point_lom[,2])
coordinates(x_y_lom) <- ~long+lat
x_y_lomb@proj4string <- nord_italia@proj4string

# PIEMONTE
lat_long_point_piem = cbind(as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Piemonte" ),"Lat"])), as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Piemonte" ),"Long"])))
x_y_piem = data.frame(lat=lat_long_point_piem[,1], long=lat_long_point_piem[,2])
coordinates(x_y_piem) <- ~long+lat
x_y_piem@proj4string <- nord_italia@proj4string

# VENETO
lat_long_point_ven = cbind(as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Veneto" ),"Lat"])), as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Veneto" ),"Long"])))
x_y_ven = data.frame(lat=lat_long_point_ven[,1], long=lat_long_point_ven[,2])
coordinates(x_y_ven) <- ~long+lat
x_y_ven@proj4string <- nord_italia@proj4string

# EMILIA
lat_long_point_em = cbind(as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Emilia-Romagna" ),"Lat"])), as.numeric(as.character(Stazioni[which(Stazioni$Regione == "Emilia-Romagna" ),"Long"])))
x_y_em = data.frame(lat=lat_long_point_em[,1], long=lat_long_point_em[,2])
coordinates(x_y_em) <- ~long+lat
x_y_em@proj4string <- nord_italia@proj4string



# PLOTS -------------------------------------------------------------------

# Plot by region
plot(nord_italia ,main="Stazioni")
points(x_y_lom, col = "cornflowerblue", cex = 1 ,pch=20)
points(x_y_em, col = "darkorange2", cex = 1 ,pch=20)
points(x_y_piem, col = "chartreuse3", cex = 1 ,pch=20)
points(x_y_ven, col = "palevioletred3", cex = 1 ,pch=20)

axis(1) # showing the axes helps to check whether the coordinates are what you expected
axis(2)

# Plot by altitude 
# plot(nord_italia ,main="Stazioni")
# points(x_y, col = rainbow(4, start = 0, end = 0.25)[x_y], cex = 1 ,pch=20)
