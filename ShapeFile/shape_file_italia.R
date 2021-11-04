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
library(corrplot)

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
points(x_y_lom, col = "cornflowerblue", cex = 0.8 ,pch=20)
points(x_y_em, col = "darkorange2", cex = 0.8 ,pch=20)
points(x_y_piem, col = "chartreuse3", cex = 0.8 ,pch=20)
points(x_y_ven, col = "palevioletred3", cex = 0.8 ,pch=20)

axis(1) # showing the axes helps to check whether the coordinates are what you expected
axis(2)


# PLOT BY MONTH ---------------------------------------------------------
StazioniEmilia <- Stazioni[which(Stazioni$Regione == "Emilia-Romagna" ),]

full_data <- read.csv(file = '../Data/Total_Data_2018.csv')

emilia = italy[which(italy@data$DEN_REG=='Emilia-Romagna'), ]
emilia <- spTransform(emilia, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


# consideriamo 12 mesi e il valore medio mensile per ogni stazione in emilia romagna
months = c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")
data_mat = matrix(0,nrow=(length(months)*nrow(StazioniEmilia)),ncol=3)


for (m in 1:length(months)) {
  for (i in 1:nrow(StazioniEmilia)) {
    j = (m-1)*nrow(StazioniEmilia)+i
    data_mat[j,1]= m  # month
    data_mat[j,2]= StazioniEmilia$NomeStazione[i]   # id stazione
    data_mat[j,3]= mean(full_data[which(full_data$NomeStazione==StazioniEmilia$NomeStazione[i] & full_data$Mese==m),"Valore"])  # media mensile
  }
}

emilia_data = data.frame(data_mat)
names(emilia_data) = c("Month","Id_stazione", "Valore")
emilia_data = emilia_data[which(!is.nan(emilia_data$Valore)),]

png("images/input%03d.png")
col_bal=colorRampPalette(c("yellow", "orange","red","red4"), bias= 1)
emilia_data$color = col_bal(100)[as.numeric(cut(emilia_data$Valore,breaks=100))]

for (m in 1:length(months)) {
  dati = emilia_data[which(emilia_data$Month==m), ]
  plot(emilia ,main="PM10 medio mensile (Emilia - 2018)", cex.main=2)
  points(x_y_em, col="black", bg = dati[,4], cex = 2 ,pch=21)
  axis(1) 
  axis(2)
  colorlegend(col_bal(100), round(seq(min(emilia_data$Valore),max(emilia_data$Valore), len = 3),1), ylim=c(43.5,44), xlim = c(9.3,9.7),  align = 'l')
  text(12, 45.4, months[m], col="red4", cex=2)
}
dev.off()

png_files <- sprintf("images/input%03d.png", 1:length(months))
av::av_encode_video(png_files, 'images/output_prova.mp4', framerate = 1)

# PLOT PROVINCE ---------------------------------------------------------
italy_prov = readOGR("ProvCM01012021_g/ProvCM01012021_g_WGS84.shp", GDAL1_integer64_policy = TRUE)
summary(italy_prov) 

# ALL
nord_it = italy_prov[which(italy_prov@data$COD_REG %in% c(1,2,3,4,5,6,7,8,9)), ]
plot(nord_it)
summary(nord_it) 

dati = read.csv(file = '../Data/Total_Data_2018.csv')

prov = nord_it$SIGLA
PM10_prov = matrix(0,nrow=(12*length(prov)), ncol=3)
PM10_prov[ ,1] = as.character(prov)
for (m in 1:12) {
  for (j in ((m-1)*length(prov)+1):(m*length(prov))) {
    data = dati[which(dati$Mese==m & dati$Provincia==PM10_prov[j,1]), ]
    PM10_prov[j,2] = mean(data$Valore)
    PM10_prov[j,3] = m
  }
}

PM10_prov = as.data.frame(PM10_prov)
names(PM10_prov) = c('Provincia', 'Valore_Medio', 'Mese')
PM10_prov$Valore_Medio = as.numeric(as.character(PM10_prov$Valore_Medio))
PM10_prov[is.na(PM10_prov$Valore_Medio), 2] = 0

# plot video
col_bal=colorRampPalette(c("khaki1","yellow","orange","red","red4"), bias=1)
PM10_prov$color = col_bal(100)[as.numeric(cut(PM10_prov$Valore_Medio, breaks=100))]
PM10_prov[PM10_prov$Valore_Medio==0, 4] = 'white' 

png("input%03d.png")
for (m in 1:12) {
  data = PM10_prov[which(PM10_prov$Mese==m), ]
  plot(nord_it, col=data$color, main="PM10 in time")
  axis(1) 
  axis(2)
  colorlegend(col_bal(100), round(seq(min(PM10_prov$Valore),max(PM10_prov$Valore), len = 3),1), 
              ylim=c(43.5,44), xlim = c(9.3,9.7),  align = 'l')
  text(12, 45.4, months[m], col="red4", cex=2)
}
dev.off()
png_files <- sprintf("input%03d.png", 1:12)
av::av_encode_video(png_files, 'province_mesi.mp4', framerate = 1.5)




# EMILIA
emilia_prov = italy_prov[which(italy_prov@data$COD_REG == 8), ]
plot(emilia_prov)

dati = read.csv(file = '../Data/PM10_Emilia.csv')
dati = dati[which(dati$Anno==2018), ]

prov = emilia_prov$SIGLA
PM10_prov = matrix(0,nrow=(12*length(prov)), ncol=3)
PM10_prov[ ,1] = as.character(prov)
for (m in 1:12) {
  for (j in ((m-1)*length(prov)+1):(m*length(prov))) {
    data = dati[which(dati$Mese==m & dati$Provincia==PM10_prov[j,1]), ]
    PM10_prov[j,2] = mean(data$Valore)
    PM10_prov[j,3] = m
  }
}

PM10_prov = as.data.frame(PM10_prov)
names(PM10_prov) = c('Provincia', 'Valore_Medio', 'Mese')
PM10_prov$Valore_Medio = as.numeric(as.character(PM10_prov$Valore_Medio))

# plot video
col_bal=colorRampPalette(c("khaki1","yellow","orange","red","red4"), bias=1)
PM10_prov$color = col_bal(100)[as.numeric(cut(PM10_prov$Valore_Medio, breaks=100))]
PM10_prov[PM10_prov$Valore_Medio==0, 4] = 'white' 

png("input%03d.png")
for (m in 1:12) {
  data = PM10_prov[which(PM10_prov$Mese==m), ]
  plot(emilia_prov, col=data$color, main="PM10 - Emilia")
  axis(1) 
  axis(2)
  colorlegend(col_bal(100), round(seq(min(PM10_prov$Valore),max(PM10_prov$Valore), len = 3),1), 
              ylim=c(43.5,44), xlim = c(9.3,9.7),  align = 'l')
  text(12, 45.4, months[m], col="red4", cex=2)
}
dev.off()
png_files <- sprintf("input%03d.png", 1:12)
av::av_encode_video(png_files, 'Emilia_prov_mesi.mp4', framerate = 1.5)

