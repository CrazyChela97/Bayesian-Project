###########################################################################
#             FOURIER FITTING FOR EMILIA R/S/U - F/I/T                    #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------

library(pracma)
library(readr)
library(readxl)
library(dygraphs)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(RColorBrewer)
require(gridExtra)
library(plotly)
library(lubridate)
library(ggridges)
library(viridis)
library(mvtnorm) #norm library
library(car) #Box-Cox transformations, Hierarchical clustering
library(fda) #FDA
library(fields) #FDA
library(KernSmooth) #Local Polynomial (FDA)
library(fdakma) #KMA (FDA)
library(reshape)

# Dataset Creation --------------------------------------------------------

PM10 = read_csv('../Data/PM10_Emilia.csv')
PM10$Quota = scale(PM10$Quota, center = TRUE, scale = TRUE)
View(PM10)

PM10_2018 = PM10[which(PM10$Anno==2018),c(2,3,4,8,10,11,13)]
colnames(PM10_2018) = c("Data","NS","Valore","Giorno","Tipo","Area","Quota")
intermediate = powerTransform(PM10_2018$Valore[which(!is.na(PM10_2018$Valore))] ~ 1, family = "bcnPower")
intermediate$lambda = 0 
PM10_2018$Valore[which(!is.na(PM10_2018$Valore))] = bcnPower(PM10_2018$Valore[which(!is.na(PM10_2018$Valore))],
                                                        intermediate$lambda, gamma=intermediate$gamma)
PM10_2018$NS = as.factor(PM10_2018$NS)
PM10_2018$Tipo = as.factor(PM10_2018$Tipo)

Stazioni = as.factor(levels(PM10_2018$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2018$NS))
Dati_Stazioni[2] = PM10_2018$Tipo[pos]
Dati_Stazioni[3] = PM10_2018$Area[pos]
Dati_Stazioni[4] = PM10_2018$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Tipo","Area","Quota")
Rurale.f = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale" 
                               & Dati_Stazioni$Tipo=="Fondo"),] #11
Rurale.i = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"
                         & Dati_Stazioni$Tipo=="Industriale"),] #1
# Rurale.t = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"
#                                & Dati_Stazioni$Tipo=="Traffico"),]
# Nessun valore
Sub_Urb.f = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale" 
                               & Dati_Stazioni$Tipo=="Fondo"),] #22
Sub_Urb.i = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"
                               & Dati_Stazioni$Tipo=="Industriale"),] #4
Sub_Urb.t = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"
                               & Dati_Stazioni$Tipo=="Traffico"),] #11



dates = as.factor(seq.Date(as.Date("2018-01-01"), length.out = 365, by = "day"))


#RURALE
PM10_data.stazione_rurale.f = as.data.frame(dates)
for(i in 2:(dim(Rurale.f)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Rurale.f$Stazioni[i-1]),]
  pos = match(PM10_data.stazione_rurale.f$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale.f[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale.f) = c("Date",as.character(Rurale.f$Stazioni))
PM10_data.stazione_rurale.f$Date = as.Date(PM10_data.stazione_rurale.f$Date)

PM10_data.stazione_rurale.i = as.data.frame(dates)
for(i in 2:(dim(Rurale.i)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Rurale.i$Stazioni[i-1]),]
  pos = match(PM10_data.stazione_rurale.i$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale.i[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale.i) = c("Date",as.character(Rurale.i$Stazioni))
PM10_data.stazione_rurale.i$Date = as.Date(PM10_data.stazione_rurale.i$Date)


#SUB/URB
PM10_data.stazione_sub_urb.f = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb.f)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Sub_Urb.f$Stazioni[i-1]),] 
  pos = match(PM10_data.stazione_sub_urb.f$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb.f[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb.f) = c("Date",as.character(Sub_Urb.f$Stazioni))
PM10_data.stazione_sub_urb.f$Date = as.Date(PM10_data.stazione_sub_urb.f$Date)

PM10_data.stazione_sub_urb.i = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb.i)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Sub_Urb.i$Stazioni[i-1]),] 
  pos = match(PM10_data.stazione_sub_urb.i$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb.i[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb.i) = c("Date",as.character(Sub_Urb.i$Stazioni))
PM10_data.stazione_sub_urb.i$Date = as.Date(PM10_data.stazione_sub_urb.i$Date)

PM10_data.stazione_sub_urb.t = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb.t)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Sub_Urb.t$Stazioni[i-1]),] 
  pos = match(PM10_data.stazione_sub_urb.t$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb.t[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb.t) = c("Date",as.character(Sub_Urb.t$Stazioni))
PM10_data.stazione_sub_urb.t$Date = as.Date(PM10_data.stazione_sub_urb.t$Date)

# Mean and NA elimination -------------------------------------------------

Stazioni_mean_rurale.f = colMeans(PM10_data.stazione_rurale.f[,-1],na.rm = TRUE)
PM10_data.stazione_rurale.f = PM10_data.stazione_rurale.f[,-1]
#eliminazione NA
for(i in 1:dim(Rurale.f)[1]){
  for(j in 1:dim(PM10_data.stazione_rurale.f)[1]){
    if(is.na(PM10_data.stazione_rurale.f[j,i]))
      PM10_data.stazione_rurale.f[j,i] = Stazioni_mean_rurale.f[i]
  }
}
Stazioni_mean_rurale.i = mean(PM10_data.stazione_rurale.i[,-1],na.rm = TRUE)
nome = colnames(PM10_data.stazione_rurale.i)
PM10_data.stazione_rurale.i = PM10_data.stazione_rurale.i[,-1]
#eliminazione NA
PM10_data.stazione_rurale.i[which(is.na(PM10_data.stazione_rurale.i))] = Stazioni_mean_rurale.i
PM10_data.stazione_rurale.i = as.data.frame(PM10_data.stazione_rurale.i)
colnames(PM10_data.stazione_rurale.i) = nome[2]

Stazioni_mean_sub_urb.f = colMeans(PM10_data.stazione_sub_urb.f[,-1],na.rm = TRUE)
PM10_data.stazione_sub_urb.f = PM10_data.stazione_sub_urb.f[,-1]
#eliminazione NA
for(i in 1:dim(Sub_Urb.f)[1]){
  for(j in 1:dim(PM10_data.stazione_sub_urb.f)[1]){
    if(is.na(PM10_data.stazione_sub_urb.f[j,i]))
      PM10_data.stazione_sub_urb.f[j,i] = Stazioni_mean_sub_urb.f[i]
  }
}
Stazioni_mean_sub_urb.i = colMeans(PM10_data.stazione_sub_urb.i[,-1],na.rm = TRUE)
PM10_data.stazione_sub_urb.i = PM10_data.stazione_sub_urb.i[,-1]
#eliminazione NA
for(i in 1:dim(Sub_Urb.i)[1]){
  for(j in 1:dim(PM10_data.stazione_sub_urb.i)[1]){
    if(is.na(PM10_data.stazione_sub_urb.i[j,i]))
      PM10_data.stazione_sub_urb.i[j,i] = Stazioni_mean_sub_urb.i[i]
  }
}
Stazioni_mean_sub_urb.t = colMeans(PM10_data.stazione_sub_urb.t[,-1],na.rm = TRUE)
PM10_data.stazione_sub_urb.t = PM10_data.stazione_sub_urb.t[,-1]
#eliminazione NA
for(i in 1:dim(Sub_Urb.f)[1]){
  for(j in 1:dim(PM10_data.stazione_sub_urb.t)[1]){
    if(is.na(PM10_data.stazione_sub_urb.t[j,i]))
      PM10_data.stazione_sub_urb.t[j,i] = Stazioni_mean_sub_urb.t[i]
  }
}

dates = as.Date(dates)

Rurale_mean.f = rowMeans(PM10_data.stazione_rurale.f,na.rm = TRUE)
Rurale_mean.f = as.data.frame(Rurale_mean.f)
colnames(Rurale_mean.f) = "MeanValue"
Rurale_mean.f$Date = dates
#Rurale_mean.i = rowMeans(PM10_data.stazione_rurale.i,na.rm = TRUE) 
#Una sola stazione quindi no media
Rurale_mean.i = PM10_data.stazione_rurale.i
colnames(Rurale_mean.i) = "MeanValue"
Rurale_mean.i$Date = dates
Sub_Urb_mean.f = rowMeans(PM10_data.stazione_sub_urb.f,na.rm = TRUE)
Sub_Urb_mean.f = as.data.frame(Sub_Urb_mean.f)
colnames(Sub_Urb_mean.f) = "MeanValue"
Sub_Urb_mean.f$Date = dates
Sub_Urb_mean.i = rowMeans(PM10_data.stazione_sub_urb.i,na.rm = TRUE)
Sub_Urb_mean.i = as.data.frame(Sub_Urb_mean.i)
colnames(Sub_Urb_mean.i) = "MeanValue"
Sub_Urb_mean.i$Date = dates
Sub_Urb_mean.t = rowMeans(PM10_data.stazione_sub_urb.t,na.rm = TRUE)
Sub_Urb_mean.t = as.data.frame(Sub_Urb_mean.t)
colnames(Sub_Urb_mean.t) = "MeanValue"
Sub_Urb_mean.t$Date = dates

# Fourier Fitting ---------------------------------------------------------

giorni=1:365
basis <- create.fourier.basis(rangeval=c(1,365),nbasis=2)

#rural
rural.smooth <- smooth.basis(argvals=giorni, y=Rurale_mean.f$MeanValue, fdParobj=basis)
rurale.fd.media.f <- eval.fd(giorni, rural.smooth$fd)
rural.smooth <- smooth.basis(argvals=giorni, y=Rurale_mean.i$MeanValue, fdParobj=basis)
rurale.fd.media.i <- eval.fd(giorni, rural.smooth$fd)
#sub/urban
sub_urb.smooth <- smooth.basis(argvals=giorni, y=Sub_Urb_mean.f$MeanValue, fdParobj=basis)
sub_urb.fd.media.f <- eval.fd(giorni, sub_urb.smooth$fd)
sub_urb.smooth <- smooth.basis(argvals=giorni, y=Sub_Urb_mean.i$MeanValue, fdParobj=basis)
sub_urb.fd.media.i <- eval.fd(giorni, sub_urb.smooth$fd)
sub_urb.smooth <- smooth.basis(argvals=giorni, y=Sub_Urb_mean.t$MeanValue, fdParobj=basis)
sub_urb.fd.media.t <- eval.fd(giorni, sub_urb.smooth$fd)

# PLOTS
#colors
pal_r = colorRampPalette(c("cyan", "purple"))
pal_su = colorRampPalette(c("red", "yellow"))

#Dataset ggplot
PM10_data.stazione_rurale.f$Date = dates
data_rural.f = melt(PM10_data.stazione_rurale.f,id.vars= "Date")
PM10_data.stazione_rurale.i$Date = dates
data_rural.i = melt(PM10_data.stazione_rurale.i,id.vars= "Date")
PM10_data.stazione_sub_urb.f$Date = dates
data_us.f = melt(PM10_data.stazione_sub_urb.f,id.vars= "Date")
PM10_data.stazione_sub_urb.i$Date = dates
data_us.i = melt(PM10_data.stazione_sub_urb.i,id.vars= "Date")
PM10_data.stazione_sub_urb.t$Date = dates
data_us.t = melt(PM10_data.stazione_sub_urb.t,id.vars= "Date")

#Plot
raf_fit <- ggplot(data_rural.f,aes(x=Date, y=value, col=variable)) +
   geom_line() + 
   scale_color_manual(values=alpha(pal_r(12),.4)) +
   geom_line(data = Rurale_mean.f, aes(x=Date, y=rurale.fd.media.f), col = "darkblue", size=1) +
   ylim(c(0,6)) +
   labs(title="PM10-Emilia: RURAL AREA - FONDO",y="Log Values") +
   theme(legend.position="none") 

rai_fit <- ggplot(data_rural.i,aes(x=Date, y=value, col=variable)) +
   geom_line() + 
   scale_color_manual(values=alpha(pal_r(12),.4)) +
   geom_line(data = Rurale_mean.i, aes(x=Date, y=rurale.fd.media.i), col = "darkblue", size=1) +
   ylim(c(0,6)) +
   labs(title="PM10-Emilia: RURAL AREA - INDUSTRIAL",y="Log Values") +
   theme(legend.position="none") 

x11()
grid.arrange(raf_fit, rai_fit, ncol=2)

usaf_fit <- ggplot(data_us.f,aes(x=Date, y=value, col=variable)) +
  geom_line() + 
  scale_color_manual(values=alpha(pal_su(37),.4)) +
  geom_line(data = Sub_Urb_mean.f, aes(x=Date, y=sub_urb.fd.media.f), col = "darkred", size=1) +
  ylim(c(0,6)) +
  labs(title="PM10-Emilia: SUBURBAN & URBAN AREA - FONDO",y="Log Values") +
  theme(legend.position="none") 

usai_fit <- ggplot(data_us.i,aes(x=Date, y=value, col=variable)) +
  geom_line() + 
  scale_color_manual(values=alpha(pal_su(37),.4)) +
  geom_line(data = Sub_Urb_mean.i, aes(x=Date, y=sub_urb.fd.media.i), col = "darkred", size=1) +
  ylim(c(0,6)) +
  labs(title="PM10-Emilia: SUBURBAN & URBAN AREA - INDUSTRIAL",y="Log Values") +
  theme(legend.position="none") 

usat_fit <- ggplot(data_us.t,aes(x=Date, y=value, col=variable)) +
  geom_line() + 
  scale_color_manual(values=alpha(pal_su(37),.4)) +
  geom_line(data = Sub_Urb_mean.t, aes(x=Date, y=sub_urb.fd.media.t), col = "darkred", size=1) +
  ylim(c(0,6)) +
  labs(title="PM10-Emilia: SUBURBAN & URBAN AREA - TRAFFIC",y="Log Values") +
  theme(legend.position="none") 

x11()
grid.arrange(usaf_fit, usai_fit, usat_fit, ncol=3)






