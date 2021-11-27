###########################################################################
#                FOURIER FITTING FOR EMILIA R/S/U                         #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------

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
#STAN: C:/Users/franc/Documents/.cmdstanr/cmdstan-2.28.2


# Functional dataset ------------------------------------------------------

PM10 = read_csv('Data/PM10_Emilia.csv')
View(PM10)

PM10_2018 = PM10[which(PM10$Anno==2018),c(2,3,4,8,9,11,13)]
colnames(PM10_2018) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2018$NS = as.factor(PM10_2018$NS)

Stazioni = as.factor(levels(PM10_2018$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2018$NS))
Dati_Stazioni[2] = PM10_2018$Provincia[pos]
Dati_Stazioni[3] = PM10_2018$Area[pos]
Dati_Stazioni[4] = PM10_2018$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2018-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

# Mean and Other Values ---------------------------------------------------

Stazioni_mean_rurale = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

# Fourier Fitting ---------------------------------------------------------

#rurale
PM10_data.stazione_rurale = PM10_data.stazione_rurale[,-1]

#eliminazione NA
for(i in 1:dim(Rurale)[1]){
  for(j in 1:dim(PM10_data.stazione_rurale)[1]){
    if(is.na(PM10_data.stazione_rurale[j,i]))
      PM10_data.stazione_rurale[j,i] = Stazioni_mean_rurale[i]
  }
}

giorni=1:365
basis <- create.fourier.basis(rangeval=c(1,365),nbasis=12)
Xsp <- smooth.basis(argvals=giorni, y=as.matrix(PM10_data.stazione_rurale), fdParobj=basis)
rurale.fd <- Data2fd(y = as.matrix(PM10_data.stazione_rurale),argvals = giorni,basisobj = basis)

Xsp$fd$coefs

x11()
par(mfrow=c(1,2))
plot.fd(rurale.fd)
lines(mean(rurale.fd),lwd=3)
matplot(giorni,as.matrix(PM10_data.stazione_rurale),type="l")
matlines(Rurale_mean,lwd=3)

#interpolazione sulla media
Rurale_mean[365]=mean(Stazioni_mean_rurale)
Xsp.media <- smooth.basis(argvals=giorni, y=Rurale_mean, fdParobj=basis)
rurale.fd.media <- eval.fd(giorni, Xsp.media$fd)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,364), ylim=c(2,51), type="l",col = "red",lwd=2)
lines(Rurale_mean,type="l",lty=2)

#ggplot 
rurale.fd.media.ggplot = data.frame(as.Date(dates),rurale.fd.media)
colnames(rurale.fd.media.ggplot) = c("Date","Mean")
Rurale_mean.ggplot = data.frame(as.Date(dates),Rurale_mean)
colnames(Rurale_mean.ggplot) = c("Date","Mean")
colors <- c("Rural Mean" = "black","Interpolation" = "red")

x11()
ggplot(Rurale_mean.ggplot,aes(x=Date, y=Mean)) +
  geom_line(aes(color="Rural Mean"), size=0.4, alpha=0.9, linetype=2) +
  geom_line(data=rurale.fd.media.ggplot,aes(color="Interpolation"),size=1) +
  ggtitle("Fourier Basis of Rural PM10 Mean") +
  theme_ipsum() +
  labs(x = "Date",
       y = "PM10 Concentration",
       color = "Legend") +
  scale_color_manual(values = colors)




#suburbano
PM10_data.stazione_suburbano = PM10_data.stazione_suburbano[,-1]

#eliminazione NA
for(i in 1:dim(Suburbano)[1]){
  for(j in 1:dim(PM10_data.stazione_suburbano)[1]){
    if(is.na(PM10_data.stazione_suburbano[j,i]))
      PM10_data.stazione_suburbano[j,i] = Stazioni_mean_suburbano[i]
  }
}

giorni=1:365
basis <- create.fourier.basis(rangeval=c(1,365),nbasis=12)
Xsp <- smooth.basis(argvals=giorni, y=as.matrix(PM10_data.stazione_suburbano), fdParobj=basis)
suburbano.fd <- Data2fd(y = as.matrix(PM10_data.stazione_suburbano),argvals = giorni,basisobj = basis)

Xsp$fd$coefs

x11()
par(mfrow=c(1,2))
plot.fd(suburbano.fd)
lines(mean(suburbano.fd),lwd=3)
matplot(giorni,as.matrix(PM10_data.stazione_suburbano),type="l")
matlines(Suburbano_mean,lwd=3)

#interpolazione sulla media
Suburbano_mean[365]=mean(Stazioni_mean_suburbano)
Xsp.media <- smooth.basis(argvals=giorni, y=Suburbano_mean, fdParobj=basis)
suburbano.fd.media <- eval.fd(giorni, Xsp.media$fd)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,364), ylim=c(2,83), type="l",col = "red",lwd=2)
lines(Suburbano_mean,type="l",lty=2)

#ggplot 
suburbano.fd.media.ggplot = data.frame(as.Date(dates),suburbano.fd.media)
colnames(suburbano.fd.media.ggplot) = c("Date","Mean")
Suburbano_mean.ggplot = data.frame(as.Date(dates),Suburbano_mean)
colnames(Suburbano_mean.ggplot) = c("Date","Mean")
colors <- c("Suburban Mean" = "black","Interpolation" = "red")

x11()
ggplot(Suburbano_mean.ggplot,aes(x=Date, y=Mean)) +
  geom_line(aes(color="Suburban Mean"), size=0.4, alpha=0.9, linetype=2) +
  geom_line(data=suburbano.fd.media.ggplot,aes(color="Interpolation"),size=1) +
  ggtitle("Fourier Basis of Suburban PM10 Mean") +
  theme_ipsum() +
  labs(x = "Date",
       y = "PM10 Concentration",
       color = "Legend") +
  scale_color_manual(values = colors)




#urbano
PM10_data.stazione_urbano = PM10_data.stazione_urbano[,-1]

#eliminazione NA
for(i in 1:dim(Urbano)[1]){
  for(j in 1:dim(PM10_data.stazione_urbano)[1]){
    if(is.na(PM10_data.stazione_urbano[j,i]))
      PM10_data.stazione_urbano[j,i] = Stazioni_mean_urbano[i]
  }
}

giorni=1:365
basis <- create.fourier.basis(rangeval=c(1,365),nbasis=12)
Xsp <- smooth.basis(argvals=giorni, y=as.matrix(PM10_data.stazione_urbano), fdParobj=basis)
urbano.fd <- Data2fd(y = as.matrix(PM10_data.stazione_urbano),argvals = giorni,basisobj = basis)

Xsp$fd$coefs

x11()
par(mfrow=c(1,2))
plot.fd(urbano.fd)
lines(mean(urbano.fd),lwd=3)
matplot(giorni,as.matrix(PM10_data.stazione_urbano),type="l")
matlines(Urbano_mean,lwd=3)

#interpolazione sulla media
Urbano_mean[365]=mean(Stazioni_mean_urbano)
Xsp.media <- smooth.basis(argvals=giorni, y=Urbano_mean, fdParobj=basis)
urbano.fd.media <- eval.fd(giorni, Xsp.media$fd)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,364), ylim=c(2,83), type="l",col = "red",lwd=2)
lines(Urbano_mean,type="l",lty=2)

#ggplot 
urbano.fd.media.ggplot = data.frame(as.Date(dates),urbano.fd.media)
colnames(urbano.fd.media.ggplot) = c("Date","Mean")
Urbano_mean.ggplot = data.frame(as.Date(dates),Urbano_mean)
colnames(Urbano_mean.ggplot) = c("Date","Mean")
colors <- c("Urban Mean" = "black","Interpolation" = "red")

x11()
ggplot(Urbano_mean.ggplot,aes(x=Date, y=Mean)) +
  geom_line(aes(color="Urban Mean"), size=0.4, alpha=0.9, linetype=2) +
  geom_line(data=urbano.fd.media.ggplot,aes(color="Interpolation"),size=1) +
  ggtitle("Fourier Basis of Urban PM10 Mean") +
  theme_ipsum() +
  labs(x = "Date",
       y = "PM10 Concentration",
       color = "Legend") +
  scale_color_manual(values = colors)

# Comparison other years --------------------------------------------------

#2019
PM10_2019 = PM10[which(PM10$Anno==2019),c(2,3,4,8,9,11,13)]
colnames(PM10_2019) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2019$NS = as.factor(PM10_2019$NS)

Stazioni = as.factor(levels(PM10_2019$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2019$NS))
Dati_Stazioni[2] = PM10_2019$Provincia[pos]
Dati_Stazioni[3] = PM10_2019$Area[pos]
Dati_Stazioni[4] = PM10_2019$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2019-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2019[which(PM10_2019$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2019[which(PM10_2019$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2019[which(PM10_2019$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2019[which(PM10_2019$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale9 = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano9 = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano9 = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb9 = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean9 = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean9 = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean9 = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean9 = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

Rurale_mean9[365]=mean(Stazioni_mean_rurale9)
Suburbano_mean9[365]=mean(Stazioni_mean_suburbano9)
Urbano_mean9[365]=mean(Stazioni_mean_urbano9)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines(Rurale_mean9,type="l",lty=2)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines(Suburbano_mean9,type="l",lty=2)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines(Urbano_mean9,type="l",lty=2)




#2017
PM10_2017 = PM10[which(PM10$Anno==2017),c(2,3,4,8,9,11,13)]
colnames(PM10_2017) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2017$NS = as.factor(PM10_2017$NS)

Stazioni = as.factor(levels(PM10_2017$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2017$NS))
Dati_Stazioni[2] = PM10_2017$Provincia[pos]
Dati_Stazioni[3] = PM10_2017$Area[pos]
Dati_Stazioni[4] = PM10_2017$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2017-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2017[which(PM10_2017$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2017[which(PM10_2017$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2017[which(PM10_2017$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2017[which(PM10_2017$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale7 = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano7 = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano7 = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb7 = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean7 = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean7 = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean7 = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean7 = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

Rurale_mean7[365]=mean(Stazioni_mean_rurale7)
Suburbano_mean7[365]=mean(Stazioni_mean_suburbano7)
Urbano_mean7[365]=mean(Stazioni_mean_urbano7)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(2,88), type="l",col = "red",lwd=2)
lines(Rurale_mean7,type="l",lty=2)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,365), ylim=c(4,155), type="l",col = "red",lwd=2)
lines(Suburbano_mean7,type="l",lty=2)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,365), ylim=c(4,163), type="l",col = "red",lwd=2)
lines(Urbano_mean7,type="l",lty=2)



#2016
PM10_2016 = PM10[which(PM10$Anno==2016),c(2,3,4,8,9,11,13)]
colnames(PM10_2016) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2016$NS = as.factor(PM10_2016$NS)

Stazioni = as.factor(levels(PM10_2016$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2016$NS))
Dati_Stazioni[2] = PM10_2016$Provincia[pos]
Dati_Stazioni[3] = PM10_2016$Area[pos]
Dati_Stazioni[4] = PM10_2016$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2016-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2016[which(PM10_2016$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2016[which(PM10_2016$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2016[which(PM10_2016$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2016[which(PM10_2016$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale6 = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano6 = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano6 = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb6 = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean6 = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean6 = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean6 = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean6 = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

Rurale_mean6[365]=mean(Stazioni_mean_rurale6)
Suburbano_mean6[365]=mean(Stazioni_mean_suburbano6)
Urbano_mean6[365]=mean(Stazioni_mean_urbano6)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(2,88), type="l",col = "red",lwd=2)
lines(Rurale_mean6,type="l",lty=2)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,365), ylim=c(4,155), type="l",col = "red",lwd=2)
lines(Suburbano_mean6,type="l",lty=2)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,365), ylim=c(4,163), type="l",col = "red",lwd=2)
lines(Urbano_mean6,type="l",lty=2)



#2015
PM10_2015 = PM10[which(PM10$Anno==2015),c(2,3,4,8,9,11,13)]
colnames(PM10_2015) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2015$NS = as.factor(PM10_2015$NS)

Stazioni = as.factor(levels(PM10_2015$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2015$NS))
Dati_Stazioni[2] = PM10_2015$Provincia[pos]
Dati_Stazioni[3] = PM10_2015$Area[pos]
Dati_Stazioni[4] = PM10_2015$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2015-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2015[which(PM10_2015$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2015[which(PM10_2015$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2015[which(PM10_2015$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2015[which(PM10_2015$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale5 = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano5 = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano5 = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb5 = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean5 = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean5 = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean5 = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean5 = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

Rurale_mean5[365]=mean(Stazioni_mean_rurale5)
Suburbano_mean5[365]=mean(Stazioni_mean_suburbano5)
Urbano_mean5[365]=mean(Stazioni_mean_urbano5)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(2,88), type="l",col = "red",lwd=2)
lines(Rurale_mean5,type="l",lty=2)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,365), ylim=c(4,155), type="l",col = "red",lwd=2)
lines(Suburbano_mean5,type="l",lty=2)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,365), ylim=c(4,163), type="l",col = "red",lwd=2)
lines(Urbano_mean5,type="l",lty=2)



#2014
PM10_2014 = PM10[which(PM10$Anno==2014),c(2,3,4,8,9,11,13)]
colnames(PM10_2014) = c("Data","NS","Valore","Giorno","Provincia","Area","Quota")
PM10_2014$NS = as.factor(PM10_2014$NS)

Stazioni = as.factor(levels(PM10_2014$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_2014$NS))
Dati_Stazioni[2] = PM10_2014$Provincia[pos]
Dati_Stazioni[3] = PM10_2014$Area[pos]
Dati_Stazioni[4] = PM10_2014$Quota[pos]
colnames(Dati_Stazioni) = c("Stazioni","Provincia","Area","Quota")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="Rurale"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Suburbano"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="Urbano"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="Rurale"),]

dates = as.factor(seq.Date(as.Date("2014-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_2014[which(PM10_2014$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_2014[which(PM10_2014$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_2014[which(PM10_2014$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_2014[which(PM10_2014$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale4 = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano4 = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano4 = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb4 = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean4 = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean4 = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean4 = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean4 = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

Rurale_mean4[365]=mean(Stazioni_mean_rurale4)
Suburbano_mean4[365]=mean(Stazioni_mean_suburbano4)
Urbano_mean4[365]=mean(Stazioni_mean_urbano4)

x11()
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(2,88), type="l",col = "red",lwd=2)
lines(Rurale_mean4,type="l",lty=2)

x11()
plot(giorni, suburbano.fd.media, xlim=c(1,365), ylim=c(4,155), type="l",col = "red",lwd=2)
lines(Suburbano_mean4,type="l",lty=2)

x11()
plot(giorni, urbano.fd.media, xlim=c(1,365), ylim=c(4,163), type="l",col = "red",lwd=2)
lines(Urbano_mean4,type="l",lty=2)


# Comparison other regions ------------------------------------------------

#Lombardia
PM10_L <- read_excel("Data/all_PM10_lomb.xlsx")
PM10_L_2018=PM10_L[which(year(PM10_L$Data)==2018),c(2,3,15,27)]
PM10_L_2018$Data=as.Date(PM10_L_2018$Data)
colnames(PM10_L_2018) = c("Data","Valore","NS","Area")
PM10_L_2018$NS = as.factor(PM10_L_2018$NS)

Stazioni = as.factor(levels(PM10_L_2018$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_L_2018$NS))
Dati_Stazioni[2] = PM10_L_2018$Area[pos]
colnames(Dati_Stazioni) = c("Stazioni","Area")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="R"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="S"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="U"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="R"),]

dates = as.factor(seq.Date(as.Date("2018-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale_L = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano_L = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano_L = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb_L = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean_L = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean_L = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean_L = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean_L = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

x11()
par(mfrow=c(1,2))
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines(Rurale_mean_L,type="l",lty=2)
plot(giorni, (rurale.fd.media+5), xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines((Rurale_mean_L),type="l",lty=2)

x11()
plot(giorni,suburbano.fd.media, xlim=c(1,365), ylim=c(4,88), type="l",col = "red",lwd=2)
lines(Suburbano_mean_L,type="l",lty=2)

x11()
plot(giorni,urbano.fd.media, xlim=c(1,365), ylim=c(4,87), type="l",col = "red",lwd=2)
lines(Urbano_mean_L,type="l",lty=2)



#Piemonte
PM10_P <- read_excel("Data/all_PM10_lomb.xlsx")
PM10_P_2018=PM10_L[which(year(PM10_L$Data)==2018),c(2,3,15,27)]
PM10_P_2018$Data=as.Date(PM10_L_2018$Data)
colnames(PM10_P_2018) = c("Data","Valore","NS","Area")
PM10_P_2018$NS = as.factor(PM10_L_2018$NS)

Stazioni = as.factor(levels(PM10_L_2018$NS))
Dati_Stazioni = data.frame(Stazioni)
pos = match(Dati_Stazioni$Stazioni,as.factor(PM10_L_2018$NS))
Dati_Stazioni[2] = PM10_L_2018$Area[pos]
colnames(Dati_Stazioni) = c("Stazioni","Area")
Rurale = Dati_Stazioni[which(Dati_Stazioni$Area=="R"),]
Suburbano = Dati_Stazioni[which(Dati_Stazioni$Area=="S"),]
Urbano = Dati_Stazioni[which(Dati_Stazioni$Area=="U"),] 
Sub_Urb = Dati_Stazioni[which(Dati_Stazioni$Area!="R"),]

dates = as.factor(seq.Date(as.Date("2018-01-01"), length.out = 365, by = "day"))
PM10_data.stazione_rurale = as.data.frame(dates)
for(i in 2:(dim(Rurale)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Rurale$Stazioni[i-1]),] 
  Totale_mean_rurale = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_rurale$dates,as.factor(v_s$Data))
  PM10_data.stazione_rurale[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_rurale) = c("Date",as.character(Rurale$Stazioni))
PM10_data.stazione_rurale$Date = as.Date(PM10_data.stazione_rurale$Date)

PM10_data.stazione_suburbano = as.data.frame(dates)
for(i in 2:(dim(Suburbano)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Suburbano$Stazioni[i-1]),] 
  Totale_mean_suburbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_suburbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_suburbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_suburbano) = c("Date",as.character(Suburbano$Stazioni))
PM10_data.stazione_suburbano$Date = as.Date(PM10_data.stazione_suburbano$Date)

PM10_data.stazione_urbano = as.data.frame(dates)
for(i in 2:(dim(Urbano)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Urbano$Stazioni[i-1]),] 
  Totale_mean_urbano = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_urbano$dates,as.factor(v_s$Data))
  PM10_data.stazione_urbano[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_urbano) = c("Date",as.character(Urbano$Stazioni))
PM10_data.stazione_urbano$Date = as.Date(PM10_data.stazione_urbano$Date)

PM10_data.stazione_sub_urb = as.data.frame(dates)
for(i in 2:(dim(Sub_Urb)[1]+1)){
  v_s = PM10_L_2018[which(PM10_L_2018$NS==Sub_Urb$Stazioni[i-1]),] 
  Totale_mean_sub_urb = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione_sub_urb$dates,as.factor(v_s$Data))
  PM10_data.stazione_sub_urb[i] = v_s$Valore[pos]
}
colnames(PM10_data.stazione_sub_urb) = c("Date",as.character(Sub_Urb$Stazioni))
PM10_data.stazione_sub_urb$Date = as.Date(PM10_data.stazione_sub_urb$Date)

Stazioni_mean_rurale_L = colMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Stazioni_mean_suburbano_L = colMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Stazioni_mean_urbano_L = colMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Stazioni_mean_sub_urb_L = colMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)
Rurale_mean_L = rowMeans(PM10_data.stazione_rurale[,-1],na.rm = TRUE)
Suburbano_mean_L = rowMeans(PM10_data.stazione_suburbano[,-1],na.rm = TRUE)
Urbano_mean_L = rowMeans(PM10_data.stazione_urbano[,-1],na.rm = TRUE)
Sub_Urb_mean_L = rowMeans(PM10_data.stazione_sub_urb[,-1],na.rm = TRUE)

x11()
par(mfrow=c(1,2))
plot(giorni, rurale.fd.media, xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines(Rurale_mean_L,type="l",lty=2)
plot(giorni, (rurale.fd.media+5), xlim=c(1,365), ylim=c(4,79), type="l",col = "red",lwd=2)
lines((Rurale_mean_L),type="l",lty=2)

x11()
plot(giorni,suburbano.fd.media, xlim=c(1,365), ylim=c(4,88), type="l",col = "red",lwd=2)
lines(Suburbano_mean_L,type="l",lty=2)

x11()
plot(giorni,urbano.fd.media, xlim=c(1,365), ylim=c(4,87), type="l",col = "red",lwd=2)
lines(Urbano_mean_L,type="l",lty=2)


