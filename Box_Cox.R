###########################################################################
#                         MODELLO EMILIA 2018                             #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------

library(readr)
library(ggplot2)
require(gridExtra)
library(reshape2)
library(pracma)
library(lubridate)
library(car) 

# Load data + BC transformation ---------------------------------------------------------------
PM10 = read_csv('Data/PM10_Emilia.csv')

PM10_2018 = PM10[which(PM10$Anno==2018),c(2,3,4,9,10,11,12)]
colnames(PM10_2018) = c("Data","NS","Valore","Provincia","Tipo","Area","Zonizzazione")
PM10_2018$NS = as.factor(PM10_2018$NS)

Stazioni = as.factor(levels(PM10_2018$NS))
dates = as.factor(levels(as.factor(PM10_2018$Data)))
PM10_data.stazione = as.data.frame(dates)
for(i in 2:(length(Stazioni)+1)){
  v_s = PM10_2018[which(PM10_2018$NS==Stazioni[i-1]),] 
  Stazioni_mean = mean(v_s$Valore,na.rm = TRUE)
  pos = match(PM10_data.stazione$dates,as.factor(v_s$Data))
  PM10_data.stazione[i] = v_s$Valore[pos]
}

colnames(PM10_data.stazione) = c("Date",as.character(Stazioni))
PM10_data.stazione$Date = as.Date(PM10_data.stazione$Date)

Stazioni_mean = NULL
Stazioni_mean = colMeans(as.matrix(PM10_data.stazione[,-1]), na.rm = TRUE)

data_long = melt(PM10_data.stazione,id.vars= "Date")
for(i in 1:length(Stazioni)){
  for(j in 1:dim(data_long)[1]){
    if(is.na(data_long$value[j]))
      data_long$value[j] = Stazioni_mean[i]
  }
}
colnames(data_long) = c("Date","Station","PM10")

PM10$Area = as.factor(PM10$Area)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Zona = PM10$Area[pos]
data_long$Zona = factor(data_long$Zona, levels = c("Rurale", "Suburbano", "Urbano"))

# Transformed Data
intermediate = powerTransform(data_long$PM10 ~ 1, family = "bcnPower")
intermediate$lambda # circa 0 ~ log
data_BC = bcnPower(data_long$PM10, intermediate$lambda, gamma=intermediate$gamma)
data_long$BC_trans = data_BC

# rurale
data_rural = data_long[which(data_long$Zona=="Rurale"),]
mean_rural = aggregate(data_rural$BC_trans, FUN=mean, by=list(data_rural$Date))
colnames(mean_rural) = c("Date", "MeanValue")
# urbano + suburbano
data_us = data_long[which(data_long$Zona=="Suburbano" | data_long$Zona=="Urbano"),]
mean_us = aggregate(data_us$BC_trans, FUN=mean, by=list(data_us$Date))
colnames(mean_us) = c("Date", "MeanValue")




# SEASONALITY FUNCTION ( 1 basis ) ----------------------------------------

omega = 2*pi/365;  # fixed period of 1 year

Seasonality =  function(p,xdata){
  A=p[1]
  phi=p[2]
  K=p[3]
  
  data = as.Date(xdata, origin = "1970-01-01")
  Seasonality_short = 1  # La seasonality short non ? molto esplicativa
  numeric_date=as.POSIXlt(data, format="%m/%d/%Y")$yday  # returns the number of the day in the year
  Seasonality_long =  K+A*cos(omega*numeric_date + phi)
  
  Seasonality_short * Seasonality_long
}


# FITTING
# rural
rural_fit = lsqcurvefit(Seasonality, p0=c(1,1,0,mean(data_rural$BC_trans)), xdata=as.numeric(mean_rural$Date), ydata=mean_rural$MeanValue)
rural_fit$x; rural_fit$ssq
# urban + suburban 
us_fit = lsqcurvefit(Seasonality, p0=c(1,1,0,mean(data_us$BC_trans)), xdata=as.numeric(mean_us$Date), ydata=mean_us$MeanValue)
us_fit$x


# PLOTS
# Plot of the values of PM10 divided by zone type and plot of the fitted value 
ra_fit <- ggplot(data_rural,aes(x=Date, y=BC_trans, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[1:12]) +
  geom_line(data = mean_rural, aes(x=Date, y=Seasonality(rural_fit$x,Date)), col = "darkred", size=1) +
  ylim(c(0,6)) +
  labs(title="PM10-Emilia: RURAL AREA") +
  theme(legend.position="none") 

usa_fit <- ggplot(data_us,aes(x=Date, y=BC_trans, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[13:50]) +
  geom_line(data = mean_us, aes(x=Date, y=Seasonality(us_fit$x,Date)), col = "darkred", size=1) +
  ylim(c(0,6)) +
  labs(title="PM10-Emilia: SUBURBAN & URBAN AREA") +
  theme(legend.position="none") 

grid.arrange(ra_fit, usa_fit, ncol=2)


# RESIDUALS ANALYSIS
trans_data = PM10[which(PM10$Anno==2018), -c(1,5,6,7,8)]
trans_data$Valore = bcnPower(trans_data$Valore, intermediate$lambda, gamma=intermediate$gamma)

# # # RURAL DATA # # #
trans_rural = trans_data[trans_data$Area == 'Rurale', ]
ft = Seasonality(rural_fit$x, trans_rural$Data)
trans_rural$Res = trans_rural$Valore - ft

model = lm(Res ~ Tipo+Zonizzazione+Quota+Provincia, data=trans_rural)
summary(model)

model = lm(Valore ~ ft+Tipo+Zonizzazione+Quota+Provincia, data=trans_rural)
summary(model)
# tutti significativi


# ANALISI DATI PER STAZIONE
dati_per_stazione = xtabs(Valore ~ Data + NomeStazione, data=trans_rural)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
# tolgo zeri dati da xtabs
for(i in 1:dim(dati_per_stazione)[2]){
  for(j in 1:dim(dati_per_stazione)[1]){
    if(dati_per_stazione[j,i] == 0)
      dati_per_stazione[j,i] = colMeans(dati_per_stazione)[i]
  }
}

matplot(dati_per_stazione, type='l') 

# NORMALITY CHECK : Transformed Data
# Shapiro test + histogram
dati_BC= dati_per_stazione
shapiro_BC = rep(0,dim(dati_BC)[2])
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col, main = colnames(dati_BC)[i], sub= shapiro_BC[i], col.sub=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main = colnames(dati_BC)[i], col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 3 normal
#           9 not-normal
dev.off()


# ANALISI RESIDUI PER STAZIONE
dati_per_stazione = xtabs(Res ~ Data + NomeStazione, data=trans_rural)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
# tolgo zeri dati da xtabs
for(i in 1:dim(dati_per_stazione)[2]){
  for(j in 1:dim(dati_per_stazione)[1]){
    if(dati_per_stazione[j,i] == 0)
      dati_per_stazione[j,i] = colMeans(dati_per_stazione)[i]
  }
}

matplot(dati_per_stazione, type='l') 

# NORMALITY CHECK : Residuals
# Shapiro test + histogram
dati_BC= dati_per_stazione
shapiro_BC = rep(0,dim(dati_BC)[2])
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col, main = colnames(dati_BC)[i], sub= shapiro_BC[i], col.sub=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main = colnames(dati_BC)[i], col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 6 normal
#           6 not-normal
dev.off()




# # # URBAN & SUBURBAN DATA # # #
trans_us = trans_data[which(trans_data$Area == 'Urbano' | trans_data$Area == 'Suburbano'), ]
ft = Seasonality(us_fit$x, trans_us$Data)
trans_us$Res = trans_us$Valore - ft

model = lm(Res ~ Tipo+Zonizzazione+Quota+Provincia, data=trans_us)
summary(model)

model = lm(Valore ~ ft+Tipo+Zonizzazione+Quota+Provincia, data=trans_us)
summary(model)
# tutti significativi


# ANALISI DATI PER STAZIONE
dati_per_stazione = xtabs(Valore ~ Data + NomeStazione, data=trans_us)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
# tolgo zeri dati da xtabs
for(i in 1:dim(dati_per_stazione)[2]){
  for(j in 1:dim(dati_per_stazione)[1]){
    if(dati_per_stazione[j,i] == 0)
      dati_per_stazione[j,i] = colMeans(dati_per_stazione)[i]
  }
}

matplot(dati_per_stazione, type='l') 

# NORMALITY CHECK : Transformed Data
# Shapiro test + histogram
dati_BC= dati_per_stazione
shapiro_BC = rep(0,dim(dati_BC)[2])
x11()
par(mfrow=c(6,7))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col, main = colnames(dati_BC)[i], sub= shapiro_BC[i], col.sub=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(6,7))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main = colnames(dati_BC)[i], col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 13 normal
#           24 not-normal
dev.off()


# ANALISI RESIDUI PER STAZIONE
dati_per_stazione = xtabs(Res ~ Data + NomeStazione, data=trans_us)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
# tolgo zeri dati da xtabs
for(i in 1:dim(dati_per_stazione)[2]){
  for(j in 1:dim(dati_per_stazione)[1]){
    if(dati_per_stazione[j,i] == 0)
      dati_per_stazione[j,i] = colMeans(dati_per_stazione)[i]
  }
}

matplot(dati_per_stazione, type='l') 

# NORMALITY CHECK : Residuals
# Shapiro test + histogram
dati_BC= dati_per_stazione
shapiro_BC = rep(0,dim(dati_BC)[2])
x11()
par(mfrow=c(6,7))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col, main = colnames(dati_BC)[i], sub= shapiro_BC[i], col.sub=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(6,7))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main = colnames(dati_BC)[i], col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 4 normal
#           33 not-normal
dev.off()


# FOURIER FITTING ( 4 basis ) -------------------------------------------


# FOURIER FITTING ( 8 basis) ----------------------------------------------


