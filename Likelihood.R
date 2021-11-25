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

# Load data ---------------------------------------------------------------
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

data_long$Days = weekdays(data_long$Date)
data_long$Week = week(data_long$Date)

PM10$Area = as.factor(PM10$Area)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Zona = PM10$Area[pos]
data_long$Zona = factor(data_long$Zona, levels = c("Rurale", "Suburbano", "Urbano"))

data_rural = data_long[which(data_long$Zona=="Rurale"),]
mean_rural = aggregate(data_rural$PM10, FUN=mean, by=list(data_rural$Date))
colnames(mean_rural) = c("Date", "MeanValue")
mean_rural$Days = weekdays(mean_rural$Date)
mean_rural$Week = week(mean_rural$Date)

data_suburban = data_long[which(data_long$Zona=="Suburbano"),]
mean_suburban = aggregate(data_suburban$PM10, FUN=mean, by=list(data_suburban$Date))
colnames(mean_suburban) = c("Date", "MeanValue")

data_urban = data_long[which(data_long$Zona=="Urbano"),]
mean_urban = aggregate(data_urban$PM10, FUN=mean, by=list(data_urban$Date))
colnames(mean_urban) = c("Date", "MeanValue")



# Plots -------------------------------------------------------------------

# Plot of the values of PM10 divided by zone type and plot of mean value 
  ra <- ggplot(data_rural,aes(x=Date, y=PM10, col=Station)) +
    geom_line() + 
    scale_color_manual(values=terrain.colors(50)[1:12]) +
    geom_line(data = mean_rural, aes(x=Date, y=MeanValue), col = "darkred", size=1) +
    ylim(c(0,124)) +
    labs(title="PM10-Emilia: RURAL AREA") +
    theme(legend.position="none") 
  
  sa <- ggplot(data_suburban,aes(x=Date, y=PM10, col=Station)) +
    geom_line() + 
    scale_color_manual(values=terrain.colors(50)[13:26]) +
    geom_line(data = mean_rural, aes(x=Date, y=MeanValue), col = "darkred", size=1) +
    ylim(c(0,124)) +
    labs(title="PM10-Emilia: SUBURBAN AREA") +
    theme(legend.position="none") 
  
  ua <- ggplot(data_urban,aes(x=Date, y=PM10, col=Station)) +
    geom_line() + 
    scale_color_manual(values=terrain.colors(50)[27:49]) + 
    geom_line(data = mean_urban, aes(x=Date, y=MeanValue), col = "darkred", size=1) +
    labs(title="PM10-Emilia: URBAN AREA") +
    theme(legend.position="none") 
  
  x11()
  grid.arrange(ra, sa, ua, ncol=3)

# Comparison on mean values
  plot(mean_rural$Date, mean_rural$MeanValue, type="l", col="red", lwd=2, xlab="date", ylab="average PM10 value", ylim=c(0,90 ))
  lines(mean_suburban$Date, mean_suburban$MeanValue, col="blue", lwd=2)
  lines(mean_urban$Date, mean_urban$MeanValue, col="green", lwd=2)
  title("Mean comparison by zone type")
  legend(x=mean(mean_rural$Date), y=max(mean_urban$MeanValue), legend=c("rural","suburban","urban"), col=c("red","blue","green"),lty=1)

# Comparison by weekday
ggplot(mean_rural,aes(x=Days, y=MeanValue, group=Week, color=Week) )+ 
    geom_line() + 
    scale_color_gradientn(colours = rainbow(52))+
    ylim(c(0,60)) +
    labs(title="PM10-Emilia: RURAL AREA") +
    theme(legend.position="none") 

# numerical comparison of weekdays

mean_by_day_rural = aggregate(mean_rural$MeanValue, FUN=mean, by=list(mean_rural$Days))
mean_by_day_urban = aggregate(mean_urban$MeanValue, FUN=mean, by=list(weekdays(mean_urban$Date)))
mean_by_day_suburban = aggregate(mean_suburban$MeanValue, FUN=mean, by=list(weekdays(mean_suburban$Date)))
mean_by_day = merge(mean_by_day_rural, mean_by_day_suburban, by="Group.1")
mean_by_day = merge(mean_by_day, mean_by_day_urban, by="Group.1")
colnames(mean_by_day)=c("Weekdays","rural","suburban","urban")
# ordering by weekdays
mean_by_day$Weekdays<- factor(mean_by_day$Weekdays, levels=c("luned?","marted?", "mercoled?", "gioved?", "venerd?", "sabato","domenica"))
mean_by_day<-mean_by_day[order(mean_by_day$Weekdays),]

mean_by_day



# Seasonality fitting -----------------------------------------------------

omega = 2*pi/365;  # fixed period of 1 year

Seasonality =  function(p,xdata){
  gamma=p[1]
  A=p[2]
  phi=p[3]
  
  data = as.Date(xdata, origin = "1970-01-01")
  Seasonality_short = gamma *(weekdays(data) %in% c("luned?","marted?", "mercoled?","domenica")) + (7-4*gamma)/3 * (weekdays(data) %in% c("gioved?", "venerd?", "sabato"))
  
  numeric_date=as.POSIXlt(data, format="%m/%d/%Y")$yday  # returns the number of the day in the year
  Seasonality_long =  1+A*cos(omega*numeric_date + phi)
  
  Seasonality_short * Seasonality_long
}

# FITTING
  
# rural
rural_fit = lsqcurvefit(Seasonality, p0=c(1,1,0), xdata=as.numeric(mean_rural$Date), ydata=mean_rural$MeanValue)
rural_fit$x; rural_fit$ssq
# suburban
suburban_fit = lsqcurvefit(Seasonality, p0=c(1,1,0), xdata=as.numeric(mean_suburban$Date), ydata=mean_suburban$MeanValue)
suburban_fit$x
# urban
urban_fit = lsqcurvefit(Seasonality, p0=c(1,1,0), xdata=as.numeric(mean_urban$Date), ydata=mean_urban$MeanValue)
urban_fit$x

# PLOTS

# Plot of the values of PM10 divided by zone type and plot of the fitted value 
ra_fit <- ggplot(data_rural,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[1:12]) +
  geom_line(data = mean_rural, aes(x=Date, y=Seasonality(rural_fit$x,Date)+20), col = "darkred", size=1) +
  ylim(c(0,124)) +
  labs(title="PM10-Emilia: RURAL AREA") +
  theme(legend.position="none") 

sa_fit <- ggplot(data_suburban,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[13:26]) +
  geom_line(data = mean_suburban, aes(x=Date, y=Seasonality(suburban_fit$x,Date)+mean(data_suburban$PM10)), col = "darkred", size=1) +
  ylim(c(0,124)) +
  labs(title="PM10-Emilia: SUBURBAN AREA") +
  theme(legend.position="none") 

ua_fit <- ggplot(data_urban,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[27:49]) + 
  geom_line(data = mean_urban, aes(x=Date, y=Seasonality(urban_fit$x,Date)+mean(data_urban$PM10)), col = "darkred", size=1) +
  labs(title="PM10-Emilia: URBAN AREA") +
  theme(legend.position="none") 


grid.arrange(ra_fit, sa_fit, ua_fit, ncol=3)



# Normality test -----------------------------------------------------
# SEASONALITY FUNCTION
# tolgo short seasonality perchè non è esplicativa
Seasonality =  function(p,xdata){
  gamma=p[1]
  A=p[2]
  phi=p[3]
  
  data = as.Date(xdata, origin = "1970-01-01")
  Seasonality_short = gamma *(weekdays(data) %in% c("luned?","marted?", "mercoled?","domenica")) + (7-4*gamma)/3 * (weekdays(data) %in% c("gioved?", "venerd?", "sabato"))
  
  numeric_date=as.POSIXlt(data, format="%m/%d/%Y")$yday  # returns the number of the day in the year
  Seasonality_long =  1+A*cos(omega*numeric_date + phi)
  
  Seasonality_long
}

rural_fit = lsqcurvefit(Seasonality, p0=c(1,1,0), xdata=as.numeric(mean_rural$Date), ydata=mean_rural$MeanValue)
rural_fit$x; rural_fit$ssq

Date = unique(data_rural$Date)
f_t_rural = Seasonality(rural_fit$x,Date)+20

dati_per_stazione = xtabs(PM10 ~ Date + Station, data=data_rural)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
f_t_mat = t(repmat(f_t_rural,12,1))
dati_norm = dati_per_stazione - f_t_mat

matplot(dati_per_stazione, type='l')
matplot(dati_norm, type='l')
# anche togliendo f(t) non vengono detrendizzati i dati
# -> bisogna trovare funzione più specifica

# FOURIER ?



