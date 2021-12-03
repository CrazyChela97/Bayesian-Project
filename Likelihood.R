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
  K=p[4]
  
  data = as.Date(xdata, origin = "1970-01-01")
  #Seasonality_short = gamma *(weekdays(data) %in% c("luned?","marted?", "mercoled?","domenica")) + (7-4*gamma)/3 * (weekdays(data) %in% c("gioved?", "venerd?", "sabato"))
  Seasonality_short = 1  # La seasonality short non ? molto esplicativa
  numeric_date=as.POSIXlt(data, format="%m/%d/%Y")$yday  # returns the number of the day in the year
  Seasonality_long =  K+A*cos(omega*numeric_date + phi)
  
  Seasonality_short * Seasonality_long
}

# FITTING
  
# rural
rural_fit = lsqcurvefit(Seasonality, p0=c(1,1,0,mean(data_rural$PM10)), xdata=as.numeric(mean_rural$Date), ydata=mean_rural$MeanValue)
rural_fit$x; rural_fit$ssq
# suburban
suburban_fit = lsqcurvefit(Seasonality, p0=c(1,1,0,mean(data_suburban$PM10)), xdata=as.numeric(mean_suburban$Date), ydata=mean_suburban$MeanValue)
suburban_fit$x
# urban
urban_fit = lsqcurvefit(Seasonality, p0=c(1,1,0,mean(data_urban$PM10)), xdata=as.numeric(mean_urban$Date), ydata=mean_urban$MeanValue)
urban_fit$x

# PLOTS

# Plot of the values of PM10 divided by zone type and plot of the fitted value 
ra_fit <- ggplot(data_rural,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[1:12]) +
  geom_line(data = mean_rural, aes(x=Date, y=Seasonality(rural_fit$x,Date)), col = "darkred", size=1) +
  ylim(c(0,124)) +
  labs(title="PM10-Emilia: RURAL AREA") +
  theme(legend.position="none") 

sa_fit <- ggplot(data_suburban,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[13:26]) +
  geom_line(data = mean_suburban, aes(x=Date, y=Seasonality(suburban_fit$x,Date)), col = "darkred", size=1) +
  ylim(c(0,124)) +
  labs(title="PM10-Emilia: SUBURBAN AREA") +
  theme(legend.position="none") 

ua_fit <- ggplot(data_urban,aes(x=Date, y=PM10, col=Station)) +
  geom_line() + 
  scale_color_manual(values=terrain.colors(50)[27:49]) + 
  geom_line(data = mean_urban, aes(x=Date, y=Seasonality(urban_fit$x,Date)), col = "darkred", size=1) +
  labs(title="PM10-Emilia: URBAN AREA") +
  theme(legend.position="none") 


grid.arrange(ra_fit, sa_fit, ua_fit, ncol=3)



# Study of Gaussianity ----------------------------------------------------
Shapiro=matrix(data=NA,nrow=2,ncol=3) # in the first row are stored the statistics of each variables, in the second row the p-values
Shapiro_trans = matrix(data=NA,nrow=2,ncol=3)
# RURAL
  data_rural$Residual_value = data_rural$PM10 - Seasonality(rural_fit$x, data_rural$Date)
  
  # PLOTS BEFORE TRASFORMATION
  # Histogram
  hist(data_rural$Residual_value, main="Histogram")
  # normal QQ plot
  qqnorm(data_rural$Residual_value, main="QQ norm")
  qqline(data_rural$Residual_value)
  # BoxPlot
  boxplot(data_rural$Residual_value,main="BoxPlot")
  
  test=shapiro.test(data_rural$Residual_value)
  print(test)
  # storing the results of the Shapiro test
  Shapiro[1,1]=test$statistic
  Shapiro[2,1]=test$p.value
  
  
  # BoxCox power trasformation
  intermediate=powerTransform(data_rural$Residual_value~1,family = "bcnPower")
  print(intermediate$lambda)
  data_rural$Residual_value_trans=bcnPower(data_rural$Residual_value,intermediate$lambda,gamma=intermediate$gamma)
  
  # PLOTS AFTER TRASFORMATION
  # Histogram
  hist(data_rural$Residual_value_trans, main="Histogram")
  # normal QQ plot
  qqnorm(data_rural$Residual_value_trans, main="QQ norm")
  qqline(data_rural$Residual_value_trans)
  # BoxPlot
  boxplot(data_rural$Residual_value_trans,main="BoxPlot")
  
  test=shapiro.test(data_rural$Residual_value_trans)
  print(test)
  # storing the results of the Shapiro test
  Shapiro_trans[1,1]=test$statistic
  Shapiro_trans[2,1]=test$p.value

  
# BOX-COX TRANSFORMATION : different parameters ------------------------------------

# --- RURAL ---
Date = unique(data_rural$Date)
f_t_rural = Seasonality(rural_fit$x,Date)

dati_per_stazione = xtabs(PM10 ~ Date + Station, data=data_rural)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
f_t_mat = t(repmat(f_t_rural,12,1))
dati_norm = dati_per_stazione - f_t_mat

matplot(dati_per_stazione, type='l')
matplot(dati_norm, type='l') # non c'è abbastanza detrend

# BOX-COX TRANSFORMATION for each station
BC_data = matrix(data=NA, nrow=dim(dati_norm)[1], ncol=dim(dati_norm)[2])
parameters = cbind(rep(0, dim(dati_norm)[2]), rep(0, dim(dati_norm)[2]))
colnames(parameters) = c('Lambda', 'Gamma')
for (i in 1:dim(dati_norm)[2]){
  intermediate = powerTransform(dati_norm[ ,i]~1, family = "bcnPower")
  parameters[i, 1] = intermediate$lambda
  parameters[i, 2] = intermediate$gamma
  BC_data[,i] = bcnPower(dati_norm[ ,i], intermediate$lambda, gamma=intermediate$gamma)
}
matplot(BC_data, type='l') 

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0,12)
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_norm)[2]){
  shapiro_BC[i] = shapiro.test(BC_data[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(BC_data[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_norm)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(BC_data[,i], main="QQ norm", col=norm_col)
  qqline(BC_data[,i])
}
# SUMMARY : 8 normal
#           4 not-normal
dev.off()



# --- URBAN ---
Date = unique(data_urban$Date)
f_t_urban = Seasonality(urban_fit$x,Date)

dati_per_stazione = xtabs(PM10 ~ Date + Station, data=data_urban)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
f_t_mat = t(repmat(f_t_urban, dim(data_urban)[2],1))
dati_norm = dati_per_stazione - f_t_mat

matplot(dati_per_stazione, type='l')
matplot(dati_norm, type='l') # non abbastanza detrend

# BOX-COX TRANSFORMATION for each station
BC_data = matrix(data=NA, nrow=dim(dati_norm)[1], ncol=dim(dati_norm)[2])
parameters = cbind(rep(0, dim(dati_norm)[2]), rep(0, dim(dati_norm)[2]))
colnames(parameters) = c('Lambda', 'Gamma')
for (i in 1:dim(dati_norm)[2]){
  intermediate = powerTransform(dati_norm[ ,i]~1, family = "bcnPower")
  parameters[i, 1] = intermediate$lambda
  parameters[i, 2] = intermediate$gamma
  BC_data[,i] = bcnPower(dati_norm[ ,i], intermediate$lambda, gamma=intermediate$gamma)
}
matplot(BC_data, type='l') 

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0, dim(dati_norm)[2])
x11()
par(mfrow=c(5,5))
for (i in 1:dim(dati_norm)[2]){
  shapiro_BC[i] = shapiro.test(BC_data[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(BC_data[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(5,5))
for (i in 1:dim(dati_norm)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(BC_data[,i], main="QQ norm", col=norm_col)
  qqline(BC_data[,i])
}
# SUMMARY : tutti normali
dev.off()



# --- SUBURBAN ---
Date = unique(data_suburban$Date)
f_t_urban = Seasonality(suburban_fit$x,Date)

dati_per_stazione = xtabs(PM10 ~ Date + Station, data=data_suburban)
dati_per_stazione = as.data.frame.matrix(dati_per_stazione)
staz = which(colSums(dati_per_stazione) != 0)
dati_per_stazione = dati_per_stazione[,staz]
f_t_mat = t(repmat(f_t_urban, dim(data_suburban)[2],1))
dati_norm = dati_per_stazione - f_t_mat

matplot(dati_per_stazione, type='l')
matplot(dati_norm, type='l') # non abbastanza detrend

# BOX-COX TRANSFORMATION for each station
BC_data = matrix(data=NA, nrow=dim(dati_norm)[1], ncol=dim(dati_norm)[2])
parameters = cbind(rep(0, dim(dati_norm)[2]), rep(0, dim(dati_norm)[2]))
colnames(parameters) = c('Lambda', 'Gamma')
for (i in 1:dim(dati_norm)[2]){
  intermediate = powerTransform(dati_norm[ ,i]~1, family = "bcnPower")
  parameters[i, 1] = intermediate$lambda
  parameters[i, 2] = intermediate$gamma
  BC_data[,i] = bcnPower(dati_norm[ ,i], intermediate$lambda, gamma=intermediate$gamma)
}
matplot(BC_data, type='l')

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0, dim(dati_norm)[2])
x11()
par(mfrow=c(4,4))
for (i in 1:dim(dati_norm)[2]){
  shapiro_BC[i] = shapiro.test(BC_data[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(BC_data[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(4,4))
for (i in 1:dim(dati_norm)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(BC_data[,i], main="QQ norm", col=norm_col)
  qqline(BC_data[,i])
}
# SUMMARY : tutti normali
dev.off()



# BOX-COX TRANSFORMATION : same parameter ---------------------------------

# --- RURAL ---
data_rural$residuals = data_rural$PM10 - Seasonality(rural_fit$x, data_rural$Date)
# BOX-COX TRANSFORMATION unica per tutte le stazioni
intermediate = powerTransform(data_rural$residuals~1, family = "bcnPower")
data_rural$res_BC = bcnPower(data_rural$residuals, intermediate$lambda, gamma=intermediate$gamma)

dati_BC = xtabs(res_BC ~ Date + Station, data=data_rural)
dati_BC = as.data.frame.matrix(dati_BC)
staz = which(colSums(dati_BC) != 0)
dati_BC = dati_BC[,staz]

matplot(dati_BC, type='l') 

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0, dim(dati_BC)[2])
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(3,4))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main="QQ norm", col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 5 normal
#           7 not-normal
dev.off()



# --- SUBURBAN ---
data_suburban$residuals = data_suburban$PM10 - Seasonality(suburban_fit$x, data_suburban$Date)
# BOX-COX TRANSFORMATION unica per tutte le stazioni
intermediate = powerTransform(data_suburban$residuals~1, family = "bcnPower")
data_suburban$res_BC = bcnPower(data_suburban$residuals, intermediate$lambda, gamma=intermediate$gamma)

dati_BC = xtabs(res_BC ~ Date + Station, data=data_suburban)
dati_BC = as.data.frame.matrix(dati_BC)
staz = which(colSums(dati_BC) != 0)
dati_BC = dati_BC[,staz]

matplot(dati_BC, type='l') 

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0, dim(dati_BC)[2])
x11()
par(mfrow=c(4,4))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(4,4))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main="QQ norm", col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 9 normal
#           5 not-normal
dev.off()



# --- URBAN ---
data_urban$residuals = data_urban$PM10 - Seasonality(urban_fit$x, data_urban$Date)
# BOX-COX TRANSFORMATION unica per tutte le stazioni
intermediate = powerTransform(data_urban$residuals~1, family = "bcnPower")
data_urban$res_BC = bcnPower(data_urban$residuals, intermediate$lambda, gamma=intermediate$gamma)

dati_BC = xtabs(res_BC ~ Date + Station, data=data_urban)
dati_BC = as.data.frame.matrix(dati_BC)
staz = which(colSums(dati_BC) != 0)
dati_BC = dati_BC[,staz]

matplot(dati_BC, type='l') 

# NORMALITY CHECK
# Shapiro test + histogram
shapiro_BC = rep(0, dim(dati_BC)[2])
x11()
par(mfrow=c(5,5))
for (i in 1:dim(dati_BC)[2]){
  shapiro_BC[i] = shapiro.test(dati_BC[,i])$p.value
  norm_col = (shapiro_BC[i] > 0.05) +2
  hist(dati_BC[,i], col=norm_col)
}
as.data.frame(shapiro_BC)
# QQ-Plot
x11()
par(mfrow=c(5,5))
for (i in 1:dim(dati_BC)[2]){
  norm_col = (shapiro_BC[i] > 0.05) +2
  qqnorm(dati_BC[,i], main="QQ norm", col=norm_col)
  qqline(dati_BC[,i])
}
# SUMMARY : 17 normal
#           6 not-normal
dev.off()
