###########################################################################
#               STATISTICA DESCRITTIVA EMILIA 2018                        #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------

library(readr)
library(readxl)
library(dygraphs)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
require(gridExtra)
library(plotly)
library(lubridate)
library(reshape2)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
library(wesanderson)
library(ggridges)
library(viridis)

# Dataset Creation --------------------------------------------------------

PM10 = read_csv('Data/PM10_Emilia.csv')
View(PM10)

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
View(PM10_data.stazione)
#creo questo dataset perchè mi permette di creare dei NA in tutte le date
#non registrate per una determinata stazione, a livello grafico viene 
#meglio rispetto all'utilizzare PM10_2018. Infatti con quest'ultimo
#compaiono righe orizzontali nel plot finale

# Mean and other values ---------------------------------------------------

Range_val = range(PM10_data.stazione[,-1], na.rm = TRUE)
Total_mean = mean(as.matrix(PM10_data.stazione[,-1]), na.rm = TRUE)
Stazioni_mean = NULL
Stazioni_mean = colMeans(as.matrix(PM10_data.stazione[,-1]), na.rm = TRUE)
Giorni_mean = rowMeans(as.matrix(PM10_data.stazione[,-1]), na.rm = TRUE)
Giorni_mean_dataset = as.data.frame(dates)
Giorni_mean_dataset[,2] = Giorni_mean
Giorni_mean_dataset[,1] = as.Date(Giorni_mean_dataset[,1])
colnames(Giorni_mean_dataset) = c("Date", "Mean")
PM10_2018 = PM10[which(PM10$Anno==2018),c(2,3,4,6,9,10,11,12)]
Normality = aggregate(formula = Valore ~ Mese,
                      data = PM10_2018,
                      FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})
PM10_2018 = PM10[which(PM10$Anno==2018),c(2,3,4,9,10,11,12)]

# Istogramma e Ridgeline Plot per PM10 ------------------------------------

colors=c(wes_palette("Zissou1", 70, type = "continuous")[50:70], rev(wes_palette("Zissou1", 104, type = "continuous")))
x11()
ggplot(PM10_2018,aes(x=Valore,y=..density..)) +
  geom_histogram( binwidth=1, fill=colors, color="black", alpha=0.9, size=0.05) +
  geom_line(aes(x=Valore, y=dgamma(Valore,5.2,0.22)), col="blue",size=0.8) +
  ggtitle("Histogram for day-by-day PM10's mean values (Emilia-Romagna)") +
  theme_ipsum() +
  xlab("PM10") +
  theme(plot.title = element_text(size=15))

x11()
ggplot(PM10_2018,aes(x=Valore, y=..density.., color=month(Data, label = TRUE), fill=month(Data, label = TRUE))) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Density") +
  facet_wrap(~month(Data, label = TRUE))

x11()
ggplot(PM10_2018, aes(x = Valore, y = month(Data, label = TRUE), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low="red", high="green") +
  labs(title = 'PM10 in 2018') +
  ylab("Months") +
  xlim(c(0,90)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Year Plot (All Stations) ------------------------------------------------

#formato ideale per ggplot
data_long = melt(PM10_data.stazione,id.vars= "Date")
for(i in 1:length(Stazioni)){
  for(j in 1:dim(data_long)[1]){
    if(is.na(data_long$value[j]))
      data_long$value[j] = Stazioni_mean[i]
  }
}
colnames(data_long) = c("Date","Station","PM10")

x11()
ggplot(data_long,aes(x=Date, y=PM10, col= Station)) +
  geom_line() + 
  scale_color_manual(values=hsv(seq(0,1 - 1/49,length.out = 49), 
                                .55, 1), guide=FALSE) +
  geom_line(data = Giorni_mean_dataset, aes(x=Date, y=Mean), col = "darkred",
            size=1.2, show.legend = TRUE) +
  labs(title="PM10-Emilia and Mean") + 
  theme(legend.position=c(0.8, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10,face="bold"))

# ZoneType Plots (Rural, Suburban, Urban) ---------------------------------

PM10$Area = as.factor(PM10$Area)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Zona = PM10$Area[pos]
data_long$Zona = factor(data_long$Zona, levels = c("Rurale", "Suburbano", "Urbano"))
Area_mean = c(mean(as.matrix(data_long[which(data_long$Zona=="Rurale"),3])),
              mean(as.matrix(data_long[which(data_long$Zona=="Suburbano"),3])),
              mean(as.matrix(data_long[which(data_long$Zona=="Urbano"),3])))

#Unico Grafico
x11()
ggplot(data_long,aes(x=Date, y=PM10, group=Station, col=Zona)) +
  geom_line(alpha=0.5) +
  scale_color_manual(values=c("darkgreen",terrain.colors(50)[25],"red")) +
  geom_hline(aes(yintercept=Area_mean[1]), col= "darkgreen", 
             linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Area_mean[2]), col= terrain.colors(50)[25], 
             linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Area_mean[3]), col= "red", 
             linetype="twodash", size=1, show.legend = FALSE) +
  labs(title="PM10-Emilia for each zone: Rural, Suburban and Urban") +
  theme(legend.position=c(0.5, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10,face="bold"))

#grafico per ogni zona

#levels(as.factor(as.character(data_long$Station[which(data_long$Zona=="Rurale")])))
#12 zone Rurali
ra <- ggplot(data_long[which(data_long$Zona=="Rurale"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() + 
        scale_color_manual(values=terrain.colors(50)[1:12]) +
        geom_hline(aes(yintercept=Area_mean[1], linetype= "Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        ylim(c(0,124)) +
        labs(title="PM10-Emilia: RURAL AREA") +
        theme(legend.position="none") 

#levels(as.factor(as.character(data_long$Station[which(data_long$Zona=="Suburbano")])))
#14 zone Suburbane
sa <- ggplot(data_long[which(data_long$Zona=="Suburbano"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() + 
        scale_color_manual(values=terrain.colors(50)[21:34]) +
        geom_hline(aes(yintercept=Area_mean[2], linetype= "Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        ylim(c(0,124)) +
        labs(title="PM10-Emilia: SUBURBAN AREA") +
        theme(legend.position="none") 

#levels(as.factor(as.character(data_long$Station[which(data_long$Zona=="Urbano")])))
#23 zone Urbane
ua <- ggplot(data_long[which(data_long$Zona=="Urbano"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() + 
        scale_color_manual(values=hsv(1, seq(0,1,length.out = 23) , 1)) +
        geom_hline(aes(yintercept=Area_mean[3], linetype= "Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: URBAN AREA") +
        theme(legend.position="none") 

x11()
grid.arrange(ra, sa, ua, ncol=3)

#Istogrammi e Ridgeplot

x11()
ggplot(PM10_2018,aes(x=Valore, color=Area, fill=Area)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Count") +
  facet_wrap(~Area)

x11()
ggplot(PM10_2018, aes(x = Valore, y = Area, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low="red", high="blue") +
  labs(title = 'PM10 in 2018 according to different areas: Rural, Suburban, Urban') +
  ylab("Areas") +
  xlim(c(0,90)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Weekends and Weekdays Plots ---------------------------------------------

PM10$WGio = as.factor(weekdays(as.Date(PM10$Data), abbreviate = TRUE))
pos = match(as.factor(data_long$Date),as.factor(PM10$Data))
data_long$Giorni = PM10$WGio[pos]
data_long$Giorni = factor(data_long$Giorni, levels = c("lun", "mar", "mer", "gio", "ven", "sab", "dom"))
Giorni_mean = c(mean(as.matrix(data_long[which(data_long$Giorni=="lun"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="mar"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="mer"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="gio"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="ven"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="sab"),3])),
                mean(as.matrix(data_long[which(data_long$Giorni=="dom"),3])))
Weekday_end_mean = c(mean(as.matrix(data_long[which(data_long$Giorni!="sab" & data_long$Giorni!="dom"),3])),
                     mean(as.matrix(data_long[which(data_long$Giorni=="sab" | data_long$Giorni=="dom"),3])))

#Unico Grafico
x11()
ggplot(data_long,aes(x=Date, y=PM10, group=Station, col=Giorni)) +
  geom_line(alpha=0.4) +
  scale_color_manual(values=rainbow(7)) +
  geom_hline(aes(yintercept=Giorni_mean[1], linetype= "Total Mean"), 
             col= rainbow(7)[1], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[2], linetype= "Total Mean"), 
             col= rainbow(7)[2], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[3], linetype= "Total Mean"), 
             col= rainbow(7)[3], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[4], linetype= "Total Mean"), 
             col= rainbow(7)[4], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[5], linetype= "Total Mean"), 
             col= rainbow(7)[5], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[6], linetype= "Total Mean"), 
             col= rainbow(7)[6], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Giorni_mean[7], linetype= "Total Mean"), 
             col= rainbow(7)[7], linetype="twodash", size=1, show.legend = FALSE) +
  labs(title="PM10-Emilia with color for each day of the week") +
  theme(legend.position=c(0.5, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

#Istogrammi

#tutti i giorni
PM10_2018$WGio = as.factor(weekdays(as.Date(PM10_2018$Data), abbreviate = TRUE))
PM10_2018$WGio = factor(PM10_2018$WGio, levels = c("lun", "mar", "mer", "gio", "ven", "sab", "dom"))

x11()
ggplot(PM10_2018,aes(x=Valore, color=WGio, fill=WGio)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Count") +
  facet_wrap(~WGio)

x11()
ggplot(PM10_2018, aes(x = Valore, y = rev(WGio), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low="yellow", high="purple") +
  labs(title = 'PM10 in 2018 for each day in a week') +
  ylab("Days") +
  xlim(c(0,90)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

#settimana vs weekend
PM10_2018$Ends = rep("Day", dim(PM10_2018)[1])
PM10_2018[which(PM10_2018$WGio == "sab" | PM10_2018$WGio == "dom"),9] = "Ends"

x11()
ggplot(PM10_2018,aes(x=Valore, y =..density.., color=Ends, fill=Ends)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Density") +
  facet_wrap(~Ends)

# Provincia Plots ---------------------------------------------------------

PM10$Provincia = as.factor(PM10$Provincia)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Provincia = PM10$Provincia[pos]
Province_mean = aggregate(PM10_2018$Valore, list(PM10_2018$Provincia), FUN=mean)

#Unico Grafico
x11()
ggplot(data_long,aes(x=Date, y=PM10, group=Station, col=Provincia)) +
  geom_line(alpha=0.6) +
  scale_color_manual(values=rainbow(9)) +
  geom_hline(aes(yintercept=Province_mean[1,2], linetype= "Total Mean"), 
             col= rainbow(9)[1], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[2,2], linetype= "Total Mean"), 
             col= rainbow(9)[2], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[3,2], linetype= "Total Mean"), 
             col= rainbow(9)[3], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[4,2], linetype= "Total Mean"), 
             col= rainbow(9)[4], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[5,2], linetype= "Total Mean"), 
             col= rainbow(9)[5], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[6,2], linetype= "Total Mean"), 
             col= rainbow(9)[6], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[7,2], linetype= "Total Mean"), 
             col= rainbow(9)[7], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[8,2], linetype= "Total Mean"), 
             col= rainbow(9)[8], linetype="twodash", size=1, show.legend =FALSE) +
  geom_hline(aes(yintercept=Province_mean[9,2], linetype= "Total Mean"), 
             col= rainbow(9)[9], linetype="twodash", size=1, show.legend =FALSE) +
  labs(title="PM10-Emilia for each Provincia") +
  theme(legend.position=c(0.8, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

#grafico per ogni provincia

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="BO")])))
#7
bo <- ggplot(data_long[which(data_long$Provincia=="BO"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[1:7]) +
        geom_hline(aes(yintercept=Province_mean[1,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: BO") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="FC")])))
#5
fc <- ggplot(data_long[which(data_long$Provincia=="FC"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[8:12]) +
        geom_hline(aes(yintercept=Province_mean[2,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: FC") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="FE")])))
#4
fe <- ggplot(data_long[which(data_long$Provincia=="FE"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[13:16]) +
        geom_hline(aes(yintercept=Province_mean[3,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: FE") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="MO")])))
#6
mo <- ggplot(data_long[which(data_long$Provincia=="MO"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[17:22]) +
        geom_hline(aes(yintercept=Province_mean[4,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: MO") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="PC")])))
#7
pc <- ggplot(data_long[which(data_long$Provincia=="PC"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[23:29]) +
        geom_hline(aes(yintercept=Province_mean[5,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: PC") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="PR")])))
#7
pr <- ggplot(data_long[which(data_long$Provincia=="PR"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[30:36]) +
        geom_hline(aes(yintercept=Province_mean[6,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: PR") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="RA")])))
#4
ra <- ggplot(data_long[which(data_long$Provincia=="RA"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[37:40]) +
        geom_hline(aes(yintercept=Province_mean[7,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: RA") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="RE")])))
#5
re <- ggplot(data_long[which(data_long$Provincia=="RE"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[41:45]) +
        geom_hline(aes(yintercept=Province_mean[8,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: RE") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Provincia=="RN")])))
#4
rn <- ggplot(data_long[which(data_long$Provincia=="RN"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=rainbow(49)[46:49]) +
        geom_hline(aes(yintercept=Province_mean[9,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: RN") +
        ylim(c(0,124)) +
        theme(legend.position="none")

x11()
grid.arrange(bo, fc, fe, mo, pc, pr, ra, re, rn, ncol=3)

#Istogrammi e Ridgeplots

x11()
ggplot(PM10_2018,aes(x=Valore, color=Provincia, fill=Provincia)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Count") +
  facet_wrap(~Provincia)

# Zonizzazione Plots ------------------------------------------------------

PM10$Zonizzazione = as.factor(PM10$Zonizzazione)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Zonizzazione =PM10$Zonizzazione[pos]
Zonizzazione_mean = aggregate(PM10_2018$Valore, list(PM10_2018$Zonizzazione), FUN=mean)

#Unico Grafico
x11()
ggplot(data_long,aes(x=Date, y=PM10, group=Station, col=Zonizzazione)) +
  geom_line(alpha=0.4) +
  scale_color_manual(values=rainbow(4)) +
  geom_hline(aes(yintercept=Zonizzazione_mean[1,2], linetype= "Total Mean"), 
             col= rainbow(4)[1], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Zonizzazione_mean[2,2], linetype= "Total Mean"), 
             col= rainbow(4)[2], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Zonizzazione_mean[3,2], linetype= "Total Mean"), 
             col= rainbow(4)[3], linetype="twodash", size=1, show.legend = FALSE) +
  geom_hline(aes(yintercept=Zonizzazione_mean[4,2], linetype= "Total Mean"), 
             col= rainbow(4)[4], linetype="twodash", size=1, show.legend = FALSE) +
  labs(title="PM10-Emilia according to: Agglomerato, Appennini, Pianura Est/Ovest") +
  theme(legend.position=c(0.75, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

#grafico per ogni zona

#levels(as.factor(as.character(data_long$Station[which(data_long$Zonizzazione=="Agglomerato")])))
#4
ag <- ggplot(data_long[which(data_long$Zonizzazione=="Agglomerato"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=hsv(1, seq(0,1,length.out = 5) , 1)[2:5]) +
        geom_hline(aes(yintercept=Zonizzazione_mean[1,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: Conurbation") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Zonizzazione=="Appennino")])))
#4
ap <- ggplot(data_long[which(data_long$Zonizzazione=="Appennino"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=hsv(0.3, seq(0,1,length.out = 5) , 1)[2:5]) +
        geom_hline(aes(yintercept=Zonizzazione_mean[2,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: Appennino") +
        ylim(c(0,124)) +
        theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Zonizzazione=="Pianura Est")])))
#18
pe <- ggplot(data_long[which(data_long$Zonizzazione=="Pianura Est"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=hsv(0.6, seq(0,1,length.out = 19) , 1)[2:19]) +
        geom_hline(aes(yintercept=Zonizzazione_mean[3,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: East Plain") +
        ylim(c(0,124)) +
        theme(legend.position="none")
      
#levels(as.factor(as.character(data_long$Station[which(data_long$Zonizzazione=="Pianura Ovest")])))
#23
po <- ggplot(data_long[which(data_long$Zonizzazione=="Pianura Ovest"),],aes(x=Date, y=PM10, col=Station)) +
        geom_line() +
        scale_color_manual(values=hsv(0.8, seq(0,1,length.out = 24) , 1)[2:24]) +
        geom_hline(aes(yintercept=Zonizzazione_mean[4,2], linetype= "Total Mean"), 
                   col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
        labs(title="PM10-Emilia: West Plain") +
        ylim(c(0,124)) +
        theme(legend.position="none")

x11()
grid.arrange(ag, ap, pe, po, ncol=2)

#Istogrammi e Ridgeplots

x11()
ggplot(PM10_2018,aes(x=Valore, y=..density.., color=Zonizzazione, fill=Zonizzazione)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Count") +
  facet_wrap(~Zonizzazione)

# Fondo, Industriale, Traffico Plots --------------------------------------

PM10$Tipo = as.factor(PM10$Tipo)
pos = match(as.factor(data_long$Station),PM10$NomeStazione)
data_long$Tipo = PM10$Tipo[pos]
Tipo_mean = aggregate(PM10_2018$Valore, list(PM10_2018$Tipo), FUN=mean)

#Unico Grafico
x11()
ggplot(data_long,aes(x=Date, y=PM10, group=Station, col=Tipo)) +
  geom_line(alpha=0.4) +
  scale_color_manual(values=rainbow(3)) +
  geom_hline(aes(yintercept=Tipo_mean[1,2], linetype= "Total Mean"), 
             col= rainbow(3)[1], linetype="twodash", size=1.2, show.legend = FALSE) +
  geom_hline(aes(yintercept=Tipo_mean[2,2], linetype= "Total Mean"), 
             col= rainbow(3)[2], linetype="twodash", size=1.2, show.legend = FALSE) +
  geom_hline(aes(yintercept=Tipo_mean[3,2], linetype= "Total Mean"), 
             col= rainbow(3)[3], linetype="twodash", size=1.2, show.legend = FALSE) +
  labs(title="PM10-Emilia: Fondo, Industriale, Traffico") +
  theme(legend.position=c(0.8, 0.9),
        legend.direction="horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

#grafico per ogni categoria

#levels(as.factor(as.character(data_long$Station[which(data_long$Tipo=="Fondo")])))
#33
f <- ggplot(data_long[which(data_long$Tipo=="Fondo"),],aes(x=Date, y=PM10, col=Station)) +
       geom_line() +
       scale_color_manual(values=colors(59)[10:43]) +
       geom_hline(aes(yintercept=Tipo_mean[1,2], linetype= "Total Mean"), 
                  col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
       labs(title="PM10-Emilia: Background") +
       ylim(c(0,124)) +
       theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Tipo=="Industriale")])))
#5
i <- ggplot(data_long[which(data_long$Tipo=="Industriale"),],aes(x=Date, y=PM10, col=Station)) +
       geom_line() +
       scale_color_manual(values=colors(59)[44:48]) +
       geom_hline(aes(yintercept=Tipo_mean[2,2], linetype= "Total Mean"), 
                  col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
       labs(title="PM10-Emilia: Industrial") +
       ylim(c(0,124)) +
       theme(legend.position="none")

#levels(as.factor(as.character(data_long$Station[which(data_long$Tipo=="Traffico")])))
#11
t <- ggplot(data_long[which(data_long$Tipo=="Traffico"),],aes(x=Date, y=PM10, col=Station)) +
     geom_line() +
     scale_color_manual(values=colors(59)[49:59]) +
     geom_hline(aes(yintercept=Tipo_mean[3,2], linetype= "Total Mean"), 
                col= "darkred", linetype="twodash", size=1.2, show.legend =TRUE) +
     labs(title="PM10-Emilia: Traffic") +
     ylim(c(0,124)) +
     theme(legend.position="none")

x11()
grid.arrange(f, i, t, ncol=3)

#Istogrammi e Ridgeplots

x11()
ggplot(PM10_2018,aes(x=Valore, y=..density.., color=Tipo, fill=Tipo)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("PM10") +
  ylab("Count") +
  facet_wrap(~Tipo)


  


