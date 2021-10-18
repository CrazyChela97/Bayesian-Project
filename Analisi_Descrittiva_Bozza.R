#~
rm(list=ls())
setwd("C:/Users/franc/Desktop/Bayesian Statistic/Project")

library(readxl) #Excel xlsx
library(anytime) #Date type
library(dygraphs)
library(xts)          
library(tidyverse)
library(lubridate)
library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(fda)
library(roahd)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)
options(knitr.table.format = "html")
library(reshape2)

PM10 <- read_excel("Base_PM10_2014-2019.xlsx")
View(PM10)

PM10_raw=cbind.data.frame(PM10$Data,PM10$Valore,PM10$NomeStazione)
colnames(PM10_raw)=c("Data","Valore","NS")

PM10_raw$Data=anydate(as.factor(PM10_raw$Data))
#forse meglio factor per fare l'ascissa nel dato funzionale
PM10_raw$NS=as.factor(as.character(PM10_raw$NS))
E_Stazioni=as.factor(levels(PM10_raw$NS))


#Pulizia database ridotto
#Caso 1: Valore=NA
length(PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]) #6121
#dato che voglio creare una serie nel tempo continua metto a 0 questi valori
#che risultano essere comunque pochi rispetto al campione preso in considerazione
#PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]=-1

#Caso 2:NS=NA
length(PM10_raw$NS[which(is.na(PM10_raw$NS)=='TRUE')]) #328
#dati da eliminare perchè non non hanno significato senza geolocalizzazione
PM10_raw=PM10_raw[which(is.na(PM10_raw$NS)=='FALSE'),]

#Caso 3:Data=NA
length(PM10_raw$Data[which(is.na(PM10_raw$Data)=='TRUE')]) #0
#no data

data_for_dayNS=NULL
for (i in 1:length(E_Stazioni)){
  data_for_dayNS[i]=dim(PM10_raw[which(PM10_raw$NS==as.character(E_Stazioni[i])),])[1]
} 

dates=as.factor(levels(as.factor(PM10_raw$Data)))
PM10_valuecol=as.data.frame(dates)
E_Stazioni[which(data_for_dayNS>length(dates))]
## "Asti - D'Acquisto" "Baceno - Alpe Devero" "Biella - Sturzo" "Bra - Madonna Fiori" 
## "Cuneo - Alpini" "Ivrea - Liberazione"  "Novara - Arpa" "Torino - Lingotto"   
## "Torino - Rubino"



#creo databese adeguato
for(i in 2:(length(E_Stazioni)+1)){
  v_s=PM10_raw[which(PM10_raw$NS==E_Stazioni[i-1]),] #str per vedere il tipo della variabile
  pos = match(PM10_valuecol$dates,as.factor(v_s$Data))
  PM10_valuecol[i]=v_s$Valore[pos]
}

colnames(PM10_valuecol)=c("Date",as.character(E_Stazioni))
View(PM10_valuecol)
#database che ha per righe le date e per colonne le stazioni. Non in tutte le date sono 
#stati raccolti i dati, quindi ho dei NA per i valori corrispondenti, bisogna decidere se 
#metterli a zero oppure a -1 per differenziarli dalla casistica di prima (o mettere entrambi
#a zero/-1)

#provo con tutto a -1
PM10_valuecol[is.na(PM10_valuecol)] = -1
PM10_valuecol$Date=as.Date(PM10_valuecol$Date)

max(PM10_valuecol[,-1])
min(PM10_valuecol[,-1])

#conversione per formato ottimale
data_long=melt(PM10_valuecol,id.vars= "Date")
x11()
ggplot(data_long,aes(x=Date, y=value, col=variable)) +
  geom_line() + 
  theme(legend.position="none") +
  scale_fill_manual(values=rainbow(205),
                    guide=FALSE)




#aggiungo tutti i giorni per i quali non ho dati e creo un nuovo database
all_dates=seq(as.Date("2014-01-01"), as.Date("2020-06-30"), by="days")
PM10_alldates=data.frame(all_dates)

for(i in 2:(length(E_Stazioni)+1)){
  v_s=PM10_raw[which(PM10_raw$NS==E_Stazioni[i-1]),] #str per vedere il tipo della variabile
  pos = match(as.factor(PM10_alldates$all_dates),as.factor(v_s$Data))
  PM10_alldates[i]=v_s$Valore[pos]
}

colnames(PM10_alldates)=c("Date",as.character(E_Stazioni))
View(PM10_alldates)

PM10_alldates[is.na(PM10_alldates)] = -1

#conversione per formato ottimale
data_long=melt(PM10_alldates,id.vars= "Date")
x11()
ggplot(data_long,aes(x=Date, y=value, col=variable)) +
  geom_line() + 
  theme(legend.position="none")


