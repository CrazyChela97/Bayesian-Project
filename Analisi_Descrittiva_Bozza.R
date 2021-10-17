#~
rm(list=ls())
setwd("C:/Users/franc/Desktop/Bayesian Statistic/Project")

library("readxl") #Excel xlsx
library(anytime) #Date type
library(dygraphs)
library(xts)          
library(tidyverse)
library(lubridate)

PM10 <- read_excel("Base_PM10_2014-2019.xlsx")
View(PM10)

PM10_raw=cbind.data.frame(PM10$Data,PM10$Valore,PM10$NomeStazione)
colnames(PM10_raw)=c("Data","Valore","NS")

PM10_raw$Data=anydate(as.factor(PM10_raw$Data))
#forse meglio factor per fare l'ascissa nel dato funzionale
PM10_raw$NS=as.factor(as.character(PM10_raw$NS))
E_Stazioni=levels(PM10_raw$NS)


#Pulizia database ridotto

#Caso 1: Valore=NA
length(PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]) #6121
#dato che voglio creare una serie nel tempo continua metto a 0 questi valori
#che risultano essere comunque pochi rispetto al campione preso in considerazione

PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]=-1


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

data_for_dayNS==length(dates)

dates=levels(as.factor(PM10_raw$Data))
PM10_valuecol=data.frame(dates)

for(i in 2:(length(E_Stazioni)+1)){
  pos = match(PM10_valuecol$dates,as.factor(PM10_raw$Data[which(PM10_raw$NS==E_Stazioni[i-1])]))
  PM10_valuecol[i]=PM10_raw$Valore[pos]
}

colnames(PM10_valuecol)=c("Date",E_Stazioni)
View(PM10_valuecol)
#database che ha per righe le date e per colonne le stazioni. Non in tutte le date sono 
#stati raccolti i dati, quindi ho dei NA per i valori corrispondenti, bisogna decidere se 
#metterli a zero oppure a -1 per differenziarli dalla casistica di prima (o mettere entrambi
#a zero/-1)

#provo con tutto a -1






don <- xts(x = PM10_raw$Valore[which(PM10_raw$NS=="Adria")], order.by = PM10_raw$Data[which(PM10_raw$NS=="Adria")])

# Finally the plot
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

x11()
p


