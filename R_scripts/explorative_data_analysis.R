###########################################################################
#                    EXPLORATIVE DATA ANALYSIS                            #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------

# library(rstudioapi)
library(tibble)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(roahd)
library(anytime) #Date type
library(dygraphs)
library(xts)          
library(tidyverse)
library(lubridate)


# Importazione dei dati ---------------------------------------------------

PM10_raw <- read_excel("Data/Base PM10 2014-2019.xlsx")
PM10_raw$Data = as.Date(PM10_raw$Data)
PM10_raw$AreaStazione=as.factor(PM10_raw$AreaStazione)
PM10_raw$TipoStazione=as.factor(PM10_raw$TipoStazione)
PM10_raw$NomeStazione=as.factor(as.character(PM10_raw$NomeStazione))


PM10=cbind.data.frame(PM10_raw$Data,PM10_raw$NomeStazione, PM10_raw$Valore)
colnames(PM10)=c("Data","NomeStazione","Valore")


# Studio del dataset ------------------------------------------------------

## Controllo sulle date

max(PM10$Data) - min(PM10$Data)
length(levels(factor(PM10$Data)))
# Dovremmo avere 6 anni e mezzo di dati --> circa 2300 giorni
# Abbiamo invece solo 1269 giorni registrati
# Ci sono molti giorni per cui non ? stata fatta alcuna rilevazione

## Controllo sulle stazioni

length(levels(factor(PM10$NomeStazione)))
# Risultano 205 stazioni diverse mentre, guardando l'articolo, dovremmo averne solo 180
# Vediamo numero di osservazioni utili (non NA) per ogni stazione
dati_no_NA = PM10[which(!is.na(PM10$Valore)), ]
daily_obs = table(dati_no_NA$Data, dati_no_NA$NomeStazione) 
dati_per_stazione = colSums(daily_obs)
length(which(dati_per_stazione > 1000)) 
# Scartando le stazioni con poche osservazioni rispetto alle date considerate ne otteniamo circa 180
# E' giusto tenere solo queste?

max(dati_per_stazione)
# molte piu osservazioni rispetto alle date considerate 
# --> alcune stazioni avranno piu osservazioni al giorno
max(daily_obs)   # osservazioni doppie per alcune stazioni
length(daily_obs[which(daily_obs > 1)])
# piu di 4000 giorni con osservazioni doppie : come trattarle?

PM10_freq = as.data.frame(daily_obs)

# per es Asti-D'Acquisto 2014-01-01
PM10[which(PM10$Data=='2014-01-01' & PM10$NomeStazione=="Asti - D'Acquisto"), 2]
PM10$`Asti - D'Acquisto`[which(PM10$Data=='2014-01-01')]

# xtabs fa la somma: no bueno --> proviamo a fare la media
# COSA FA ?
nobs_per_day = as.matrix(daily_obs)
write.csv(nobs_per_day, file='Data/nobs.csv')
nobs_per_day <- read_csv("Data/nobs.csv")
names(nobs_per_day)[1] = 'Data'
prova = PM10[,-1]/nobs_per_day[,-1]
prova[is.na(prova)] = 0
prova = add_column(prova, nobs_per_day$Data, .before=1)


## pie chart of additional variables

ggplot(PM10_raw, aes(x="", fill=AreaStazione))+ geom_bar(width = 1)+ coord_polar("y")
ggplot(PM10_raw, aes(x="", fill=TipoStazione))+ geom_bar(width = 1)+ coord_polar("y")

# Pulizia database --------------------------------------------------------

#Caso 1: Valore=NA
length(PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]) #6121
#dato che voglio creare una serie nel tempo continua metto a 0 questi valori
#che risultano essere comunque pochi rispetto al campione preso in considerazione

PM10_raw$Valore[which(is.na(PM10_raw$Valore)=='TRUE')]=-1


#Caso 2:NS=NA
length(PM10_raw$NomeStazione[which(is.na(PM10_raw$NomeStazione)=='TRUE')]) #328
#dati da eliminare perch? non non hanno significato senza geolocalizzazione

PM10_raw=PM10_raw[which(is.na(PM10_raw$NomeStazione)=='FALSE'),]


#Caso 3:Data=NA
length(PM10_raw$Data[which(is.na(PM10_raw$Data)=='TRUE')]) #0
#no data

E_Stazioni=levels(PM10$NomeStazione)

data_for_dayNS=NULL
for (i in 1:length(E_Stazioni)){
  data_for_dayNS[i]=dim(PM10_raw[which(PM10_raw$NomeStazione==as.character(E_Stazioni[i])),])[1]
} 

dates=levels(as.factor(PM10_raw$Data))
data_for_dayNS=length(dates)

PM10_valuecol=data.frame(dates)

for(i in 2:(length(E_Stazioni)+1)){
  pos = match(PM10_valuecol$dates,as.factor(PM10_raw$Data[which(PM10_raw$NomeStazione==E_Stazioni[i-1])]))
  PM10_valuecol[i]=PM10_raw$Valore[pos]
}

colnames(PM10_valuecol)=c("Date",E_Stazioni)
View(PM10_valuecol)
#database che ha per righe le date e per colonne le stazioni. Non in tutte le date sono 
#stati raccolti i dati, quindi ho dei NA per i valori corrispondenti, bisogna decidere se 
#metterli a zero oppure a -1 per differenziarli dalla casistica di prima (o mettere entrambi
#a zero/-1)

# Studio dei valori mancanti ----------------------------------------------


# Tre stazioni hanno solo valori NA : 
  # `Asiago Cima Ekar`, `Bassano del Grappa`, and `Montebello Vicentino`
  MB = PM10[which(PM10$NomeStazione=='Montebello Vicentino'), 2]
  dim(MB)[1]       
  length(which(is.na(MB)))  # tutti i valori registrati sono NA
  
  BG = PM10[which(PM10$NomeStazione=='Bassano del Grappa'), 2]
  dim(BG)[1]       
  length(which(is.na(BG)))  # tutti i valori registrati sono NA
  
  AS = PM10[which(PM10$NomeStazione=='Asiago Cima Ekar'), 2]
  dim(AS)[1]       
  length(which(is.na(AS)))  # tutti i valori registrati sono NA

# Grafici esplorativi -----------------------------------------------------

# plot dati iniziali
matplot(PM10_raw[,-1], type='l')
# functional data con pacchetto roahd
l = dim(PM10_raw)[1]
data_fun = fData(1:l, t(PM10_raw[, -1]))
plot(data_fun)


# plot dati puliti
plot.new()
matplot(dati_clean[,-1], type='l')
# functional data con pacchetto roahd
l = dim(dati_clean)[1]
data_fun = fData(1:l, t(dati_clean[, -1]))
plot(data_fun)



don <- xts(x = PM10_raw$Valore[which(PM10_raw$NomeStazione=="Adria")], order.by = PM10_raw$Data[which(PM10_raw$NomeStazione=="Adria")])
# Finally the plot
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

p


# prova 1234



