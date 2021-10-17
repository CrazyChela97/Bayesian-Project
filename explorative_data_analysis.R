### ANALISI MICHI

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(roahd)

##### IMPORT DATASET #####
raw_data <- read_excel("Base PM10 2014-2019.xlsx")
raw_data$Data = as.Date(raw_data$Data)
dati = raw_data[,c(1,8,2)]
dati = dati[which(!is.na(dati$NomeStazione)), ]
dati = dati[which(!is.na(dati$Data)), ]


# Tutti i valori NA sono posti uguali a zero 
# Tutte le stazioni con SOLO valori NA vengono rimosse
# Definire prima NA trattati diversamente (rimossi? media tra i valori precedente-successivo?)
prova_table = xtabs(Valore ~ Data+NomeStazione, data=dati)
prova_table = as.matrix(prova_table)
write.csv(prova_table, file='dati.csv')
dati_raw <- read_csv("dati.csv")
names(dati_raw)[1] = 'Data'


##### DUBBI SU DATASET #####

# Controllo sulle date
max(raw_data$Data) - min(raw_data$Data)
length(levels(factor(raw_data$Data)))
# Dovremmo avere 6 anni e mezzo di dati --> circa 2300 giorni
# Abbiamo invece solo 1269 giorni registrati
# Ci sono molti giorni per cui non è stata fatta alcuna rilevazione

# Controllo sulle stazioni
length(levels(factor(raw_data$NomeStazione)))
# Risultano 205 stazioni diverse mentre, guardando l'articolo, dovremmo averne solo 180
# Vediamo numero di osservazioni utili (non NA) per ogni stazione
dati_no_NA = raw_data[which(!is.na(raw_data$Valore)), ]
daily_obs = table(dati_no_NA$Data, dati_no_NA$NomeStazione)
dati_per_stazione = colSums(daily_obs)
length(which(dati_per_stazione > 1000)) 
# Scartando le stazioni con poche osservazioni rispetto alle date considerate ne otteniamo circa 180
# E' giusto tenere solo queste?
max(dati_per_stazione)
# molte più osservazioni rispetto alle date considerate 
# --> alcune stazioni avranno più osservazioni al giorno
max(daily_obs)   # osservazioni doppie per alcune stazioni
length(daily_obs[which(daily_obs > 1)])
# più di 4000 giorni con osservazioni doppie : come trattarle?



# DATASET PULITO
nomi_stazioni = c(names(dati_per_stazione[which(dati_per_stazione > 1000)]))
nomi_stazioni
dati_clean = subset(dati_raw, select= nomi_stazioni)


##### GRAFICI ESPLORATIVI #####
# plot dati iniziali
matplot(dati_raw[,-1], type='l')
# functional data con pacchetto roahd
l = dim(dati_raw)[1]
data_fun = fData(1:l, t(dati_raw[, -1]))
plot(data_fun)


# plot dati ridotti
matplot(dati_clean[,-1], type='l')
# functional data con pacchetto roahd
l = dim(dati_clean)[1]
data_fun = fData(1:l, t(dati_clean[, -1]))
plot(data_fun)



##### Verifica stazioni NA #####
# Tre stazioni hanno solo valori NA : 
# `Asiago Cima Ekar`, `Bassano del Grappa`, and `Montebello Vicentino`
MB = raw_data[which(raw_data$NomeStazione=='Montebello Vicentino'), 2]
dim(MB)[1]       
length(which(is.na(MB)))
# tutti i valori registrati sono NA

BG = raw_data[which(raw_data$NomeStazione=='Bassano del Grappa'), 2]
dim(BG)[1]       
length(which(is.na(BG)))
# tutti i valori registrati sono NA

AS = raw_data[which(raw_data$NomeStazione=='Asiago Cima Ekar'), 2]
dim(AS)[1]       
length(which(is.na(AS)))
# tutti i valori registrati sono NA
