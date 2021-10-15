# import dataset
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(roahd)

raw_data <- read_excel("Base PM10 2014-2019.xlsx")
raw_data$Data = as.Date(raw_data$Data)
dati = raw_data
dati = dati[,c(1,8,2)]
#toglie da solo NA : li butta a zero! Quindi bisogna trattarli prima di creare tabella
prova_table = xtabs(Valore ~ Data+NomeStazione, data=dati)
prova_table = as.matrix(prova_table)
write.csv(prova_table, file='dati.csv')
dati <- read_csv("dati.csv")
names(dati)[1] = 'Data'

# plot con matplot
matplot(dati[,-1], type='l')
# functional data con pacchetto roahd
l = dim(dati)[1]
data_fun = fData(1:l, t(dati[, -1]))
plot(data_fun)

length(levels(factor(raw_data$Data)))
length(levels(factor(raw_data$NomeStazione)))
# perchè ne abbiamo solo 202 e non 205? :'(
# vanno controllati dati doppi e dati NA
       