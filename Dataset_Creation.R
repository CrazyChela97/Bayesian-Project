###########################################################################
#                     DATA EXPLORATION AND ADJUSTMENT                     #
###########################################################################

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

rm(list = ls())

# Packages ----------------------------------------------------------------
library(tibble)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(roahd)

# LOMBARDIA ---------------------------------------------------------------

PM10_Lombardia = read_csv('all_PM10_lomb.csv')
# 2020 Data not considered
PM10_Lombardia = PM10_Lombardia[which(PM10_Lombardia$Anno!="2020"), ]

# Variables of interest
dati_lomb = PM10_Lombardia[ ,c(2,3,15,6)]
dati_lomb$Data = as.Date(dati_lomb$Data)
dati_lomb = dati_lomb[which(!is.na(dati_lomb$Valore) & !is.na(dati_lomb$NomeStazione)), ]

# Check on the dates : 1 day is missing (2015-01-01)
max(dati_lomb$Data) - min(dati_lomb$Data) + 1   # adding 1 day since 2016 is bisestile
length(levels(factor(dati_lomb$Data)))

# Check on the stations : 64 stations
length(levels(factor(dati_lomb$NomeStazione)))


# DOUBLE DATA
n_obs = table(dati_lomb$NomeStazione, dati_lomb$Data)
n_obs = as.data.frame(n_obs)
max(n_obs$Freq)    # some stations actually registered 2 values
length(which(n_obs$Freq==2)) # only 9 stations with double data

# Analysis of these stations
obs_doppie = n_obs[which(n_obs$Freq==2), ]
names(obs_doppie) = c('NomeStazione', 'Data', 'Freq')
obs_doppie$Data = as.Date(obs_doppie$Data)
dati_doppi = as.data.frame(t(names(PM10_Lombardia)))
dati_doppi = dati_doppi[-1, ]
names(dati_doppi) = names(PM10_Lombardia)
for (i in 1:9){
  index = which(PM10_Lombardia$NomeStazione == obs_doppie$NomeStazione[i] & PM10_Lombardia$Data == obs_doppie$Data[i])
  temp = PM10_Lombardia[index, ]
  dati_doppi = rbind(dati_doppi, temp)
}
view(dati_doppi)

# Identical observations registered twice 
# Keeping only one of them
PM10_Lombardia = unique(PM10_Lombardia)
# One typo remaining: removing 34 and keeping 35 as value
n_obs = table(PM10_Lombardia$NomeStazione, PM10_Lombardia$Data)
n_obs = as.data.frame(n_obs)
n_obs$Var2 = as.Date(n_obs$Var2)
err = n_obs[which(n_obs$Freq==2), ]
errore = which(PM10_Lombardia$Data==err$Var2 & PM10_Lombardia$NomeStazione==err$Var1 & PM10_Lombardia$Valore==35)
PM10_Lombardia = PM10_Lombardia[-errore, ]


# MISSING VALUES
length(which(n_obs$Freq==0)) 
# nearly 10.000 total missing values over 140.160

# Missing values by year
missing_L = rep(0,6)
missing_by_year = 365-table(PM10_Lombardia$Anno, PM10_Lombardia$NomeStazione)
missing_by_year = as.data.frame(missing_by_year)
for (i in 2014:2019){
  missing_L[i-2013] = sum(missing_by_year$Freq[which(missing_by_year$Var1==i)])
}
rbind(year=c(2014:2019), missing_values=missing_L)
# Year with less missing values : 2018

# Keeping only useful variables
PM10_Lombardia = PM10_Lombardia[ , c(2,15,3,6,7,8,10,17,28,27,26,16)]
names(PM10_Lombardia) = c("Data", "NomeStazione", "Valore", "Anno", "Mese",       
                          "Giorno", "Wday", "Provincia", "Tipo", "Area",
                          "Zonizzazione", "Quota")
write.csv(PM10_Lombardia, 'PM10_Lombardia.csv')

rm(list = ls())


# EMILIA ------------------------------------------------------------------

PM10_Emilia = read_excel('all_PM10_emilia.xlsx')
meta_emilia = read_excel('all_PM10_emilia.xlsx', sheet = 2)
# 2020 Data not considered
PM10_Emilia = PM10_Emilia[which(PM10_Emilia$Anno!="2020"), ]

# Variables of interest
dati_em = PM10_Emilia[ ,c(5,6,2,8)]
index = match(dati_em$COD_STAZ, meta_emilia$COD_STAZ)
dati_em$NomeStazione = meta_emilia$Stazione[index]
names(dati_em)[1] = 'Data'
dati_em$Data = as.Date(dati_em$Data)
dati_em = dati_em[which(!is.na(dati_em$VALORE) & !is.na(dati_em$NomeStazione)), ]

# Check on the dates: 5 days are missing 
# Missing days are those of NYE ("2014-12-31" "2015-12-31" "2016-12-31" "2017-12-31" "2018-12-31")
max(dati_em$Data) - min(dati_em$Data) + 1   # adding 1 day since 2016 is bisestile
length(levels(factor(dati_em$Data)))

# Check on the stations : 49 stations
length(levels(factor(dati_em$NomeStazione)))


# DOUBLE DATA
n_obs = table(dati_em$NomeStazione, dati_em$Data)
n_obs = as.data.frame(n_obs)
max(n_obs$Freq)    
length(which(n_obs$Freq==2)) 
# no multiple observations


# MISSING VALUES
length(which(n_obs$Freq==0)) 
# nearly 9.000 total missing values over 107.114

# Missing values by year
missing_E = rep(0,6)
missing_by_year = 365-table(dati_em$Anno, dati_em$NomeStazione)
missing_by_year = as.data.frame(missing_by_year)
for (i in 2014:2019){
  missing_E[i-2013] = sum(missing_by_year$Freq[which(missing_by_year$Var1==i)])
}
rbind(c(2014:2019), missing_E)
# Year with less missing values : 2018


# Keeping only useful variables 
PM10_Emilia = PM10_Emilia[ ,c(2,5,6,8,9,10,11)]
# Matching with metadata
index = match(PM10_Emilia$COD_STAZ, meta_emilia$COD_STAZ)
PM10_Emilia = cbind(PM10_Emilia[ ,-1], meta_emilia[index, c(2,5,6,10,11,13)])
PM10_Emilia = PM10_Emilia[ ,c(1,7,2,3,4,5,6,8,10,11,12,9)]
names(PM10_Emilia) = c("Data", "NomeStazione", "Valore", "Anno", "Mese",       
                       "Giorno", "Wday", "Provincia", "Tipo", "Area",
                       "Zonizzazione", "Quota")

write.csv(PM10_Emilia, 'PM10_Emilia.csv')

rm(list = ls())


# PIEMONTE ----------------------------------------------------------------

PM10_Piemonte = read_excel('PM10_all_piemonte.xlsx')
# 2020 Data not considered
PM10_Piemonte = PM10_Piemonte[which(PM10_Piemonte$Anno!="2020"), ]

# Variables of interest
PM10_Piemonte$Data = as.Date(PM10_Piemonte$Data)
PM10_Piemonte$Wday = weekdays(PM10_Piemonte$Data)
dati_pi = PM10_Piemonte[ ,c(1,3,5,6)]
names(dati_pi)[3] = 'Valore'
dati_pi = dati_pi[which(!is.na(dati_pi$Valore) & !is.na(dati_pi$NomeStazione)
                        & !is.na(dati_pi$Data)), ]

# Check on the dates : OK
max(dati_pi$Data) - min(dati_pi$Data) + 1   # adding 1 day since 2016 is bisestile
length(levels(factor(dati_pi$Data)))

# Check on the stations : 51 stations
length(levels(factor(dati_pi$NomeStazione)))


# DOUBLE DATA
n_obs = table(dati_pi$NomeStazione, dati_pi$Data)
n_obs = as.data.frame(n_obs)
max(n_obs$Freq)    # some stations actually registered 2 values
length(which(n_obs$Freq==2))  # more than 6000 stations with double data

# Analysis of these stations
obs_doppie = n_obs[which(n_obs$Freq==2), ]
names(obs_doppie) = c('NomeStazione', 'Data', 'Freq')
obs_doppie$Data = as.Date(obs_doppie$Data)
dati_doppi = as.data.frame(t(names(PM10_Piemonte)))
dati_doppi = dati_doppi[-1, ]
names(dati_doppi) = names(PM10_Piemonte)
N = length(which(n_obs$Freq==2))
for (i in 1:N){
  temp = PM10_Piemonte[which(PM10_Piemonte$NomeStazione == obs_doppie$NomeStazione[i] & PM10_Piemonte$Data == obs_doppie$Data[i]), ]
  dati_doppi = rbind(dati_doppi, temp)
}
view(dati_doppi)
# PROBLEM : in this case the values registered twice are different from eachothers
# For coherence with other datasets we choose to keep the 'Beta' value registered 
# (i.e. the automatic registration) in case the two registered values are of the kind
# 'Beta' vs 'Basso Volume'

# Even after discarding the non-beta values, we are left with 25 more double values from a single station
# in this case the value is the same but the 'Inquinante' column is cathegorized differently
# since the data are actually equal, we keep only one of them 
double_eq = dati_doppi[which(dati_doppi$NomeStazione=='Cigliano - Autostrada'), ]
table(double_eq$Inquinante)

obs_doppie = obs_doppie[-which(obs_doppie$NomeStazione=='Cigliano - Autostrada'), ]
N = dim(obs_doppie)[1]
for (i in 1:N){
  index = which(PM10_Piemonte$NomeStazione == obs_doppie$NomeStazione[i] & PM10_Piemonte$Data == obs_doppie$Data[i]
                & PM10_Piemonte$Inquinante == 'PM10 - Basso Volume - Microgrammi al metro cubo')
  PM10_Piemonte = PM10_Piemonte[-index, ]
}

date = unique(double_eq$Data)
N = dim(double_eq)[1]/2
for (i in 1:N){
  index = which(PM10_Piemonte$NomeStazione == double_eq$NomeStazione[i] & PM10_Piemonte$Data == date[i] 
                & PM10_Piemonte$Inquinante == "PM10 - Beta (media giornaliera) - Microgrammi al metro cubo" )
  PM10_Piemonte = PM10_Piemonte[-index, ]
}


# MISSING VALUES
length(which(n_obs$Freq==0)) 
# nearly 10.500 total missing values over 111.741

# Missing values by year
missing_P = rep(0,6)
missing_by_year = 365-table(PM10_Piemonte$Anno, PM10_Piemonte$NomeStazione)
missing_by_year = as.data.frame(missing_by_year)
for (i in 2014:2019){
  missing_P[i-2013] = sum(missing_by_year$Freq[which(missing_by_year$Var1==i)])
}
rbind(c(2014:2019), missing_P)
# Year with less missing values : 2016
# But since the number of missing values in 2018 is the second lowest value reported
# we choose to analyze the totality of data during 2018, coherently with other regions


# Keeping only useful variables
PM10_Piemonte = PM10_Piemonte[ , c(3,1,5,6,7,8,16,9,11,12,15,13)]
names(PM10_Piemonte) = c("Data", "NomeStazione", "Valore", "Anno", "Mese",       
                         "Giorno", "Wday", "Provincia", "Tipo", "Area",
                         "Zonizzazione", "Quota")
write.csv(PM10_Piemonte, 'PM10_Piemonte.csv')

rm(list = ls())


# DATASET 2018 ------------------------------------------------------------

emilia = read_csv('PM10_Emilia.csv')
emilia = emilia[which(emilia$Anno=="2018"), -1]
lombardia = read_csv('PM10_Lombardia.csv')
lombardia = lombardia[which(lombardia$Anno=="2018"), -1]
piemonte = read_csv('PM10_Piemonte.csv')
piemonte = piemonte[which(piemonte$Anno=="2018"), -1]

data_2018 = rbind(lombardia, emilia, piemonte)

# Check on stations : 163 stations
length(levels(factor(data_2018$NomeStazione)))
# Check on dates : OK
max(data_2018$Data) - min(data_2018$Data) + 1
length(levels(factor(data_2018$Data)))

# Functional Data : PM10 value by day by station
dati_2018 = xtabs(Valore ~ NomeStazione+Data, data = data_2018)
dati_2018 = as.matrix(dati_2018)
write.csv(dati_2018, 'dati_2018_ss.csv')
dati_2018 = read_csv('dati_2018_ss.csv')

matplot(t(dati_2018[ , -1]), type = 'l')
# functional data con pacchetto roahd
l = dim(dati_2018[ ,-1])[2]
data_fun = fData(1:l, dati_2018[, -1])
plot(data_fun)

# Plot by region
L_fun = xtabs(Valore ~ NomeStazione+Data, data = lombardia)
data_fun = fData(1:l, L_fun)
plot(data_fun)
title('PM10 during 2018 - Lombardia')

P_fun = xtabs(Valore ~ NomeStazione+Data, data = piemonte)
data_fun = fData(1:l, P_fun)
plot(data_fun)
title('PM10 during 2018 - Piemonte')

E_fun = xtabs(Valore ~ NomeStazione+Data, data = emilia)
data_fun = fData(1:(l-1), E_fun)
plot(data_fun)
title('PM10 during 2018 - Emilia')

