library("rjson")

#### CARICAMENTO  DATI####
list_dir = list.dirs(path = "./Data", full.names = TRUE, recursive = TRUE)
list_dir = list_dir[2:length(list_dir)]

list_data = list()
for (dir in list_dir) {
  a = fromJSON(file = paste(dir, "/data_try_1.json", sep=""))
  list_data = c(list_data, list(a))
}

#### CREARE TIBBLE ####

#### HIST CON DATI QUESTIONARIO ####

#### CALCOLO MISURE PER LIBRO ####
# ADEGUATEZZA MEDIA
# MAX
# MIN

#### AGGREGAZIONE TRA LIBRI ####
# ADEGUATEZZA MEDIA
# MSE 
# LIBRO CON ADEGUATEZZA MAX

#### GIUSTIFICAZIONI ####
# LUNGHEZZA MEDIA
# MAX
# MIN

#### DESCRIZIONE RISULTATI ####
# 2 DIM A PIACERE
# RISULTATI COMBO APPREZZAMENTO E POSSESSO DEL LIBRO
# RISULTATI COMBO APPREZZAMENTO E LETTURA DELL'AUTORE

###############################################################################

#### EXTRA ####
# AGGREGARE RISU QUEST CON DATI SUI LIBRI
# ANALISI DELLE RISPOSTE CON CODICE FRANCESCHET (SENTIMENT ANALYSIS)
# ANALISI TEMPI PER RISPOSTA
# CRT
