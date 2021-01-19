library("rjson")
library("tidyverse")
library("ggplot2")


#### CARICAMENTO  DATI####
list_dir = list.dirs(path = "./Data", full.names = TRUE, recursive = TRUE)
list_dir = list_dir[2:length(list_dir)]

list_data = list()
for (dir in list_dir) {
  a = fromJSON(file = paste(dir, "/data_try_1.json", sep=""))
  book = fromJSON(file = paste(dir, "/task_data.json", sep=""))
  a["documents_answers"][[1]][[1]] = c(a["documents_answers"][[1]][[1]], book["documents"][[1]][[1]]["nome"], book["documents"][[1]][[1]]["anno"])
  a["documents_answers"][[1]][[2]] = c(a["documents_answers"][[1]][[2]], book["documents"][[1]][[2]]["nome"], book["documents"][[1]][[2]]["anno"])
  a["documents_answers"][[1]][[3]] = c(a["documents_answers"][[1]][[3]], book["documents"][[1]][[3]]["nome"], book["documents"][[1]][[3]]["anno"])
  a["questionnaires_answers"][[1]][[1]] = c(a["questionnaires_answers"][[1]][[1]])
  a["questionnaires_answers"][[1]][[2]] = c(a["questionnaires_answers"][[1]][[2]])
  a["timesramps_elapsed"] = list(a["timesramps_elapsed"])[[1]]
  list_data = c(list_data, list(a))
}

#### CREARE TIBBLE ####
questionnaires = tibble()

for (input in list_data) {
  questionnaires = bind_rows(questionnaires, input["questionnaires_answers"][[1]][[1]])
}

crt = tibble()
for (input in list_data) {
  crt = bind_rows(crt, input["questionnaires_answers"][[1]][[2]])
}

books = tibble()
for (input in list_data) {
  books = bind_rows(books, input["documents_answers"][[1]][[1]])
  books = bind_rows(books, input["documents_answers"][[1]][[2]])
  books = bind_rows(books, input["documents_answers"][[1]][[3]])
}

times = tibble()
for (input in list_data) {
  times = bind_rows(times, input["timestamps_elapsed"])
}

#### HIST CON DATI QUESTIONARIO ####

questionario1 = questionnaires %>%
  select(sesso) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "0", replacement = "Uomo") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "Donna") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "2", replacement = "Non binario") %>%
  ggplot() +
  geom_bar(aes(sesso, fill=sesso)) +
  ggtitle("Questionario: Genere worker") +
  ylab("") +
  xlab("Sesso") +
  theme(legend.position = "none") 
  
questionario1
ggsave("questionario1.png")

questionario2 = questionnaires %>%
  select(tipologia) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "0", replacement = "Digitale") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "Cartaceo") %>%
  ggplot() +
  geom_bar(aes(tipologia, fill = tipologia)) +
  ggtitle("Questionario: Modalità di lettura preferita") +
  ylab("") +
  xlab("Modalità") +
  theme(legend.position = "none") 

questionario2
ggsave("questionario2.png")

questionario3 = questionnaires %>%
  select(genere) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "0", replacement = "Fantasy") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "Giallo") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "2", replacement = "Thriller") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "3", replacement = "Romanzo") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "4", replacement = "Avventura") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "5", replacement = "Rosa") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "6", replacement = "Altro") %>%
  ggplot() +
  geom_bar(aes(genere, fill = genere)) +
  ggtitle("Questionario: Genere letterario preferito") +
  ylab("") +
  xlab("Genere") +
  theme(legend.position = "none") 

questionario3
ggsave("questionario3.png")

questionario4 = questionnaires %>%
  select(preferenze) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "0", replacement = "Giornali") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "Libri") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "2", replacement = "Fumetti") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "3", replacement = "Altro") %>%
  ggplot() +
  geom_bar(aes(preferenze, fill = preferenze)) +
  ggtitle("Questionario: Preferenze tipologia di lettura") +
  ylab("") +
  xlab("Tipologia") +
  theme(legend.position = "none") 


questionario4
ggsave("questionario4.png")

questionario5 = questionnaires %>%
  select(giorni) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "3", replacement = "7") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "1-3 giorni") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "2", replacement = "4-6") %>%
  ggplot() +
  geom_bar(aes(giorni, fill=giorni)) +
  ggtitle("Questionario: Giorni dedicati a settimana alla lettura") +
  ylab("") +
  xlab("Giorni") +
  theme(legend.position = "none") 

questionario5
ggsave("questionario5.png")

questionario6 = questionnaires %>%
  select(tempo) %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "0", replacement = "0-4") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "5-9") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "2", replacement = "10-29") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "3", replacement = "30-59") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "4", replacement = "60+") %>%
  ggplot() +
  geom_bar(aes(tempo, fill = tempo)) +
  ggtitle("Questionario: Tempo dedicato ogni volta a questa attività") +
  ylab("") +
  xlab("Minuti") +
  theme(legend.position = "none") 


questionario6
ggsave("questionario6.png")

crt1 = crt %>%
  select(mazza) 
crt1 = map(crt1$mazza, function(x) toString(x))
crt1 = tibble(crt1) %>%
  ggplot() +
  geom_bar(aes(crt1, fill=crt1)) +
  ggtitle("CRT - risposta esatta 0.5") +
  ylab("") +
  xlab("Risposte")+
  theme(legend.position = "none") 

crt1
ggsave("crt.png")

#### CALCOLO MISURE PER LIBRO ####
# ADEGUATEZZA MEDIA
mean_books = books %>%
  select(nome, `Indica quanto ti sembra adeguato_value`) %>%
  group_by(nome) %>%
  summarise(media = sum(`Indica quanto ti sembra adeguato_value`)/n())
# MAX
max_books = books %>%
  select(nome, `Indica quanto ti sembra adeguato_value`) %>%
  group_by(nome) %>%
  summarise(max = max(`Indica quanto ti sembra adeguato_value`))
# MIN
min_books = books %>%
  select(nome, `Indica quanto ti sembra adeguato_value`) %>%
  group_by(nome) %>%
  summarise(min = min(`Indica quanto ti sembra adeguato_value`))

total_books = inner_join(mean_books, max_books, by=c("nome")) %>%
  inner_join(min_books) %>%
  gather("media", "max", "min", key = "tipo", value = "value") %>%
  ggplot(aes(x = nome, y = value, fill = tipo)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Libri: Valutazioni adeguatezza prezzi") +
  ylab("") +
  xlab("Libri")
  
total_books
ggsave("total_books.png")

ggplotly(total_books)

#### AGGREGAZIONE TRA LIBRI ####
# ADEGUATEZZA MEDIA
media_all = mean(unlist(mean_books["media"]))
# MSE
v_media = rep(media_all, 18)
mse = mean((v_media - books$`Indica quanto ti sembra adeguato_value`)^2)
# LIBRO CON ADEGUATEZZA MAX
max_book = max_books %>%
  select(nome, max) %>%
  filter(max == max(max))

#### GIUSTIFICAZIONI ####
# LUNGHEZZA MEDIA
giustificazioni = books["Che impressione hai di questa edizione_justification"]

split_giustificazioni = map(giustificazioni, function(x) strsplit(x," "))[[1]]
len = c()
for (el in split_giustificazioni){
  len = c(len, length(el))
}

mean_length = mean(unlist(len))

# MAX
max_length = max(unlist(len))
# MIN
min_length = min(unlist(len))

#### DESCRIZIONE RISULTATI ####
# 2 DIM A PIACERE
# RISULTATI COMBO APPREZZAMENTO E POSSESSO DEL LIBRO

libro = books %>%
  group_by(`Possiedi già questo libro, anche di un'altra edizione?_value`, `Il prezzo ti sembra adeguato?_value`) %>%
  summarise(n = n())
  summarise(mean = median(`Indica quanto ti sembra adeguato_value`))

books %>%
  summarise(mean = median(`Indica quanto ti sembra adeguato_value`))


  ggplot(aes(x=``, y=n, fill=`Acquisteresti questo libro?_value`)) +
  geom_bar(width = 1, stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("Acquisteresti questo libro?: Prezzo non adeguato - Autore conosciuto") +
  ylab("") +
  xlab("")

# RISULTATI COMBO APPREZZAMENTO E LETTURA DELL'AUTORE

autore = books %>%
  filter(`Il prezzo ti sembra adeguato?_value` == "-1" 
         & `Hai letto altri libri di questo autore?_value` == "1") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "-1", replacement = "No") %>%
  mutate_if(is.character,
            stringr::str_replace_all, pattern = "1", replacement = "Sì") %>%
  group_by(`Acquisteresti questo libro?_value`)%>%
  summarise(n = n()) %>%
  ggplot(aes(x=`Acquisteresti questo libro?_value`, y=n, fill=`Acquisteresti questo libro?_value`)) +
  geom_bar(width = 1, stat = "identity") +
  theme(legend.position = "none") +
  ggtitle("Acquisteresti questo libro?: Prezzo non adeguato - Autore conosciuto") +
  ylab("") +
  xlab("")

autore
ggsave("autore.png")

###############################################################################

#### EXTRA ####
# AGGREGARE RISU QUEST CON DATI SUI LIBRI
# ANALISI DELLE RISPOSTE CON CODICE FRANCESCHET (SENTIMENT ANALYSIS)
# ANALISI TEMPI PER RISPOSTA
prova = c()
for (i in c(1:12)){
  prova = c(prova, c("Questionario","CRT","Libro","Libro","Libro"))
}
prova2 = c()
for (i in c(1:12)){
  prova2 = c(prova2, rep(i, 5))
}
times2 = tibble(prova, times) %>%
  tibble(prova2) %>%
  group_by(prova) %>%
  summarise(mean=mean(timestamps_elapsed))

times3 =  tibble(prova, times) %>%
  tibble(prova2) %>%
  group_by(prova2) %>%
  summarise(sum=sum(timestamps_elapsed)) %>%
  summarise(mean = mean(sum))


books %>%
  filter(`Possiedi già questo libro, anche di un'altra edizione?_value` == "1" & `Il prezzo ti sembra adeguato?_value` == "1")%>%
  select(nome, `Quante pagine ha questo libro?_justification`)
books%>%
  filter(`Possiedi già questo libro, anche di un'altra edizione?_value` == "1")%>%
  count(nome)