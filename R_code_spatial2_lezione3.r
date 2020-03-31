###### R Spatial ######



# Richiamo libreria: library()
library(sp)

# Richiamo dataset da usare: data()
data(meuse)
head(meuse)

# Inserisco coordinate del dataset: coordinates()=~ , in questo caso le coordinate sono x;y
coordinates(meuse)=~x+y


# spplot delleo Zinco: spplot(), la variabile messa ""
spplot(meuse, "zinc")
#ottengo immagine spaziale di un fiume inquinato.


# Esercizio: fare spplot del rame
head(meuse)
# altro modo per vedere i nomi delle colonne names(meuse)
spplot(meuse, "copper")
# notiamo che ha valori molto simili a quelli dello zinco, dati molto correlati.


# bubble (), per vedere la stessa rappresentazione ma con i caratteri del grafico che variano la loro dimensione in base alla concentrazione dell'elemento di interesse
bubble(meuse, "zinc")


# Esercizio: fare bubble del rame colorato di rosso
bubble(meuse, "copper", col="red")



# Utilizzo di dati personali ----> esempio: analisi correlazione tra dati su foraminiferi e carbon stock.

# per inserire oggetto: foram o carbon <- c (dati)

# per prima cosa inseriamo i dati ottenuti dai foraminiferi
foram<- c(10, 20, 35, 55, 67, 80)

# Inseriamo i dati carbon stock
carbon<- c(5, 15, 30, 70, 85, 99)

# plottiamo insieme le due serie di dati così da avere righe e colonne ed ottenere un grafico.
plot(foram, carbon, col="green", cex=2, pch=19)






# Facciamo "dialogare" R con dati esterni: dati Covid-19
# da cartella "Ecologia_paesaggio_Lab) creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")


# leggere la tabella
covid <-read.table ("covid_agg.csv", head= TRUE)
