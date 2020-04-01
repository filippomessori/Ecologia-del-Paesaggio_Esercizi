###### Codice per analisi dei point patterns ######





# Per prima cosa installo i pacchetti dati che andrò ad utilizzare: install.packages()

install.packages("ggplot2")
install.packages("spatstat")

# Richiamo i dati esterni ad R: setwd("/Users/nome_utente/Desktopo/lab"), per utenti Mac con cartella salvata sulla scrivania
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio") 

# leggere la tabella del file, così da importare i dati: <-read.table ()
covid <-read.table ("covid_agg.csv", head= TRUE)

# richiamo la tabella su R
head(covid)

# faccio un un plot dei miei dati, relativi a country (asse X) e n° di casi (asse Y): plot()
plot(covid$country, covid$cases)

# metodo alternativo per il plot dei dati, senza dovere inserire il "$":
attach(covid)
plot(country, cases)

# impostare visualizzazione delle labels sul grafico, in base a comodità di lettura/analisi:
plot(covid$country, covid$cases, las=0) # parallel labels
plot(covid$country, covid$cases, las=1) # horizontal labels
plot(covid$country, covid$cases, las=2) # perpendicular labels
plot(covid$country, covid$cases, las=3) # vertical labels

# inoltre possiamo adattare dimensioni scritte in base allo schermo del nostro CPU, per facilitare la visualizzazione:
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5)


# RISULTATO: Osserviamo il numero di casi relativo ai vari paesi del mondo, la Cina ha il numero più alto di casi.






# Richiamo il pacchetto ggplot2:

library(ggplot2)
data(mpg)
head(mpg)

# Analisi puntuale grafica dei dati
# Facciamo un ggplot, per prima cosa dichiariamo i dati (mpg), poi l'estetica (aes) ed infine sommiamo la geometria (geom_point)
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# per avere linee o poligoni al posto dei punti nel grafico:
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line() #linee
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon() #poligoni



# Utilizziamo ggplot2 per analizzare i dati del Covid-19:

# Per prima cosa richiamo le variabili che mi interessano:
names(covid)
head(covid)

#  ggplot di Covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# Risultato: grafico a pattern puntuali in base ai casi di Covid-19 nel mondo


# ESERCIZIO: Creare una mappa di densità dei dati relativi al Covid
# Consiste in un passaggio da un sistema punti (vettoriale) ad un sistema Raster: distinzione maggiore densità (colore più scuro) e minore densità (colore più chiaro)

# Per fare questo uso il pacchetto "spatstat" (installato precedentemente):

# richiamo spatstat
library(spatstat)

# Creo il dataset da utilizzare con spatstat: <- ppp(lon, lat, c(), c())
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
d <- density(covids)

# faccio il plot del dataset creato: plot()

plot(d)
# ottengo mappa densità

points(covids)
# aggiungo elementi puntuali al grafico


# Salvo il mio WorkSpace nella cartella Lab: nome file "point_patterns.Rdata"


#############################################################################2


# Carico il file salvato "point_patterns.Rdata" dalla cartella Lab: setwd("/Users/nome_utente/Desktopo/lab") e load("nome_file.Rdata")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("point_patterns.Rdata")
ls()

# Richiamo la libreria "spatstat" per calcolare la densità
library(spatstat)

# Ricreo mappa densità Covid-19
plot(d)

# Cambiare colori e personalizzare grafico: palette di colori, cl <- colorRampPalette(c('serie_di_colori')) (numero di gradazione di colori)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d,col=cl)


# ESERCIZIO: fare la stessa cosa associando la densità a colore verde e blu

cl2 <- colorRampPalette(c('green', 'blue')) (100)
plot(d, col=cl2)
# carico i punti sulla mappa: points()
points(covids)


## Ora carichiamo dei dati geografici dall'esterno, per visualizzare i confini di stato sulla mappa

# 1. carichiamo i file coastlines su cartella "Lab_ecologia_paesaggio"
# 2. creiamo l'oggetto coastlines, prima però installo rgdal e successivamente  richiamo il pacchetto library():
install.packages("rgdal")
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")


# Ora facciamo il plot delle coastlines con i dati del covid-19, aggiungiamo al plot precedente, cioè "plot(d, col=cl2)": plot(coastlines, add=T)
plot(coastlines, add=T)
#otteniamo una mappa di densità del covid coi punti nel mondo, densità massima in Europa.


# Plot di una mappa di densità con le coastlines cambiando i colori:
cl_Ex <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)
plot(d, col=cl_Ex)
plot(coastlines, add=T)




