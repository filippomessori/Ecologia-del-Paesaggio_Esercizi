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




################# INTERPOLAZIONE ################3


# ESERCIZIO: ricreare la mappa a di densità a pattern puntuali del Covid.


setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio") 
load("point_patterns.Rdata")
library(spatstat)
ls() # vedere se ho richiamato il file giusto
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d,col=cl)
points(covids)

# Aggiungo coastlines
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
cl_Ex <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)
plot(d, col=cl_Ex, main="density") # main----> mi serve per dare un titolo al grafico
plot(coastlines, add=T)
points(covids)

### FINE ESERCIZIO #####


# Andiamo a fare l'Interpolazione dei valori, usando un algoritno di stima, per tutta l'estensione del pianeta, anche dove non abbiamo fatto le misure.
# Spieghiamo al sistema qual'è la variabile che vogliamo andare ad interpolare ----> Casi covid-19 all'interno del nostro dataset.

head(covid) # vedere i valori

# Funzione "marks" del point pattern creato in precedenza "covids":
marks(covids) <- covid$cases 

s <- Smooth(covids)
# Warning message:Least Squares Cross-Validation criterion was optimised at right-hand end of interval [0.323, 27.7]; use arguments ‘hmin’, ‘hmax’ to specify a wider interval for bandwidth ‘sigma’ 
# questo perchè è difficile fare una stima delle zone a densità molto bassa.
plot(s) # per vedere il grafico


# ESERCIZIO: fare plot(s) con point patterns e coastlines

cl_S <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)
plot(s, col=cl_S, main="Estimate of cases")
plot(coastlines, add=T)
points(covids)

# FINE ESERCIZIO

# Abbiamo una stima del numero di casi in Febbraio: vediamo che la maggior densità è in Cina


######### MAPPA DEFINITIVA :

par(mfrow=c(2,1))

# Mappa di Densità effettiva
plot(d, col=cl_Ex, main="Density")
plot(coastlines, add=T)
points(covids)

# Mappa "Casi stimati"
plot(s, col=cl_S, main="Estimate of cases")
plot(coastlines, add=T)
points(covids)



############ Esempio: SAN MARINO  ###############

# Richiamiamo dati
library(spatstat)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("Tesi.RData")
ls()
head(Tesi) # Vediamo i dati a disposizione
attach(Tesi) # alleghiamo al Work-Space la tabella

# Point pattern sui dati: x, y, c(xmin, xmax), c(ymin, ymax) -----> per vedere velocemente minimo e massimo funzione "summary"
summary(Tesi) 

# x (min= 12.42, max=12.46), y(min=43.91, max= 43.94)

# Creiamo il point pattern
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.90,43.95))

# Creiamo mappa di Densità
dT <- density(Tesippp)
plot(dT)
points(Tesippp)

# Maggiore densità nella zona centrale dell'area di studio---> in relazione ai prati aridi.


# Procediamo con l'interpolazione: funzione marks(), ad ogni punto del point-pattern associamo il valore di interessw cioè la "species richness"
marks(Tesippp) <- Tesi$Species_richness

# Creiamo un mappa di pixel continua tra un punto ed un altro, usando la funzione: Smooth(), grazie alla funzione marks() conosce quali sono i punti che deve inerpolare.
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp, col="green")

# Carichiamo nella cartella i file (Lab_ecologia_paesaggio) San_Marino e richiamiamo (o installo: install.packages) il pacchetto "rgdal"
library(rgdal)
sanmarino <- readOGR("San_Marino.shp") # carico il file di interesse 
plot(sanmarino) # confini San Marino

# aggiungiamo alla mappa l'interpolazione fatta precedentemente:
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T) # rimettere i confini sopra alla mappa di interpol

dev.off()

### ESERCIZIO 1: fare un plot multiframe della mappa con Interpolazione e densità

par(mfrow=c(2,1))
# Interpol
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T)
# Densità
plot(sanmarino)
plot(dT, add=T)
points(Tesippp, col="black")
plot(sanmarino, add=T)

### ESERCIZIO 2: fare stesso plot con 2 colonne ed 1 riga

par(mfrow=c(1,2))
# Interpol
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T)
# Densità
plot(sanmarino)
plot(dT, add=T)
points(Tesippp, col="black")
plot(sanmarino, add=T)
