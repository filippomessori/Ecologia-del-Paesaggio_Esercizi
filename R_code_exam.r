################ ESAME ECOLOGIA DEL PAESAGGIO #################  Studente: Filippo Messori  (FM)  ######## a.a 2019-2020



# 1) PRIMO CODICE R
# 2) R SPATIAL: Funzioni spaziali in Ecologia del Paesaggio
# 3) R SPATIAL: Importazione dati
# 4) POINT PATTERNS: Codice per analisi dei point patterns 
# 5) TELERILEVAMENTO
# 6) LAND COVER 
# 7) Analisi MULTITEMPORALE della VARIAZIONE di Land-Cover
# 8) ANALISI NO2 data presi da ESA
# 9) Analisi SNOW-COVER
# 10) R_code_Patches
# 11) R_code_Crop
# 12) SPECIES DISTRIBUTION MODELLING
# 13) PROGETTO ESAME: FCOVER - FAPAR – NDVI: Analisi Multitemporale (2000 - 2019)

################################
####################################
##############################################


# 1) PRIMO CODICE R

install.packages("sp") # comando per installare sulla piattaforma "R" pacchetti di dati e funzioni (FM)

# Pacchetto "sp":  fornisce classi e metodi per analisi dei dati spaziali (es: punti, linee, poligoni e griglie) (FM)

Library(sp) # comando per caricare set di dati o funzioni precedentemente installati. (FM)
#  Da inserire tutte le volte, che aperto un nuovo progetto, si vuole lavorare su un determinato pacchetto di dati o funzioni (FM)
# require (sp) è un altro modo per far partire librerie (FM)

data(meuse) # Caricare Set di dati fornisce che posizioni e concentrazioni di metalli pesanti nel terreno raccolte nel campionamento del fiume Meuse, Stein (NL) (FM)

meuse # Comando per osservare il dataset su R (FM)

head(meuse) # Visualizzare le prime righe del dataset (FM)

names(meuse) # Visualizzare i nomi delle variabili  (FM)

summary(meuse) # Analisi statistica generica (moda, media, mediana ecc) di tutte le variabili del dataset (FM)

pairs(meuse) # Funzione per correlare le variabili del dataset. Ogni variabile viene messa in correlazione con le altre (FM)

pairs(~ cadmium + copper + lead , data=meuse) # Selezione delle variabili specifiche che voglio andare a correlare. In questo caso: cadmium + copper + lead (FM)

# ESERCIZIO: pairs con tutti gli elementi 
pairs(~ cadmium + copper + lead + zinc , data=meuse) # (FM)


pairs(meuse[,3:6])
# per selezionare piu velocemente le variabili interessate scegliendo le colonne, mi viene fuori lo stesso grafico di prima. (FM)


pairs(meuse[,3:6], col="red")
#cambiare colore ----> col="" (FM)


# pointcharater caratterizzare/oersonalizzare i punti del grafico: "pch="  (FM)
pairs(meuse[,3:6], col="red", pch=19)


pairs(meuse[,3:6], col="red", pch=19, cex=3) # aumentare dimensione punti: "cex="  (FM)


pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs") # Mettere il titolo al grafico: main="" (FM)


# EXERCISE: Fare la stessa cosa con elevation e gli altri elementi

pairs(meuse[,3:7], col="red", pch=19, cex=3, main="Secondo pairs") # (FM)


# COPIARE codice da un altro : codice Prof. Duccio Rocchini 

panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

# lower.paenel= parte inferiore pannello---> scegliamo cosa metterci= Correlazioni che io avevo scritto,  upper.panel= panel dot smoothing, (FM)
# diag.panel=metto quello che manca, cioè istogrammi. (FM)


# EXERCISE: cambiare ordine pannelli nel grafico

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correletions, diag.panel = panel.histograms) # (FM)


# funzione PLOT lancia la funzione -----> funzione per rappresentare rappresentare graficamente su R i dati che stiamo analizzando. Mostra sempre correlazioni tra 2 varibili su asse x e y. (FM)
# dati provengono da un dataset. Per rappresentare le singole colonne del dataset si usa l’operatore "$" per indicare a quale di esse ci riferiamo. (FM)

plot(meuse$cadmium, meuse$copper)  

# "attach" associa senza il dollaro (FM)
attach(meuse)
plot(cadmium, copper)

# caratterizzare il grafico
plot(cadmium, copper, pch=17, col="green", main= "primo plot")

# cambiare Labels, etichette grafico ----> xlab="" e ylab=""  (FM)
plot(cadmium, copper, pch=17, col="green", main= "primo plot", xlab="cadmio", ylab="rame")

# cambiare dimensioni labels e punti: cex.lab="" (FM)
plot(cadmium, copper, pch=17, col="green", main= "primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2)


##################################################
###################################################
#######################################################


# 2) R Spatial: Funzioni spaziali in Ecologia del Paesaggio


# richiamare pachetto "sp" con library() (FM)

library(sp)

# Richiamo dati: data() (FM)

data(meuse)
head(meuse)


# Plot cadmium e lead: plot(), lo caratterizziamo con colore punti col(), caratterizzazione punti pch(), dimensione punti cex() (FM)
# alleghiamo dataframe: attach() (FM)

attach(meuse)
plot(cadmium,lead,col="red", pch=19,cex=2)


# ESERCIZO: plot di copper e zinco con simbolo triangolo (pch 17) e colore verde, cex=2

plot(copper, zinc,col="green", pch=17, cex=2) # (FM)


# cambiare le etichette del grafico : labels-----> xlab="" , ylab="" (FM)

plot(copper, zinc,col="green", pch=17, cex=2, xlab="Rame", ylab="Zinco")



# MULTIFRAME o Multipanel: importantissima per fare un confronto tra grafici diversi, par(mfrow=c("numero_righe","numero_colonne")) (FM)
par(mfrow=c(1,2))
plot(copper, zinc,col="green", pch=17, cex=2)
plot(cadmium,lead,col="red", pch=19,cex=2)


# invertiamo i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(copper, zinc,col="green", pch=17, cex=2)
plot(cadmium,lead,col="red", pch=19,cex=2)



# Multiframe automatico
# "ggplot2" pacchetto che mi permettedi sviluppare modelli grafici a partire dal dataset che viene fornito (FM)
# "GGally" estende 'ggplot2' aggiungendo diverse funzioni per ridurre la complessità della combinazione di oggetti geometrici con dati trasformati. (FM)

install.packages("GGally") 

# richiamo pacchetto con library

library(GGally)
# funzione ggpairs del dataset(meuse), però il dataset è troppo grande quindu seleziono le colonne di interesse apprendo [] e digitando numero colonna di interesse (FM)

pairs(meuse[,3:6])
#ottengo un grafico che mi indica la correlazione tra le variabili, usando il coefficiente di Spearman (0-1)---> + è alto + vi è correlazione (FM)



# SPATIAL: pima cosa dobbiamo dire al software che il dataset ha delle coordinate, in questo caso la X e la Y; usiamo la funzione coordinates()=~ (FM)
head(meuse)

# Spieghiamo che le coordinate di questo dataset sono X e Y (FM)
coordinates(meuse)=~x+y


# otteniamo con plot(meuse) una distribuzione dei nostri punti nello spazio FM)
plot(meuse)


# Funzione spplot = plottare i miei dati spazialmente (le variabili in spplot vanno messe ""), ed otteniamo il primo grafico spaziale riferito allo zinco. (FM)

spplot(meuse, "zinc")
# osserviamo un grafico che mi dice il quantitativo di Zinco relativo a vari punti di un fiume. (FM)


#######################################################
#######################################################
#######################################################

### 3) R SPATIAL: Iportare dati da esterno.



# Richiamo libreria, pacchetto "sp": library() (FM)
library(sp)

# Richiamo dataset da usare: data() (FM)
data(meuse)
head(meuse)

# Inserisco coordinate del dataset: coordinates()=~ , in questo caso le coordinate sono x;y (FM)
coordinates(meuse)=~x+y


# spplot delleo Zinco: spplot(), la variabile messa "" (FM)
spplot(meuse, "zinc")
# ottengo immagine spaziale di un fiume inquinato. (FM)


# Esercizio: fare spplot del rame

head(meuse) # (FM)
# altro modo per vedere i nomi delle colonne names(meuse) (FM)
spplot(meuse, "copper")
# notiamo che ha valori molto simili a quelli dello zinco, dati molto correlati. (FM)


# Funzione "bubble ()", vedere la stessa rappresentazione ma con i caratteri del grafico che variano la loro dimensione in base alla concentrazione dell'elemento di interesse (FM)
bubble(meuse, "zinc")


# Esercizio: fare bubble del rame colorato di rosso

bubble(meuse, "copper", col="red") # (FM)



# Utilizzo di dati personali ----> esempio: analisi correlazione tra dati su foraminiferi e carbon stock. (FM)

# per inserire oggetto: foram o carbon <- c (dati) (FM)

# per prima cosa inseriamo i dati ottenuti dai foraminiferi (FM)
foram<- c(10, 20, 35, 55, 67, 80)

# Inseriamo i dati carbon stock (FM)
carbon<- c(5, 15, 30, 70, 85, 99)

# plottiamo insieme le due serie di dati così da avere righe e colonne ed ottenere un grafico. (FM)
plot(foram, carbon, col="green", cex=2, pch=19)



# Facciamo "dialogare" R con dati esterni: dati Covid-19 (FM)
# da cartella "Ecologia_paesaggio_Lab) creata su Mac (scrivania) uso il comando: setwd("/Users/nome_utente/Desktop/nome_cartella") (FM)

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")


# leggere la tabella: funzione "read.table" (FM)
covid <-read.table ("covid_agg.csv", head= TRUE)



##########################################
###########################################
######################################################



### 4) POINT PATTERNS: Codice per analisi dei point patterns 


# Per prima cosa installo i pacchetti dati/funzioni che andrò ad utilizzare: install.packages() (FM)

install.packages("ggplot2")  # "ggplot2": pacchetto che mi permettedi sviluppare modelli grafici a partire dal dataset che viene fornito (FM)
install.packages("spatstat") # "spatstat": funzioni open-source per l'analisi di modelli spaziali a Point Patterns (FM)

# Richiamo i dati esterni ad R: setwd("/Users/nome_utente/Desktopo/lab"), per utenti Mac con cartella salvata sulla scrivania (FM)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio") 

# leggere la tabella del file, così da importare i dati: <-read.table () (FM)
covid <-read.table ("covid_agg.csv", head= TRUE)

# richiamo la tabella su R
head(covid)

# faccio un un plot dei miei dati, relativi a country (asse X) e n° di casi (asse Y): plot() (FM)
plot(covid$country, covid$cases)

# metodo alternativo per il plot dei dati, senza dovere inserire il "$" (FM)
attach(covid)
plot(country, cases)

# impostare visualizzazione delle labels sul grafico, in base a comodità di lettura/analisi (FM)
plot(covid$country, covid$cases, las=0) # parallel labels
plot(covid$country, covid$cases, las=1) # horizontal labels
plot(covid$country, covid$cases, las=2) # perpendicular labels
plot(covid$country, covid$cases, las=3) # vertical labels

# inoltre possiamo adattare dimensioni scritte in base allo schermo del nostro CPU, per facilitare la visualizzazione ---> "cex.lab=" e "cex.axis=" (FM)
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5)


# RISULTATO: Osserviamo il numero di casi relativo ai vari paesi del mondo, la Cina ha il numero più alto di casi. (FM)



##################### PARTE 2: 

# Richiamo i dati esterni ad R: setwd("/Users/nome_utente/Desktopo/lab"), per utenti Mac con cartella salvata sulla scrivania (FM)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio") 

# Richiamo il pacchetto ggplot2:

library(ggplot2)
data(mpg)
head(mpg)

# Analisi puntuale grafica dei dati (FM)
# Facciamo un ggplot: per prima cosa dichiariamo i dati (mpg), poi l'estetica (aes) ed infine sommiamo la geometria (geom_point) (FM)
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# per avere linee o poligoni al posto dei punti nel grafico (FM)
ggplot(mpg,aes(x=displ,y=hwy)) + geom_line() #linee
ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon() #poligoni



# Utilizziamo ggplot2 per analizzare i dati del Covid-19 (FM)

# Per prima cosa scelgo le variabili che mi interessano (FM)
names(covid)
head(covid)

#  ggplot di Covid
ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# Risultato: grafico a pattern puntuali in base ai casi di Covid-19 nel mondo (FM)


# ESERCIZIO: Creare una mappa di densità dei dati relativi al Covid
# Consiste in un passaggio da un sistema punti (vettoriale) ad un sistema Raster: distinzione maggiore densità (colore più scuro) e minore densità (colore più chiaro) (FM)

# Per fare questo uso il pacchetto "spatstat" (installato precedentemente): (FM)

# richiamo spatstat (FM)
library(spatstat)

# Creo il dataset da utilizzare con spatstat: <- ppp(lon, lat, c(), c()) (FM)
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
d <- density(covids)

# faccio il plot del dataset creato: plot() (FM)

plot(d)
# ottengo mappa densità (FM)

points(covids)
# aggiungo elementi puntuali al grafico (FM)


# Salvo il mio WorkSpace nella cartella Lab: nome file "point_patterns.Rdata" (FM)


############################################################################# PARTE 3


# Carico il file salvato "point_patterns.Rdata" dalla cartella Lab: setwd("/Users/nome_utente/Desktopo/lab") e load("nome_file.Rdata") (FM)

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("point_patterns.Rdata")
ls() # lista di tutti gli oggetti creati nel WorkSpace appena caricato (FM)

# Richiamo la libreria "spatstat" per calcolare la densità 
library(spatstat)

# Ricreo mappa densità Covid-19
plot(d)

# Cambiare colori e personalizzare grafico: palette di colori, cl <- colorRampPalette(c('serie_di_colori')) (numero di gradazione di colori) (FM)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d,col=cl)


# ESERCIZIO: fare la stessa cosa associando la densità a colore verde e blu

cl2 <- colorRampPalette(c('green', 'blue')) (100) 
plot(d, col=cl2)                                           # (FM)
# carico i punti sulla mappa: points()
points(covids)


## Ora carichiamo dei dati geografici dall'esterno, per visualizzare i confini di stato sulla mappa (FM)

# 1. carichiamo i file coastlines su cartella "Lab_ecologia_paesaggio"
# 2. creiamo l'oggetto coastlines, prima però installo rgdal e successivamente richiamo il pacchetto library() (FM)
install.packages("rgdal") # pacchetto necessario per importare su R file Raster e Vettoriali, per le analisi spaziali. Dati Georeferenziati (FM)
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")


# Ora facciamo il plot delle coastlines con i dati del covid-19, aggiungiamo al plot precedente, cioè "plot(d, col=cl2)": plot(coastlines, add=T) (FM)
plot(coastlines, add=T)
#otteniamo una mappa di densità del covid coi punti nel mondo, densità massima in Europa. (FM)


# Plot di una mappa di densità con le coastlines cambiando i colori (FM)
cl_Ex <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)
plot(d, col=cl_Ex)
plot(coastlines, add=T)


################# INTERPOLAZIONE ################ PARTE 4


# ESERCIZIO: ricreare la mappa a di densità a pattern puntuali del Covid.


setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio") 
load("point_patterns.Rdata")
library(spatstat)
ls() # vedere se ho richiamato il file giusto
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)                         # (FM)
plot(d,col=cl)
points(covids)

# Aggiungo coastlines
library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
cl_Ex <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)                               # (FM)
plot(d, col=cl_Ex, main="density") # main----> mi serve per dare un titolo al grafico
plot(coastlines, add=T)
points(covids)

### FINE ESERCIZIO #####


# Andiamo a fare l'Interpolazione dei valori, usando un algoritmo di stima, per tutta l'estensione del pianeta, anche dove non abbiamo fatto le misure. (FM)
# Spieghiamo al sistema qual'è la variabile che vogliamo andare ad interpolare ----> Casi covid-19 all'interno del nostro dataset. (FM)

head(covid) # vedere i valori (FM)

# Funzione "marks" del point pattern creato in precedenza "covids". Estrarre e modificare dati creati in precedenza. (FM)
marks(covids) <- covid$cases 

s <- Smooth(covids) --------> # Spatial MOdellization  ------> Modelizzazione spaziale in base ai dati relativi al Covid-19 (FM)
# Warning message:Least Squares Cross-Validation criterion was optimised at right-hand end of interval [0.323, 27.7]; use arguments ‘hmin’, ‘hmax’ to specify a wider interval for bandwidth ‘sigma’ 
# questo perchè è difficile fare una stima delle zone a densità molto bassa. (FM)

plot(s) # per vedere il grafico


# ESERCIZIO: fare plot(s) con point patterns e coastlines

cl_S <- colorRampPalette(c('blue', 'light blue', 'green', 'light green')) (100)
plot(s, col=cl_S, main="Estimate of cases")
plot(coastlines, add=T)
points(covids)

# FINE ESERCIZIO

# Abbiamo una stima del numero di casi in Febbraio: vediamo che la maggior densità è in Cina (FM)


######### MAPPA DEFINITIVA :

par(mfrow=c(2,1)) # Per mettere insieme le mappe di analisi Densità e di Stima (FM)

# Mappa di Densità effettiva (FM)
plot(d, col=cl_Ex, main="Density")
plot(coastlines, add=T)
points(covids)

# Mappa "Casi stimati" (FM)
plot(s, col=cl_S, main="Estimate of cases")
plot(coastlines, add=T)
points(covids)


############ Esempio: SAN MARINO  ###############

# Richiamiamo dati
library(spatstat)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("Tesi.RData") # Importati precedentemente nella cartella "Lab_ecologia_del_Paesaggio"
ls()
head(Tesi) # Vediamo i dati a disposizione (FM)
attach(Tesi) # alleghiamo al Work-Space la tabella (FM)

# Point pattern sui dati: x, y, c(xmin, xmax), c(ymin, ymax) -----> per vedere velocemente minimo e massimo funzione "summary" (FM)
summary(Tesi) 

# x (min= 12.42, max=12.46), y(min=43.91, max= 43.94) (FM)

# Creiamo il point pattern
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.90,43.95))  # "ppp" oggetto rappresentante un dataset a pattern puntuali (FM)

# Creiamo mappa di Densità (FM)
dT <- density(Tesippp)
plot(dT)
points(Tesippp)

# Maggiore densità nella zona centrale dell'area di studio---> in relazione ai prati aridi. (FM)


# Procediamo con l'interpolazione: funzione marks(), ad ogni punto del point-pattern associamo il valore di interessw cioè la "species richness" (FM)
marks(Tesippp) <- Tesi$Species_richness

# Creiamo un mappa di pixel continua tra un punto ed un altro, usando la funzione: Smooth(), grazie alla funzione marks() conosce quali sono i punti che deve interpolare. (FM)
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp, col="green")

# Carichiamo nella cartella i file (Lab_ecologia_paesaggio) San_Marino e richiamiamo (o installo: install.packages) il pacchetto "rgdal" (FM)
library(rgdal)
sanmarino <- readOGR("San_Marino.shp") # carico il file di interesse (FM)
plot(sanmarino) # confini San Marino (FM)

# aggiungiamo alla mappa l'interpolazione fatta precedentemente , relativa ai dati della Tesi (FM)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T) # rimettere i confini sopra alla mappa di interpol (FM)

dev.off() # Chiudere il grafico (FM)


### ESERCIZIO 1: fare un plot multiframe della mappa con Interpolazione e densità

par(mfrow=c(2,1))
# Interpol
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")                    # (FM)
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
# Densità                                           (FM)
plot(sanmarino)
plot(dT, add=T)
points(Tesippp, col="black")
plot(sanmarino, add=T)


#########################################
#########################################
#########################################




### 5) TELERILEVAMENTO


# Codice R per analisi di immagini satellitari



# Scarico pacchetti raster: Pacchetto per analisi e modellizzazione dei dati geografici ---> Leggere, scrivere, manipolare e analizzare dati spaziali. (FM)
install.packages("raster") 

# Richiamo pacchetto:
library(raster)

# Richiamo wd: per Mac setwd("/Users/nome_utente/Desktop/Lab")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Richiamo l'immagine di telerilevamento di interesse presente nella mia cartella, specificando l'anno in cui il dato è stato telerilevato (FM)
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# Facciamo un plot dell'immagine satellitare: plot(nome_oggetto) (FM)
plot(p224r63_2011)
# Otteniamo un risultato della stessa area anallizzata da 7 diversi sensori che analizzano su diverse lunghezze d'onda, le seguenti: (FM)

# B1= blu, B2= verde, B3= Rosso, B4= infrarosso vicino(NIR), B5: medium infrared, B6: thermal infrared, B7: medium infrared. (FM)


# Adesso settiamo i colori per i grafici, facendo una colorRampPalette: cl <- colorRampPalette(c('black','grey','light grey'))(100) # e poi facciamo un plot (FM)
cl <- colorRampPalette(c('black','grey','light grey'))(100) #
plot(p224r63_2011, col=cl)

# se vogliamo usare una risoluzione diversa, più bassa per esempio (FM)
cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011, col=cllow)



# ColorRamp per i colori nella banda del Blu: B1,  per prima cosa uso names() per avere i nomi dei vari grafici (FM)
names(p224r63_2011)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
#attach(), non funziona con il dataset "raster", quindi dobbiamo mettere un simbolo($) che lega la colonna al dataset (FM)


# ESERCIZIO: plottare banda infrarosso vicino con colorRamp che varia dal rosso, arancione, giallo

clNIR <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clNIR)
# valori molto alti, quindi probabilmente molta vegetazione nell'area studio----> attività fotosintetica  (FM)




# Multiframe, con graficici nelle varie lunghezze d'onda colorati, per le lunghezze: Blu, Verde, Rosso, NIR  (FM)

par(mfrow=c(2,2))

# per il blu
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

# Per il verde
clG <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clG)

# per il rosso
clR <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_2011$B3_sre, col=clR)

# per il NIR
clNIR <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clNIR)

# Chiudere immagini create
dev.off()



# NATURAL COLOURS, con tre componenti : R G B (per vedere i colori come li vedrebbe l'occhio umano) (FM)
# Costruiamo 3 bande: r= banda rosso, g= banda verde, b= banda del blu -----> plotRGB() -------> RGB sta per Red-Green-Blue
# r=3, g=2, b=1 (FM)
plotRGB(p224r63_2011, r=3, g=2, b=1)

# Per ottenere un'immagine miglore facciamo un "stretch" (FM)
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")


# A questo punto per passare alla visualizzazione della banda del NIR scaliamo di 1 le posizioni di r, g, b, appena settate: (FM)
# r diventa 4, g diveenta 3, b diventa 4 (FM)
# Creiamo una composizione "false colors", questo per mettere la componente NIR (che altrimenti non sarebbe visibile) sulla banda del rosso (FM)


plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Con questo settaggio il suolo "nudo" diventa azzurrino, così da distinguerlo dalla vegetazione. (FM)
# Possiamo quindi svolgere uno studio miglore della zona di interesse (FM)


# salvare pdf grafico creato (FM)

pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()



# Multiframe dei 2 grafici precedentemente creati:

par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


# Ora montiamo l'infrarosso su altre componenti (prima l'abbiamo fatto nel rosso): ESERCIZIO -----> NIR nella componente green

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # (FM)

# NIR nella componente blu

plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # (FM)


# Salvo il working-space nella cartella "Lab_ecologia_paesaggio" come : Remote_sensing2.Rdata (FM)



########### PARTE 2


# Richiamiamo libreria "raster" e la working-directory, e i dati elaborati nella prima parte: (FM)
library(raster) 
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("Remote_sensing2.Rdata")
ls() # list per vedere i miei file (FM)



# Richiamo i dati del telerilevamento relativi al 1988 ( satellite Landsat)----> i dati devono essere all'interno della cartella "Lab_ecologia_paesaggio" (FM)
p224r63_1988 <- brick("p224r63_1988_masked.grd") # brick: Oggetto formato da più layer di raster (FM)

# Plottiamo i nuovi dati: (FM)
plot(p224r63_1988)


# Eseguiamo le stesse operazioni fatte con i dati 2011:

# Rcordiamo le bande dei sensori:
# B1: blue - 1
# B2: green - 2                                        (FM)
# B3: red - 3
# B4: near infrared (nir) - 4

# multiframe
par(mfrow=c(2,2))

# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_1988$B1_sre, col=clb)

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100) # 
plot(p224r63_1988$B2_sre, col=clg)

# red
clr <- colorRampPalette(c('dark red','red','pink'))(100) # 
plot(p224r63_1988$B3_sre, col=clr)

# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(p224r63_1988$B4_sre, col=clnir)

# Chiudiamo le immagini 
dev.off()


# Facciamo un plotRGB, per vedere i grafici con i colori naturali: (FM)
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")




# ESERCIZIO: fare un plot usando la componente NIR al posto della r in RGB space:

plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # (FM)




# Ora confrontiamo le immagini Landsat 1988 con quelle del 2011: (FM)

par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()


# Ora possiamo considerare lo stress di una pianta, così da capire se è sana. (FM)
# Facciamo questo tramite Indici di vegetazione: Sappiamo che una pianta SANA riflette molto nel Nir e nel green, e poco nel Red e nel blue (alta assorbanza---> Fotosintesi). (FM)
# In una pianta "sotto stress" avremo bassi valori di riflettanza per Nir e green e valori di riflettanza più alti per Red e blue. (FM)

# Indice più utilizzato in questi casi è il DVI (Difference Vegetation Index)------> DVI= NIR - R           (FM)

# DVI per 1988: dvi1988= Nir_1988 - R_1988 (sottraiamo in questo modo pixel per pixel)                  (FM)

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)



# ESERCIZIO: colcolare DVI per immagine relativa a dati 2011

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre                # (FM)
plot(dvi2011)


# Usiamo un'altra colorRamp, possiamo prendere una di quelle fatte precedentemente, per esempio: (FM)
cldvi <- colorRampPalette(c('light blue','light green','green'))(100) # 
plot(dvi2011, col=cldvi)




# Confrontando i 2 DVI possiamo considerare se la vegetazione era meno stressata nel 1988 o nel 2011, così da avere una base di partenza per valutare i fattori che incidono sulla vegetazione dell'area (FM)
# Per fare questo facciamo una "multi-temporal analysis":

difdvi <- dvi2011 - dvi1988    # Differenza tra DVI del 2011 e DVI del 1988 nella stessa area (FM)
plot(difdvi)

# Creiamo una colorRamp:
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)

# Dal grafico vediamo che: le immagini in blu sono le piante che stanno meglio, quelle in rosso quelle che stanno peggio, quelle in bianco quelle che sono rimaste stabili. (FM)

# Chiudiamo il grafico:
dev.off()


# Ora vogliamo visualizzzare gli output in 3 grafici differenti----> Grafico 1988, Grafico 2011 e grafico "multi-temporal analysis" (difdvi): (FM)
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

dev.off()




# Cambiare la RISOLUZIONE dell'immagine, variamo per esempio un pixel di 30 metri con un factor=10 in un pixel di 300m -----> Quindi possiamo variare la dimensione dei Pixel all'interno di un'immagine -------> determinare la risoluzione dell'immagine (FM)
# Per esempio:

p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # aggregate= aggregazione oggetti spaziali (FM)

p224r63_2011lr  # per vedere informazioni relative alla nuova immagine (FM)

# Ora richiamiamo i plotRGB fatti precedentemente e vediamo come sono le immagini con le 2 differenti risoluzioni: (FM)
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# Creiamo un oggetto ad una risoluzione ancora più bassa----->  50x30= 1500 (FM)
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50 # per vedere informazioni relative alla nuova immagine (FM)

# Inseriamo anche questa risoluzione nella visualizzazione del nostro paesaggio: Stessa immagine a 3 risoluzioni diverse (FM)

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# Ovviamente più la grana è fine (pixel piccolo) e più si riescono a notare i particolari relativi alla vegetazione. (FM)

dev.off()



# Campioniamo a risoluzione più bassa anche dati per 1988: (FM)

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)

# Calcoliamo DVI anche per 1988, così da poter fare un confronto con il DVI del 2011: (FM)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

# Plot dei 2 anni insieme:
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

# Risultato : Immagini del confronto tra DVI (vegetazione2011 e vegetazione1988) in 2 risoluzioni differenti, in quella a più alta risoluzione possiamo riconoscere quelle micro-sitazioni che hanno generato il cambiamento. (FM)


###################################################
###################################################
###################################################




### 6) LAND COVER 


# Richiamo wd: per Mac setwd("/Users/nome_utente/Desktop/Lab") 
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Richiamo pacchetto:
library(raster)


# Installo pacchetto "RStoolbox": Pacchetto con funzioni di analisi dati per il Remote-Sensing (Telerilevamento)       (FM)

install.packages("RStoolbox")
library(RStoolbox)


# Funzione brick : delle immagini che abbiamo nella cartella "Lab_ecologia_paesaggio" (FM) 
p224r63_2011 <- brick("p224r63_2011_masked.grd")


# Facciamo un plot dell'immagine, come fatto nella lezione5 sul Telerilevamento (su stessi dati, satellite Landsat): (FM)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


# In base all'immagine consideriamo il numero di classi (in questo caso 4), per classificare l'immagine, "accorpando" i pixel in 4 classi: (FM)

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4) # Raggruppamento senza supervisione dei dati Raster (FM)

p224r63_2011c # per ottenere informazioni sull'unsuperClass creato (FM)

# Facciamo un plot di p224r63_2011c :
plot(p224r63_2011c$map)

# Stabiliamo una legenda per i colori ------> colorRampPalette 8FM9
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# Facciamo una nuova mappa con numero di classi=2:
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
 
# Risultato: Tutti i pixel di vegetazione (foresta pluviale e vegetazione aggiuntiva) sono in una zona ad alto NIR e basso R, viceversa i pixel di suolo nudo e zone agricole saranno in una zona ad alto R e basso NIR. (FM)
# Colorazioni della mappa ottenuta basate sul lavoro di un algoritmo di classificazione delle bande di telerilevamento. Aumentando il numero di classi aumenta l'incertezzza nell'individuazione dei vari gruppi simili (difficile distinguere i pixel da una classe all'altra).   (FM)



#################################
#################################
#################################




### 7) Analisi MULTITEMPOROLE della VARIAZIONE di Land-Cover




# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")  (FM)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Mettiamo le immagini defor1 e defor2 nella cartella Lab_ecologia_paesaggio e richiamo libreria "raster"   (FM)
library(raster)

# Importiamo i dataset delle 2 immagini: brick()   (FM)

defor_1 <- brick("defor1_.jpg.png")
defor_2 <- brick("defor2_.jpg.png")
defor_1 # per vedere le caratteristiche del file Raster (FM)

# Il dato "names" ha tre bande: defor1_.1 = NIR, defor1_.2= Red, defor1_.3= green ------> quindi andiamo a fare un plotRGB di queste bande.   (FM)

plotRGB(defor_1, r=1, g=2, b=3, stretch="Lin") # immagine foresta pluviale (FM)


# ESERCIZIO: fare plot defor_2

plotRGB(defor_2, r=1, g=2, b=3, stretch="Lin") # otteniamo immagine foresta pluviale tempo dopo, deforestazione. (FM)



# Facciamo un par delle 2 immagini, per confrontarle insieme
par(mfrow=c(2,1))
plotRGB(defor_1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor_2, r=1, g=2, b=3, stretch="Lin")

dev.off()

# Ora classifichiamo questa immagine in modo da "scontornare" tutto ciò che è foresta: funzione ----> unsuperClass()   (FM)
# Prima richiamo la libreria "RStoolbox" o la installo ----> install.packages("RStoolbox")  (FM)

library(RStoolbox)
d1c <- unsuperClass(defor_1, nClasses=2)
plot(d1c$map) # plot per vedere la mappa classificata  (FM)

# aggiungiamo una colorRampPalette e rifacciamo il plot:
par(mfrow=c(2,1))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c$map, col=cl, main="Defor_1")



# ESESRCIZIO: Facciamo la stessa cosa per la seconda immagine

d2c <- unsuperClass(defor_2, nClasses=2)
cl <- colorRampPalette(c('green','black'))(100)                              # (FM)
plot(d2c$map, col=cl, main="Defor_2")



# Facciamo un par con 1 riga e 2 colonne
par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c$map, col=cl, main="Defor_1")
cl <- colorRampPalette(c('green','black'))(100) 
plot(d2c$map, col=cl, main="defor_2")


# Andiamo a calcolare la frequenza della foresta in base ad i pixel riferibili alla mappa1: "freq" chiediamo a R di calcolare la frequenza per d1c  (FM)
freq(d1c$map)
# Aree aperte: 36.542 celle
# Foreste: 304.750 celle

totd1 <- 304750 + 36542
totod1 # n° totale di pixel -----> 341.292

# Calcoliamo la percentuale delle nostre frequenze:  percentuale calcolato in base alla frequenza e il numero totale di pixel (FM)
percent1 <- freq(d1c$map) * 100/totd1

percent1
# Percentuali mappa1: Foreste= 89,3% , Aree aperte= 10,7%  (FM)


# Facciamo la stessa cosa per la mappa2:
freq(d2c$map)
# Aree aperte: 163.229 celle
# Foreste: 179.497 celle

totd2 <- 179497 + 163229
totd2 # n° totoale di pixel ---> 342726 (discosta di poco dalla prima mappa) (FM)

percent2 <- freq(d2c$map) * 100/totd2
percent2
# Percentuali mappa2: Foreste= 53,4% , Aree aperte= 47,6 %

# Creiamo un data-frame con questi valori: creiamo una tabella con righe e colonne impostate da noi (FM)

cover <- c("Agriculture","Forest")
before <- c(10.7,89.3)
after <- c(47.6,53.4)
output <- data.frame(cover,before,after)  # Funzione "data.frame"va a formare una raccolta di dati precedentemente impostati e creati su R in questo caso oggetti (cover, before, after) che poi verrano utilizzati per l'analisi multitemporale (FM)
View(output) #  "View" richiama un oggetto con un insieme di dati da visualizzare (FM)

# Salviamo il Work-Space: "Deforestation.Rdata"   (FM)



 ##### PARTE 2

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Carichiamo il Work-Space: "Deforestation.Rdata"
load("Deforestation.Rdata")

ls() # per controllare file a disposizione (FM)


# Richiamo librerie:
library(raster)
library(RStoolbox)



# Ricreiamo grafico della volta precedente:
par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c$map, col=cl, main="Defor_1")
cl <- colorRampPalette(c('green','black'))(100) 
plot(d2c$map, col=cl, main="defor_2")


# Richiamo output:
output <- data.frame(cover,before,after)
output # Mi fornisce un output di un determinato dataframe

# facciamo un ggplot: grafico basato su libreria "ggplot2", sul dataset "output" (FM)
library(ggplot2)

# Agr= agricoltura, For= foresta (FM)

ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")  # "aes"= mi permette di determinare l'estetica del grafico che vado a selezionare; (FM)
# Grafico che mette a confronto in percentuale la copertura in agr e for, nel primo caso (prima della deforestazione). (FM)
dev.off()

# ESERCIZIO: fare grafico del confronto land-cover dopo la deforestazione:

ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")   # (FM)

dev.off()


# Plot dei 2 istogrammi, visualizzabili insieme, nuovo pacchetto ("gridExtra) perchè il par non funziona per ggplot: "gridExtra" in sostituzione della funzione "par", fornisce una serie di funzioni per lavorare con la grafica a "griglia", per organizzare più grafici basati su una griglia (FM)
install.packages("gridExtra")
library(gridExtra)


# Funzione "grid.arrange()", che va a prendere vari plot e li mette insieme nello stesso grafico, quindi uguale al "par" ma funzionante per ggplot  (FM)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")


# ESERCIZIO: usare grid.arrange per creare un grafico unico

grid.arrange(grafico1, grafico2, nrow = 1) # crescita Agr vertiginosa rispetto alla foresta    (FM)



#### Parte 3

# Ricreiamo il grafico ottenuto nella parte 2

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)
library(ggplot2)
library(gridExtra)
cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)
output <- data.frame(cover,before,after)
output
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
grid.arrange(grafico1, grafico2, nrow = 1)

  
# Ottenere grafici con una scala da 0-100: vogliamo impostare limite delle Y da 0 a 1000 -----> ylim(0, 100)    (FM)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow = 1)



##################################################
##################################################
###################################################


### 8) ANALISI NO2 data presi da ESA


# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella") 
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)


# Carichiamo nella nostra cartella "Lab_ecologia_paesaggio" : https://iol.unibo.it/mod/resource/view.php?id=418956 (FM)

# plot della prima immagine:
EN01 <- raster("EN_0001.png")
plot(EN01)


# ESERCIZIO: importare tutte le altre immagini

EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")                       # (FM)
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")                             
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

# Facciamo una colorRamp e plottiamo insieme due immagini, una per l'inizio (Alta concentrazione NO2) e una per la fine (concentrazione più bassa) (FM)

cl <- colorRampPalette(c('red','orange','yellow'))(100) #
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

dev.off()


# Ora vediamo la differnza di NO2 tra le 2 immaggini:

difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif) # Zona Lombardia ha fatto esperienza di una differenza maggiore di NO2              (FM)


# Realizziamo le statistiche di base: 

# 1) plottiamo tutte le mappe: metodo più lento

par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)                              # (FM)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)


# Salviamo work-space nella cartella "Lab_ecologia_paesaggio" : "Temp_NO2.Rdata"       (FM)


##### Parte 2

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)
load("Temp_NO2.Rdata")
ls() # controlliamo che ci siano i file EN



# 2) Usiamo funzione lapply: per plottare una lista di dati -----> plottare i dati tutti insieme

# Creiamo dentro la cartella "Lab_ecologia_paesaggio" una nuova cartella chiamata "esa_no2" e al suo interno copiamo tutti i nostri file EN.png     (FM)
# Facciamo un nuovo set della working-directorry:

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio/esa_no2")
list.files(pattern=".png") # lista dei soli files .png della cartella "esa_no2"   (FM)
rlist <- list.files(pattern=".png")

# Carichiamo i dati tutti assieme: funzione ---->  lapply(), spiegata su sito Rdocumentation (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply)   (FM)

listafinale <- lapply(rlist, raster)
listafinale # lista di tutti e 13 i file caricati (all'interno di una lista)    (FM)
EN <- stack(listafinale) # unica immagine dei 13 file ----> 13 bande, dove ogni banda è un tempo da Gennaio(EN01) a Marzo(EN13).       (FM)

# Ora facciamo il plot
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)

# Salviamo il working-space nella cartella "Lab_ecologia_paesaggio": "lapply_NO2.Rdata"           (FM)



##### Parte 3

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)
load("lapply_NO2.Rdata")
ls() # controlliamo che ci siano i file EN             (FM)

# Differenza tra valore pixel immagine 1 con pixel ultima immagine (tra immagini EN_13 ed EN_01)         (FM)

difEN <- EN$EN_0013 - EN$EN_0001

# Creiamo una colorRamp per la differenza di colore, poi facciamo un Plot:      (FM)
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

# Ora facciamo plot dell'intero Set, prima cambiamo la colorRamp:   (FM)
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)



# Facciamo analisi statistica dei dati riguardanti NO2 in questi mesi:  (FM)

boxplot(EN)
boxplot(EN, horizontal=T) # visualizzazione orizzontale   (FM)
boxplot(EN, horizontal=T,outline=F) 
boxplot(EN, horizontal=T,outline=F,axes=T)



###################################################
###################################################
###################################################



### 9) Analisi SNOW-COVER




# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Scarichiamo i dati relativi alla criosfera dal sito "Copernicus": https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=1000101;Collection=29870071;Time=NORMAL,NORMAL,-1,,,-1,,
# Mettiamoli nella cartella "Lab_ecologia del_passaggio" ------> c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc  (FM)

# Carichiamo il pacchetto necessario a svolgere lo studio:

install.packages("ncdf4") # Utilizzando questo pacchetto, è possibile aprire i file netCDF e leggere facilmente i set di dati (FM)
library(ncdf4)
library(raster)

# Importiamo il file "nc" dentro R: "c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc", dati aggiornati a 18 Maggio 2020. (FM)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# Creiamo una colorRamp

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)


# ESERCIZIO: Fare un plot del dato

plot(snowmay, col=cl) # Copertura nevosa del 18 maggio 2020 in Europa   (FM)



# Ora scarichiamo i dati Snow_cover da IOL, e mettiamo i file .tiff all'interno della cartella "Lab_ecologia_paesaggio", e li raggruppiamo dentro una cartella chiamata "snow_cover"  (FM)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio/snow_cover")



# ESERCIZIO: caricare e plottare tutti assieme i file della cartella "snow_cover"

# Usiamo la funzione lapply : "lapply(X, FUN, …)"   -----> applicare una funzione su un elenco/vettore, riporta una lista della lunghezza X, i cui elementi sono il risultato dell'applicazione della "function"  (FM)

list.files(pattern=".tif") # lista dei singoli file dentro la cartella 
rsnowlist <- list.files(pattern=".tif") 
listasnow <- lapply(rsnowlist, raster)                                                     # (FM) 
snow.multitemp <- stack(listasnow) # unica immagine dei 5 file
plot(snow.multitemp, col=cl)

# Osserviamo com'è cambiata durante gli anni la criosfera in Europa. (FM)



# Facciamo un plot dell'immagine iniziale e finale (FM)

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# Differenza nella copertura di neve tra 2000 e 2020 (FM)

# Facciamo in modo che la scala di valori sia univoca

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))


# Ora calcoliamo la vero e propria differenza tra 2000 e 2020, poi facciamo un Plot.  (FM)

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) # colorRamp per il grafico
plot(difsnow, col=cldiff)



# Ora vediamo come si fa ad inserire un analisi di previsione all'interno di R. Previsione per il 2020. (FM)
# Inseriamo il file "prediction.r" dentro la cartella "snow_cover" -----> funzione "source" = caricare codice dall'esterno (FM)

source("prediction.r") # analisi codice importato dall'esterno  (FM)
# Siccome l'analisi richiede tempo perchè il numero di dati è molto grande, prendiamo un file già preparato da IOL e lo mettiamo nella cartella "snow_cover" ----> "predicted.snow.2025.norm.tif"       (FM)
# Comando "esc" per bloccare analisi   (FM)

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")     
plot(predicted.snow.2025.norm, col=cl)


#########################################
##########################################
###############################################





### 10) R_code_Patches




# Scarichiamo i dati "d1c" e "d2c" dal sito IOL e li mettiamo sulla cartella "Lab_ecologia_paesaggio"   (FM)

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella") e della libreria "raster"   (FM)

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)


# Carichiamo mappa classificata ("d1c" e "d2c") usando la funzione "raster":   (FM)

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")


# Facciamo un plot dei 2 file:

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# la mappa è sblagliata perchè la foresta è presentata con il colore nero, quindi correggiamo l'errore sapendo che:    (FM)
# Foresta classe 2 e agricoltura classe 1   (FM)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# Le foreste sono verdi ora   (FM)


# Ora "annulliamo" dalle immagini tutto ciò che non è foresta, mettiamo tutti gli altri valori come valori nulli.    (FM)
# Usiamo la funzione "reclassify" eliminando il valore 1 che è l'agricoltutura.  (FM)

d1c.for <- reclassify(d1c, cbind(1,NA)) # "NA" -> valori nulli, che associamo a classe 1

d1c.for # informazioni su raster   (FM)

# class      : RasterLayer 
# dimensions : 478, 714, 341292  (nrow, ncol, ncell)
# resolution : 1, 1  (x, y)
# extent     : 0, 714, 0, 478  (xmin, xmax, ymin, ymax)
# crs        : NA 
# source     : memory
# names      : d1c 
# values     : 2, 2  (min, max)  # questo perchè non ci sono altri valori


# Rifacciamo plot (FM)
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for, col=cl)


# Facciamo la stessa cosa per "d2c"  (FM)

d2c.for <- reclassify(d2c, cbind(1,NA))
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)


# Ora usiamo la funzione "clump" che aggrega i pixel insieme formando singole patches, prima installiamo pacchetto "igraph": Pacchetto per grafici semplici e analisi di rete. Può gestire molto bene grafici di grandi dimensioni e offre funzioni per la generazione/visualizzazione visualizzazione di grafici. (FM

install.packages("igraph")
library(igraph)
# Funzione "clump"
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")


# EXERCISE: plottare le mappe una accanto all'altra  

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.pacthes, col=clp)                                                           # FM)
plot(d2c.for.pacthes, col=clp)
# Osserviamo le singole patch di foresta (FM)


#  Ora andiamo a vedere quante patch sono state realizzate. (FM)

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)
library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white") 

# vediamo come varia il numero di Patches prima e dopo la deforestazione.   (FM)








### 11) R_code_Crop

# Scarichiamo i dati Snow_cover da IOL, e mettiamo i file .tiff all'interno della cartella "Lab_ecologia_paesaggio", e li raggruppiamo dentro una cartella chiamata "snow_cover" (FM)
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio/snow_cover")


# ESERCIZIO: caricare tutti assieme i file della cartella "snow_cover"

library(ncdf4)
library(raster)
list.files(pattern=".tif") # lista dei singoli file dentro la cartella (FM)
rsnowlist <- list.files(pattern=".tif") 
listasnow <- lapply(rsnowlist, raster) # funzione lapply (FM)
snow.multitemp <- stack(listasnow) # unica immagine dei 5 file (FM)


# ColorRamp e plot delle immagini
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(snow.multitemp,col=clb)


# "Zoom" ----> funzione di raster:  strumento di visualizzazione dei dati spaziali (FM)

snow.multitemp # vediamo i nomi dei vari file (FM)
plot(snow.multitemp$snow2010r, col=clb) # facciamo il plot del file di interesse fissato con il $ (FM)

extension <- c(6, 18, 40, 50) # ext funzione all'interno di zoom che mi permette di definire l'estensione dei dati (FM)
zoom(snow.multitemp$snow2010r, ext=extension)
extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
# otteniamo diverse immagine a diversa estensione del territorio ( in questo caso dell'Italia) (FM)


# Facciamo zoom direttamente dell'immagine
plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent()) # disegnare rettangolo sul plot, sulla zona di interesse, per ottenere un'immagine dell'Italia uguale a quella precedente (FM)


# Ora facciamo un "crop", funzione che crea una nuova immagine ritagliata da quella iniziale, in base alle mie indicazioni (FM)

snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)


# ESERCIZIO
# Facciamo un "crop" dell'intero stack di livelli, così da poter lavorare contemporaneamente su tutte le immaggini (FM)

extension <- c(6, 20, 35, 50)
snowcover.italy <- crop(snow.multitemp, extension)
plot(snowcover.italy, col=clb) # Unica immagine che indicala lo " Snow-cover" in Italia nel corso del tempo. (FM)
plot(snowcover.italy, col=clb, zlim=c(20,200)) # limiti del grafico (FM)

# Boxplot -----> produce un plot dei dati raggruppati (FM)
boxplot(snow.multitemp.italy, horizontal=T,outline=F) # grafico di analisi dati relativi alla "snow-cover", valori del 2020 molto più bassi rispetto a quelli iniziali (FM)







### 12) SPECIES DISTRIBUTION MODELLING

# Carichiamo il pacchetto "sdm" per la species distribution (FM)

install.packages("sdm")
library(sdm)

# Carichiamo anche i pacchetti "raster" e "rgdal" (FM)

library(raster)
library(rgdal)


# Carichiamo il file all'interno del pacchetto "sdm", uno shapefile (coordinate x,y quindi punti/insieme di punti). (FM)
# Essendo un file di sistema usiamo la funzione "system.file" e "shapefile" (FM)

file <- system.file("external/species.shp", package="sdm")
species <- shapefile(file)

species  # Informazioni sul file, con sistema di coordinate a cui fa riferimento: coordinate UTM,osserviamo che ci troviamo in Spagna. (FM) 
# class       : SpatialPointsDataFrame 
# features    : 200 
# extent      : 110112, 606053, 4013700, 4275600  (xmin, xmax, ymin, ymax)
# crs         : +proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       : Occurrence 
# min values  :          0 
# max values  :          1 

species$occurrence # distribuzione dei punti delle misurazioni a terra delle specie, valore 1 la specie è stata camiponata, valore 0 no (FM)
# [1] 1 0 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 0 1 0 1 0 1 1 1 1 0 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0 0
# [57] 1 1 1 1 0 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 0 1 0 1 0 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 1 1 0 0 0
# [113] 1 0 0 1 1 1 1 1 0 0 0 1 1 0 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 0 1 1 0 0 1 0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 1 0
# [169] 0 1 0 1 0 0 0 0 1 0 1 0 1 0 1 0 0 0 0 1 1 0 1 0 1 1 0 1 0 0 0 0

# Ora facciamo un plot per vedere la distribuzione di questa specie di interesse (FM)

plot(species)

# plot con solo i valori di "Occurrence" uguali ad 1, con pallino colorato di blu. (FM)

plot(species[species$Occurrence == 1,],col='blue',pch=16)


# Ora aggiungiamo le i valori 0, colorandoli di rosso, funzione "points" per aggiungerli al "plot" appena creato (FM)

points(species[species$Occurrence == 0,],col='red',pch=16)


# Aggiungiamo le variabili ambientali -----> "path" (FM)

path <- system.file("external", package="sdm") 
lst <- list.files(path=path,pattern='asc$',full.names = T) # codice per importare i "predittori" delle variabili ambientali che influiscono sulla distribuzione della specie (FM)
lst # lista di tutte le variabili (FM)
# [1] "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/sdm/external/elevation.asc"    
# [2] "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/sdm/external/precipitation.asc"
# [3] "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/sdm/external/temperature.asc"  
# [4] "/Library/Frameworks/R.framework/Versions/3.6/Resources/library/sdm/external/vegetation.asc"


# Facciamo uno "stack" di tutti questi layer relativi alle variabili ambientali che abbiamo a disposizione, poi facciamo un plot (FM)

preds <- stack(lst)
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100) # impostiamo scala di colori (FM)
plot(preds, col=cl) # Grafico che mi descrive le singole variabili all'interno dello stesso territorio (FM)

# Ora aggiungiamo ad ogni variabile la distibuzione della specie, per saper che relazione vi è tra ambiente e specie (FM)

plot(preds$elevation, col=cl) # Specie in base ad altezza (elevations) (FM)
points(species[species$Occurrence == 1,], pch=16) # solo punti dov'è presente la specie. Vediamo che sta meglio ad altezze più basse (FM)

# Facciamo stessa cosa per la temperatura:
plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16) # sta meglio a temperature più alte (FM)

# Precipitazioni:

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16) # distribuzione intermedia (FM)

# Vegetazione 
plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16) # Preferisce le zone ombreggiate, quindi coperta dalla vegetazione (FM)

# In conclusione: la specie si trova al meglio in condizioni di alta temperatura, altezze basse e zone ombrose. Poco influenzata dalla piovosità. (FM)


# Ora mettiamo tutte assieme queste variabili : costruendo un modello lineare (Generalized Linear Model) che mette insieme le variabili tra loro (FM)
# funzione "sdmData"

d <- sdmData(train=species, predictors=preds) # "train": dato raccolto a terra ----> nel nostro caso species. "predictors": variabili (FM)
d # caratteristiche dato (FM)
# class                                 : sdmdata 
=========================================================== 
# number of species                     :  1 
# species names                         :  Occurrence 
# number of features                    :  4 
# feature names                         :  elevation, precipitation, temperature, ... 
# type                                  :  Presence-Absence 
# has independet test data?             :  FALSE 
# number of records                     :  200 
# has Coordinates?                      :  TRUE 

# Creiamo il modello

library(parallel)
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') # il modello ha calcolato a, b, x, y...in base al set di dati che abbiamo impostato (FM)
p1 <- predict(m1, newdata=preds) # Fare previsione in base ai dati "predittivi" (FM)
plot(p1, col=cl) # facciamo il plot
points(species[species$Occurrence == 1,], pch=16) # mappa previsionale della distibuzione della specie in base alle 4 variabili (FM)



############################
############################
############################




###  13) ESAME

## FCOVER - FAPAR – NDVI: Analisi Multitemporale (2000 - 2019)



# Fraction of Vegetation Cover (FCover) corrisponde alla frazione di terreno coperta da vegetazione verde, quantifica l'estensione spaziale della vegetazione.

setwd("/Users/fillo/Desktop/Esame_ecologia_paesaggio/FCOVER")
library(ncdf4)
library(raster)

# Primo metodo:

# Carico i file relativi alla FCOVER uno ad uno: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=512260;Collection=1000081;Time=NORMAL,NORMAL,-1,,,-1,,

FC_2000 <- raster("FC_2000.nc")
FC_2005 <- raster("FC_2005.nc")
FC_2010 <- raster("FC_2010.nc")
FC_2015 <- raster("FC_2015.nc")
FC_2019 <- raster("FC_2019.nc")


# Faccio un plot per mettere a confronto i grafici dei diversi anni:

clFC <- colorRampPalette(c('gray87','wheat4','yellow', 'green', 'darkgreen'))(100) #
par(mfrow=c(3,2))
plot(FC_2000, col=clFC, main="FCOVER_2000")
plot(FC_2005, col=clFC, main="FCOVER_2005")
plot(FC_2010, col=clFC, main="FCOVER_2010")
plot(FC_2015, col=clFC, main="FCOVER_2015")
plot(FC_2019, col=clFC, main="FCOVER_2019")


# Metodo 2:

list.files(pattern=".nc") # lista dei singoli file dentro la cartella 
rFClist <- list.files(pattern=".nc") 
listaFC <- lapply(rFClist, raster)
FC.multitemp <- stack(listaFC) # unica immagine dei 5 file
plot(FC.multitemp, col=clFC)
# Osserviamo com'è cambiata durante gli anni la Fcover nel mondo. Dal 2000 al 2019, nel periodo tra il 21/03 al 20/07



# Plot di confronto tra grafico 2019 e 2000:

par(mfrow=c(1,2))
plot(FC_2000, col=clFC, main="FCOVER_2000")
plot(FC_2019, col=clFC, main="FCOVER_2019")


# Vera e propria differenza tra 2000 e 2019

difFC <- FC_2019 - FC_2000
cldifFC <- colorRampPalette(c('red','lightgray','darkgreen'))(100) #
plot(difFC, col=cldifFC, main="FC2019 - FC2000")


### 2) FAPAR

# Il FAPAR () quantifica la frazione della radiazione solare assorbita dalle foglie per l'attività di fotosintesi. Quindi, si riferisce solo agli elementi verdi e vivi della Canopy.

# Scarico i dati relativi al FAPAR 2019 e 2000 sulla cartella dei dati FC e carico i dati su R: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=512260;Collection=1000084

FAPAR_2019 <- raster("FP_2019.nc")
FAPAR_2000 <- raster("FP_2000.nc")


# Faccio un plot per confrontare i dati tra loro:

clFAPAR <- colorRampPalette(c('pink','red','darkred'))(100)
par(mfrow=c(1,2))
plot(FAPAR_2000, col=clFAPAR, main="FAPAR_2000")
plot(FAPAR_2019, col=clFAPAR, main="FAPAR_2019")


# Differernze tra FAPAR 2019 e FAPAR 2000

difFAPAR <- FAPAR_2019 - FAPAR_2000
cldifFP <- colorRampPalette(c('black','gray','darkred'))(100)
plot(difFAPAR, col=cldifFP, main="FAPAR2019 - FAPAR2000")


### 3) NDVI

# Ora  analizzo le differenze tra NDVI (Normalized Difference Vegetation Index) del 2019 e 2000.
# NDVI = (REF_nir – REF_red)/(REF_nir + REF_red), una pianta SANA riflette molto nel Nir e nel green, e poco nel Red e nel blue (alta assorbanza---> Fotosintesi).
# In una pianta "sotto stress" avremo bassi valori di riflettanza per Nir e green e valori di riflettanza più alti per Red e blue.

# Scarico i dati relativi a NDVI 2019 e 2000 sulla cartella dei dati FC e carico i dati su R: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=513186;Collection=1000085;Time=NORMAL,NORMAL,-1,,,-1,,

march_2000 <- raster("march_2000.nc")
march_2019 <- raster("march_2019.nc")
july_2000 <- raster("july_2000.nc")
july_2019 <- raster("july_2000.nc")

# Faccio un plot per confrontare i dati tra loro:

clNDVI <- colorRampPalette(c('red','lightblue','green', 'darkgreen'))(100) #
par(mfrow=c(2,2))
plot(march_2000, col=clNDVI, main="Marzo_2000_NDVI")
plot(july_2000, col=clNDVI, main="LUglio_2000_NDVI")
plot(march_2019, col=clNDVI, main="Marzo_2019_NDVI")
plot(july_2019, col=clNDVI, main="Luglio_2019_NDVI")

                           
# Differernze tra NDVI 2019 (Marzo-Luglio) e NDVI 2000 (Marzo-Luglio):

diff2000 <- july_2000 - march_2000
diff2019 <- july_2019 - march_2019
cldiffNDVI <- colorRampPalette(c('black', 'lightgray', 'green'))(100) #
par(mfrow=c(1,2))
plot(diff2000, col=cldiffNDVI, main="NDVI_07/2000 - NDVI_03/2000")
plot(diff2019, col=cldiffNDVI, main="NDVI_07/2019 - NDVI_03/2019")
                               
                               
                               
# CONCLUSIONI:
                               
# Faccio un confronto tra FCOVER, FAPAR e NDVI del 2000 e 2019. Mettendo anche le differenze.                           

par(mfrow=c(4,3))

plot(FC_2000, col=clFC, main="FCOVER_2000")
plot(FC_2019, col=clFC, main="FCOVER_2019")            # FCOVER
plot(difFC, col=clFC, main="FC2019-FC2000")
                               
plot(FAPAR_2000, col=clFAPAR, main="FAPAR_2000")
plot(FAPAR_2019, col=clFAPAR, main="FAPAR_2019")                     # FAPAR
plot(difFAPAR, col=cldifFP, main="FAPAR2020 - FAPAR2000")
                           

plot(march_2000, col=clNDVI, main="Marzo_2000_NDVI")
plot(july_2000, col=clNDVI, main="Luglio_2000_NDVI")
plot(march_2019, col=clNDVI, main="Marzo_2019_NDVI")                             # NDVI
plot(july_2019, col=clNDVI, main="Luglio_2019_NDVI")
plot(diff2000, col=cldiffNDVI, main="NDVI_03/2019 - NDVI_03/2000")
plot(diff2019, col=cldiffNDVI, main="NDVI_07/2019 - NDVI_07/2000")

