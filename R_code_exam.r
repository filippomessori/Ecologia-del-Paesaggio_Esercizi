################ ESAME ECOLOGIA DEL PAESAGGIO #################



###### 1) PRIMO CODICE R

install.packages("sp")
Library(sp)
# require (sp) è un altro modo per far partire librerie

data(meuse)
meuse

head(meuse)

names(meuse)


summary(meuse)

pairs(meuse)

pairs(~ cadmium + copper + lead , data=meuse)

# ESERCIZIO: pairs con tutti gli elementi 
pairs(~ cadmium + copper + lead + zinc , data=meuse)


pairs(meuse[,3:6])
#per selezionare piu velocemente scegliendo le colonne, mi viene fuori lo stesso grafico di prima


pairs(meuse[,3:6], col="red")
#cambiare colore 


# pointcharater caratterizzare punti
pairs(meuse[,3:6], col="red", pch=19)

# aumentare dimensione punti
pairs(meuse[,3:6], col="red", pch=19, cex=3)


pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs")
# Mettere il titolo al grafico

# EXERCISE: Fare la stessa cosa con elevation e gli altri elementi
pairs(meuse[,3:7], col="red", pch=19, cex=3, main="Secondo pairs")


# COPIARE codice da un altro

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

# lower.paenel= parte inferiore pannello---> scegliamo cosa metterci= Correlazioni che io avevo scritto,  upper.panel= panel dot smoothing,
# diag.panel=metto quello che manca, cioè istogrammi.


EXERCISE: cambiare ordine pannelli nel grafico
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correletions, diag.panel = panel.histograms)


# funzione PLOT lancia la funzione
plot(meuse$cadmium, meuse$copper)

# attach associa senza il dollaro
attach(meuse)
plot(cadmium, copper)

# caratterizzo grafico
plot(cadmium, copper, pch=17, col="green", main= "primo plot")

# cambiare Labels, etichette grafico
plot(cadmium, copper, pch=17, col="green", main= "primo plot", xlab="cadmio", ylab="rame")

# cambiare dimensioni labels e punti
plot(cadmium, copper, pch=17, col="green", main= "primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2)






### 2) R Spatial: Funzioni spaziali in Ecologia del Paesaggio


# richiamare pachetto "sp" con library()
library(sp)

# Richiamo dati: data()
data(meuse)
head(meuse)


# Plot cadmium e lead: plot(), lo caratterizziamo con colore punti col(), caratterizzazione punti pch(), dimensione punti cex()
# alleghiamo dataframe: attach()
attach(meuse)
plot(cadmium,lead,col="red", pch=19,cex=2)


# ESERCIZO: plot di copper e zinco con simbolo triangolo (pch 17) e colore verde, cex=2
plot(copper, zinc,col="green", pch=17, cex=2)

# cambiare le etichette del grafico : labels-----> xlab="" , ylab=""
plot(copper, zinc,col="green", pch=17, cex=2, xlab="Rame", ylab="Zinco")



# MULTIFRAME o Multipanel: importantissima per fare un grafico, par(mfrow=c("numero_righe","numero_colonne"))
par(mfrow=c(1,2))
plot(copper, zinc,col="green", pch=17, cex=2)
plot(cadmium,lead,col="red", pch=19,cex=2)

# invertiamo i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(copper, zinc,col="green", pch=17, cex=2)
plot(cadmium,lead,col="red", pch=19,cex=2)



# Multiframe automatico
install.packages("GGally")
# richiamo pacchetto con library
library(GGally)
# funzione ggpairs del dataset(meuse), però il dataset è troppo grande quindu seleziono le colonne di interesse apprendo [] e digitando numero colonna di interesse

pairs(meuse[,3:6])
#ottengo un grafico che mi indica la correlazione tra le variabili, usando il coefficiente di Spearman (0-1)---> + è alto + vi è correlazione



# SPATIAL: pima cosa dobbiamo dire al software che il dataset ha delle coordinate, in questo caso la X e la Y; usiamo la funzione coordinates()=~ :
head(meuse)

# Spieghiamo che le coordinate di questo dataset sono X e Y
coordinates(meuse)=~x+y


# otteniamo con plot(meuse) una distribuzione dei nostri punti nello spazio
plot(meuse)


# Funzione spplot = plottare i miei dati spazialmente (le variabili in spplot vanno messe ""), ed otteniamo il primo grafico spaziale riferito allo zinco.

spplot(meuse, "zinc")
# osserviamo un grafico che mi dice il quantitativo di Zinco relativo a vari punti di un fiume.





### 2.1) R_Spatial_2 



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








### 3) POINT PATTERNS: Codice per analisi dei point patterns 


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


############################################################################# PARTE 2


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


################# INTERPOLAZIONE ################ PARTE 3


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







### 4) TELERILEVAMENTO

# Codice R per analisi di immagini satellitari



# Scarico pacchetti raster
install.packages("raster")

# Richiamo pacchetto:
library(raster)

# Richiamo wd: per Mac setwd("/Users/nome_utente/Desktop/Lab")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Richiamo l'immagine di telerilevamento di interesse presente nella mia cartella, specificando l'anno in cui il dato è stato telerilevato :
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# Facciamo un plot dell'immagine satellitare: plot(nome_oggetto)
plot(p224r63_2011)
# Otteniamo un risultato della stessa area anallizzata da 7 diversi sensori che analizzano su diverse lunghezze d'onda, le seguenti:

# B1= blu, B2= verde, B3= Rosso, B4= infrarosso vicino(NIR), B5: medium infrared, B6: thermal infrared, B7: medium infrared.


# Adesso settiamo i colori per i grafici, facendo una colorRampPalette: cl <- colorRampPalette(c('black','grey','light grey'))(100) # e poi facciamo un plot
cl <- colorRampPalette(c('black','grey','light grey'))(100) #
plot(p224r63_2011, col=cl)

# se vogliamo usare una risoluzione diversa, più bassa per esempio:
cllow <- colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011, col=cllow)



# ColorRamp per i colori nella banda del Blu: B1,  per prima cosa uso names() per avere i nomi dei vari grafici
names(p224r63_2011)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
#attach(), non funziona con il dataset "raster", quindi dobbiamo mettere un simbolo($) che lega la colonna al dataset


# ESERCIZIO: plottare banda infrarosso vicino con colorRamp che varia dal rosso, arancione, giallo

clNIR <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clNIR)
# valori molto alti, quindi probabilmente molta vegetazione nell'area studio----> attività fotosintetica




# Multiframe, con graficici nelle varie lunghezze d'onda colorati, per le lunghezze: Blu, Verde, Rosso, NIR 
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



# NATURAL COLOURS, con tre componenti : R G B (per vedere i colori come li vedrebbe l'occhio umano)
# Costruiamo 3 bande: r= banda rosso, g= banda verde, b= banda del blu -----> plotRGB()
# r=3, g=2, b=1
plotRGB(p224r63_2011, r=3, g=2, b=1)

# Per ottenere un'immagine miglore facciamo un "stretch"
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")


# A questo punto per passare alla visualizzazione della banda del NIR scaliamo di 1 le posizioni di r, g, b, appena settate: 
# r diventa 4, g diveenta 3, b diventa 4
# Creiamo una composizione "false colors", questo per mettere la componente NIR (che altrimenti non sarebbe visibile) sulla banda del rosso


plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Con questo settaggio il suolo "nudo" diventa azzurrino, così da distinguerlo dalla vegetazione.
# Possiamo quindi svolgere uno studio miglore della zona di interesse

# salvare pdf grafico creato
pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()



# Multiframe dei 2 grafici precedentemente creati:
par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()


# Ora montiamo l'infrarosso su altre componenti (prima l'abbiamo fatto nel rosso): ESERCIZIO -----> NIR nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# NIR nella componente blu
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")


# Salvo il working-space nella cartella "Lab_ecologia_paesaggio" come : Remote_sensing2.Rdata



########### Seconda parte ############


# Richiamiamo libreria "raster" e la working-directory, e i dati elaborati nella prima parte:
library(raster) 
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
load("Remote_sensing2.Rdata")
ls() # list per vedere i miei file



# Richiamo i dati del telerilevamento relativi al 1988 ( satellite Landsat)----> i dati devono essere all'interno della cartella "Lab_ecologia_paesaggio"
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# Plottiamo i nuovi dati:
plot(p224r63_1988)


# Eseguiamo le stesse operazioni fatte con i dati 2011:

# Rcordiamo le bande dei sensori:
# B1: blue - 1
# B2: green - 2
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


# Facciamo un plotRGB, per vedere i grafici con i colori naturali:
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")




# ESERCIZIO: fare un plot usando la componente NIR al posto della r in RGB space:
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")




# Ora confrontiamo le immagini Landsat 1988 con quelle del 2011:
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()


# Ora possiamo considerare lo stress di una pianta, così da capire se è sana. 
# Facciamo questo tramite Indici di vegetazione: Sappiamo che una pianta SANA riflette molto nel Nir e nel green, e poco nel Red e nel blue (alta assorbanza---> Fotosintesi).
# In una pianta "sotto stress" avremo bassi valori di riflettanza per Nir e green e valori di riflettanza più alti per Red e blue.

# Indice più utilizzato in questi casi è il DVI (Difference Vegetation Index)------> DVI= NIR - R

# DVI per 1988: dvi1988= Nir_1988 - R_1988 (sottraiamo in questo modo pixel per pixel)

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)



# ESERCIZIO: colcolare DVI per immagine relativa a dati 2011

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi2011)


# Usiamo un'altra colorRamp, possiamo prendere una di quelle fatte precedentemente, per esempio:
cldvi <- colorRampPalette(c('light blue','light green','green'))(100) # 
plot(dvi2011, col=cldvi)




# Confrontando i 2 DVI possiamo considerare se la vegetazione era meno stressata nel 1988 o nel 2011, così da avere una base di partenza per valutare i fattori che incidono sulla vegetazione dell'area
# Per fare questo facciamo una "multi-temporal analysis":

difdvi <- dvi2011 - dvi1988
plot(difdvi)

# Creiamo una colorRamp:
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)

# Dal grafico vediamo che: le immagini in blu sono le piante che stanno meglio, quelle in rosso quelle che stanno peggio, quelle in bianco quelle che sono rimaste stabili.

# Chiudiamo il grafico:
dev.off()


# Ora vogliamo visualizzzare gli output in 3 grafici differenti----> Grafico 1988, Grafico 2011 e grafico "multi-temporal analysis" (difdvi):
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)

dev.off()




# Cambiare la RISOLUZIONE dell'immagine, variamo per esempio un pixel di 30 metri con un factor=10 in un pixel di 300m -----> Quindi possiamo variare la dimensione dei Pixel all'interno di un'immagine
# Per esempio:

p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

p224r63_2011lr  # per vedere informazioni relative alla nuova immagine

# Ora richiamiamo i plotRGB fatti precedentemente e vediamo come sono le immagini con le 2 differenti risoluzioni:
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# Creiamo un oggetto ad una risoluzione ancora più bassa----->  50x30= 1500
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50 # per vedere informazioni relative alla nuova immagine

# Inseriamo anche questa risoluzione nella visualizzazione del nostro paesaggio: Stessa immagine a 3 risoluzioni diverse
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# Ovviamente più la grana è fine (pixel piccolo) e più si riescono a notare i particolari relativi alla vegetazione.

dev.off()



# Campioniamo a risoluzione più bassa anche dati per 1988:
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)

# Calcoliamo DVI anche per 1988, così da poter fare un confronto con il DVI del 2011:
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

# Plot dei 2 anni insieme:
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

# Risultato : Immagini del confronto tra DVI (vegetazione2011 e vegetazione1988) in 2 risoluzioni differenti, in quella a più alta risoluzione possiamo riconoscere quelle micro-sitazioni che hanno generato il cambiamento.







### 5) LAND COVER 


# Richiamo wd: per Mac setwd("/Users/nome_utente/Desktop/Lab")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Richiamo pacchetto:
library(raster)

# Installo pacchetto "RStoolbox":
install.packages("RStoolbox")
library(RStoolbox)


# Funzione brick : delle immagini che abbiamo nella cartella "Lab_ecologia_paesaggio"
p224r63_2011 <- brick("p224r63_2011_masked.grd")


# Facciamo un plot dell'immagine, come fatto nella lezione5 sul Telerilevamento (su stessi dati, satellite Landsat):
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


# In base all'immagine consideriamo il numero di classi (in questo caso 4), per classificare l'immagine, "accorpando" i pixel in 4 classi:

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c # per ottenere informazioni sull'unsuperClass creato

# Facciamo un plot di p224r63_2011c :
plot(p224r63_2011c$map)

# Stabiliamo una legenda per i colori ------> colorRampPalette
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# Facciamo una nuova mappa con numero di classi=2:
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
 
# Risultato: Tutti i pixel di vegetazione (foresta pluviale e vegetazione aggiuntiva) sono in una zona ad alto NIR e basso R, viceversa i pixel di suolo nudo e zone agricole saranno in una zona ad alto R e basso NIR.
# Colorazioni della mappa ottenuta basate sul lavoro di un algoritmo di classificazione delle bande di telerilevamento. Aumentando il numero di classi aumenta l'incertezzza nell'individuazione dei vari gruppi simili (difficile distinguere i pixel da una classe all'altra).






### 6) Analisi MULTITEMPOROLE della VARIAZIONE di Land-Cover




# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Mettiamo le immagini defor1 e defor2 nella cartella Lab_ecologia_paesaggio e richiamo libreria "raster"
library(raster)

# Importiamo i dataset delle 2 immagini: brick()

defor_1 <- brick("defor1_.jpg.png")
defor_2 <- brick("defor2_.jpg.png")
defor_1 # per vedere le caratteristiche del file Raster

# Il dato "names" ha tre bande: defor1_.1 = NIR, defor1_.2= Red, defor1_.3= green ------> quindi andiamo a fare un plotRGB di queste bande.

plotRGB(defor_1, r=1, g=2, b=3, stretch="Lin") # immagine foresta pluviale


# ESERCIZIO: fare plot defor_2

plotRGB(defor_2, r=1, g=2, b=3, stretch="Lin") # otteniamo immagine foresta pluviale tempo dopo, deforestazione.



# Facciamo un par delle 2 immagini, per confrontarle insieme
par(mfrow=c(2,1))
plotRGB(defor_1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor_2, r=1, g=2, b=3, stretch="Lin")

dev.off()

# Ora classifichiamo questa immagine in modo da "scontornare" tutto ciò che è foresta: funzione ----> unsuperClass()
# Prima richiamo la libreria "RStoolbox" o la installo ----> install.packages("RStoolbox")

library(RStoolbox)
d1c <- unsuperClass(defor_1, nClasses=2)
plot(d1c$map) # plot per vedere la mappa classificata

# aggiungiamo una colorRampPalette e rifacciamo il plot:
par(mfrow=c(2,1))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c$map, col=cl, main="Defor_1")

# ESESRCIZIO: Facciamo la stessa cosa per la seconda immagine
d2c <- unsuperClass(defor_2, nClasses=2)
cl <- colorRampPalette(c('green','black'))(100) 
plot(d2c$map, col=cl, main="Defor_2")

# Facciamo un par con 1 riga e 2 colonne
par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) 
plot(d1c$map, col=cl, main="Defor_1")
cl <- colorRampPalette(c('green','black'))(100) 
plot(d2c$map, col=cl, main="defor_2")


# Andiamo a calcolare la frequenza della foresta in base ad i pixel riferibili alla mappa1:
freq(d1c$map)
# Aree aperte: 36.542 celle
# Foreste: 304.750 celle

totd1 <- 304750 + 36542
totod1 # n° totale di pixel -----> 341.292

# Calcoliamo la percentuale delle nostre frequenze:
percent1 <- freq(d1c$map) * 100/totd1

percent1
# Percentuali mappa1: Foreste= 89,3% , Aree aperte= 10,7%


# Facciamo la stessa cosa per la mappa2:
freq(d2c$map)
# Aree aperte: 163.229 celle
# Foreste: 179.497 celle

totd2 <- 179497 + 163229
totd2 # n° totoale di pixel ---> 342726 (discosta di poco dalla prima mappa)

percent2 <- freq(d2c$map) * 100/totd2
percent2
# Percentuali mappa2: Foreste= 53,4% , Aree aperte= 47,6 %

# Creiamo un data-frame con questi valori: creiamo una tabella con righe e colonne impostate da noi
cover <- c("Agriculture","Forest")
before <- c(10.7,89.3)
after <- c(47.6,53.4)
output <- data.frame(cover,before,after)
View(output)

# Salviamo il Work-Space: "Deforestation.Rdata"



 ##### Parte 2 #####

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Carichiamo il Work-Space: "Deforestation.Rdata"
load("Deforestation.Rdata")

ls() # per controllare file a disposizione


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
output

# facciamo un ggplot: grafico basato su libreria "ggplot2", sul dataset "output"
library(ggplot2)

# Agr= agricoltura, For= foresta

ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") 
# Grafico che mette a confronto in percentuale la copertura in agr e for, nel primo caso (prima della deforestazione).
dev.off()

# ESERCIZIO: fare grafico del confronto land-cover dopo la deforestazione:

ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

dev.off()


# Plot dei 2 istogrammi, visualizzabili insieme, nuovo pacchetto ("gridExtra) perchè il par non funziona per ggplot:
install.packages("gridExtra")
library(gridExtra)


# Funzione "grid.arrange()", che va a prendere vari plot e li mette insieme nello stesso grafico, quindi uguale al "par" ma funzionante per ggplot

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")


# ESERCIZIO: usare grid.arrange per creare un grafico unico

grid.arrange(grafico1, grafico2, nrow = 1) # crescita Agr vertiginosa rispetto alla foresta



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


# Ottenere grafici con una scala da 0-100: vogliamo impostare limite delle Y da 0 a 1000 -----> ylim(0, 100)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow = 1)








########### 8) Analisi SNOW-COVER




# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")

# Scarichiamo i dati relativi alla criosfera dal sito "Copernicus": https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=1000101;Collection=29870071;Time=NORMAL,NORMAL,-1,,,-1,,
# Mettiamoli nella cartella "Lab_ecologia del_passaggio" ------> c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc

# Carichiamo il pacchetto necessario a svolgere lo studio:

install.packages("ncdf4")
library(ncdf4)
library(raster)

# Importiamo il file "nc" dentro R: "c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc", dati aggiornati a 18 Maggio 2020.

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# Creiamo una colorRamp

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)


# ESERCIZIO: Fare un plot del dato

plot(snowmay, col=cl) # Copertura nevosa del 18 maggio 2020 in Europa



# Ora scarichiamo i dati Snow_cover da IOL, e mettiamo i file .tiff all'interno della cartella "Lab_ecologia_paesaggio", e li raggruppiamo dentro una cartella chiamata "snow_cover"
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio/snow_cover")



# ESERCIZIO: caricare e plottare tutti assieme i file della cartella "snow_cover"

# Usiamo la funzione lapply

list.files(pattern=".tif") # lista dei singoli file dentro la cartella 
rsnowlist <- list.files(pattern=".tif") 
listasnow <- lapply(rsnowlist, raster)
snow.multitemp <- stack(listasnow) # unica immagine dei 5 file
plot(snow.multitemp, col=cl)
# Osserviamo com'è cambiata durante gli anni la criosfera in Europa.



# Facciamo un plot dell'immagine iniziale e finale

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)
# Differenza nella copertura di neve tra 2000 e 2020

# Facciamo in modo che la scala di valori sia univoca

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))


# Ora calcoliamo la vero e propria differenza tra 2000 e 2020, poi facciamo un Plot.

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) # colorRamp per il grafico
plot(difsnow, col=cldiff)



# Ora vediamo come si fa ad inserire un analisi di previsione all'interno di R. Previsione per il 2020.
# Inseriamo il file "prediction.r" dentro la cartella "snow_cover" -----> funzione "source" = caricare codice dall'esterno

source("prediction.r") # analisi codice importato dall'esterno
# Siccome l'analisi richiede tempo perchè il numero di dati è molto grande, prendiamo un file già preparato da IOL e lo mettiamo nella cartella "snow_cover" ----> "predicted.snow.2025.norm.tif"
# Comando "esc" per bloccare analisi 

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)








### 9) R_code_Patches




# Scarichiamo i dati "d1c" e "d2c" dal sito IOL e li mettiamo sulla cartella "Lab_ecologia_paesaggio"

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella") e della libreria "raster"

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)


# Carichiamo mappa classificata ("d1c" e "d2c") usando la funzione "raster":

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")


# Facciamo un plot dei 2 file:

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# la mappa è sblagliata perchè la foresta è presentata con il colore nero, quindi correggiamo l'errore sapendo che:
# Foresta classe 2 e agricoltura classe 1

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
# Le foreste sono verdi ora 


# Ora "annulliamo" dalle immagini tutto ciò che non è foresta, mettiamo tutti gli altri valori come valori nulli.
# Usiamo la funzione "reclassify" eliminando il valore 1 che è l'agricoltutura.

d1c.for <- reclassify(d1c, cbind(1,NA)) # "NA" -> valori nulli, che associamo a classe 1

d1c.for # informazioni su raster

# class      : RasterLayer 
# dimensions : 478, 714, 341292  (nrow, ncol, ncell)
# resolution : 1, 1  (x, y)
# extent     : 0, 714, 0, 478  (xmin, xmax, ymin, ymax)
# crs        : NA 
# source     : memory
# names      : d1c 
# values     : 2, 2  (min, max)  # questo perchè non ci sono altri valori


# Rifacciamo plot
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for, col=cl)


# Facciamo la stessa cosa per "d2c"

d2c.for <- reclassify(d2c, cbind(1,NA))
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)


# Ora usiamo la funzione "clump" che aggrega i pixel insieme formando singole patches, prima installiamo pacchetto "igraph"

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
plot(d1c.for.pacthes, col=clp)
plot(d2c.for.pacthes, col=clp)
# Osserviamo le singole patch di foresta


#  Ora andiaamo a vedere quante patch sono state realizzate.

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)
library(ggplot2)
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white") 

# vediamo come varia il numero di Patches prima e dopo la deforestazione.








### 10) R_code_Crop.r

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


# Zoom ----> funzione di raster (FM)

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
