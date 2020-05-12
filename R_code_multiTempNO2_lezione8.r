############# ANALISI NO2 data presi da ESA ################


# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)


# Carichiamo nella nostra cartella "Lab_ecologia_paesaggio" : https://iol.unibo.it/mod/resource/view.php?id=418956

# plot della prima immagine:
EN01 <- raster("EN_0001.png")
plot(EN01)


# ESERCIZIO: importare tutte le altre immagini
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")



# Facciamo una colorRamp e plottiamo insieme due immagini, una per l'inizio (Alta concentrazione NO2) e una per la fine (concentrazione più bassa)

cl <- colorRampPalette(c('red','orange','yellow'))(100) #
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

dev.off()


# Ora vediamo la differnza di NO2 tra le 2 immaggini:

difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif) # Zona Lombardia ha fatto esperienza di una differenza maggiore di NO2


# Realizziamo le statistiche di base: 

# 1) plottiamo tutte le mappe: metodo più lento
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)


# Salviamo work-space nella cartella "Lab_ecologia_paesaggio" : "Temp_NO2.Rdata"


##### Parte 2

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)
load("Temp_NO2.Rdata")
ls() # controlliamo che ci siano i file EN



# 2) Usiamo funzione lapply: per plottare una lista di dati -----> plottare i dati tutti insieme

# Creiamo dentro la cartella "Lab_ecologia_paesaggio" una nuova cartella chiamata "esa_no2" e al suo interno copiamo tutti i nostri file EN.png
# Facciamo un nuovo set della working-directorry:

setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio/esa_no2")
list.files(pattern=".png") # lista dei soli files .png della cartella "esa_no2"
rlist <- list.files(pattern=".png")

# Carichiamo i dati tutti assieme: funzione ---->  lapply(), spiegata su sito Rdocumentation (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply)

listafinale <- lapply(rlist, raster)
listafinale # lista di tutti e 13 i file caricati (all'interno di una lista)
EN <- stack(listafinale) # unica immagine dei 13 file ----> 13 bande, dove ogni banda è un tempo da Gennaio(EN01) a Marzo(EN13).

# Ora facciamo il plot
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)

# Salviamo il working-space nella cartella "Lab_ecologia_paesaggio": "lapply_NO2.Rdata"



##### Parte 3

# Set della Working-Directory creata su Mac (scrivania): setwd("/Users/nome_utente/Desktop/nome_cartella")
setwd("/Users/fillo/Desktop/Lab_ecologia_paesaggio")
library(raster)
load("lapply_NO2.Rdata")
ls() # controlliamo che ci siano i file EN

# Differenza tra valore pixel immagine 1 con pixel ultima immagine (tra immagini EN_13 ed EN_01)

difEN <- EN$EN_0013 - EN$EN_0001

# Creiamo una colorRamp per la differenza di colore, poi facciamo un Plot:
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

# Ora facciamo plot dell'intero Set, prima cambiamo la colorRamp:
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)



# Facciamo analisi statistica dei dati riguardanti NO2 in questi mesi:

boxplot(EN)
boxplot(EN, horizontal=T) # visualizzazione orizzontale
boxplot(EN, horizontal=T,outline=F) 
boxplot(EN, horizontal=T,outline=F,axes=T)
