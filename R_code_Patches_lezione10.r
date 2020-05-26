############# R_code_Patches ###############




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
