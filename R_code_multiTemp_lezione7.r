############ Analisi multitemporale della VARIAZIONE di Land-Cover #############




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

