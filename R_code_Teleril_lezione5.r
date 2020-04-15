############ Telerilevamento ############

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

# Risultato : Immagini del confronto tra DVI (vegetazione2011 e vegetazione1988) in 2 risoluzioni differenti, in quella a più alta risoluzione possiamo riconoscere quelle micro-sitazioni che hanno generato il cambiamento



