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
