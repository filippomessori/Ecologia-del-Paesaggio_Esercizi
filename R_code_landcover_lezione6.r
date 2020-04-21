############# LAND COVER ################


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


