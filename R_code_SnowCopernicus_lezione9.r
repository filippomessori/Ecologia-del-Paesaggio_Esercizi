########### Analisi Snow-cover ##############




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
