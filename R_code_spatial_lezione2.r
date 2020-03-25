##### R Spatial: Funzioni spaziali in Ecologia del Paesaggio


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
