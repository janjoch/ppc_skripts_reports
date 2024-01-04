rm(list = ls()) # tabula rasa

par(mfrow=c(1,2))  # zwei nebeneinanderliegende Plots

mydata <- read.table("../R-Demos-Meister/simdos4.dat") # Messdaten aus Datei einlesen
Zeit <- mydata[,1]                           # erste Kolonne: Zeit
Temperatur <- mydata[,2]                     # zweite Kolonne: Temperatur

plot(Zeit, Temperatur, type="l", lwd=2,      # alle Messdaten
  xlab=expression("Zeit  "*italic(t)*" / "*s),
  ylab=expression("Temperatur  "*theta*" / "*degree*C)
)

p <- locator(2)                              # Zoombereich lu-ro klicken
rect(p$x[1], p$y[1], p$x[2], p$y[2], border=T, col="pink")
lines(Zeit, Temperatur, lwd=2)


plot(Zeit, Temperatur, type="l", lwd=2, 
  xlim=range(p$x), ylim=range(p$y),
  xlab=expression("Zeit  "*italic(t)*" / "*s),
  ylab=expression("Temperatur  "*theta*" / "*degree*C)
)
                                             # hineinzoomen
points(Zeit, Temperatur, pch=21, bg="white")

T <- locator(2)$y                            # Punkte fuer T-Sprung

DT <- diff(T)                                # T-Sprung
abline(h=T, col="pink")
mtext(side=3, sprintf("T-Sprung = %.4f K", DT))




