rm(list = ls()) # tabula rasa

mydata <- read.table("Ammoniumchlorid1.dat") # Messdaten aus Datei einlesen
Zeit <- mydata[,1]                           # erste Kolonne: Zeit
Temperatur <- mydata[,2]                     # zweite Kolonne: Temperatur

plot(Zeit, Temperatur, type="l", lwd=2,
  xlim=c(), ylim=c(),     
  xlab=expression("Zeit"~~italic(t)~"/"~s),
  ylab=expression("Temperatur"~~theta~"/"~degree*"C")
)

p <- locator(2)
rect(p$x[1], p$y[1], p$x[2], p$y[2], col=rgb(1,0,0,0.3))
scan()

plot(Zeit, Temperatur, type="o", lwd=2, pch=21, bg="white",
  xlim=c(range(p$x)), ylim=c(range(p$y)),     
  xlab=expression("Zeit"~~italic(t)~"/"~s),
  ylab=expression("Temperatur"~~theta~"/"~degree*"C")
)

Tp <- locator(2)$y
abline(h=Tp)
Delta.Tp <- diff(Tp)
mtext(side=3, sprintf("T-Sprung = %.4f K", Delta.Tp))
