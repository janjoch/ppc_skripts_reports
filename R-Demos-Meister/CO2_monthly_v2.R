rm(list=ls())

urlname <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_weekly_mlo.txt"

M <- read.table(url(urlname))

t <- M$V4      # Datum in Dezimalformat
conc <- M$V5   # CO2-Konzentration [ppm]

conc[which(conc==-999.99)] <- NA   # nicht vorhandene Daten

par(plt=c(0.11, 0.95, 0.16, 0.9), cex=1, pch=16)
plot(t, conc, type="o", cex=0.6, 
  xlim=c(), ylim=c(), las=1,
  xlab=expression(italic(t)*" / year"),
  ylab=expression(conc~CO[2]*" / ppm")
)

p <- locator(2)  # Zoomfenster ausw?hlen

par(plt=c(0.2, 0.45, 0.6, 0.88), cex=0.8, new=TRUE)
plot(t, conc, type="o", cex=0.7, 
  xlim=p$x, ylim=p$y, las=1,
  xlab="", ylab=""
)


