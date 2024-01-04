rm(list=ls())

filename <- "daily_in_situ_co2_mlo.csv"

M <- read.table(filename, skip=34, sep=",", dec=".")

yyyy <- M[,1]
mm <- M[,2]
dd <- M[,3]
conc <- M[,4]
t.ISO <- ISOdate(yyyy, mm, dd)
t.num <- as.numeric(t.ISO)  # Time in second
t.year <- 1958 + (t.num-t.num[1]) / (3600*24*365)

par(plt=c(0.14, 0.95, 0.16, 0.9), cex=1, pch=16)
plot(t.ISO, conc, 
  type="l", lwd=2, col="blue",
  xlim=c(), ylim=c(), las=1,
  xlab=expression(italic(t)*" / year"),
  ylab=expression("conc "*CO[2]*" / ppm")
)

p <- locator(2)$x  # Zoomfenster auswaehlen

zoomed <- which(t.ISO>p[1] & t.ISO<p[2])
lines(t.ISO[zoomed], conc[zoomed], 
      lwd=2, col="red")

par(plt=c(0.23, 0.55, 0.55, 0.88), cex=0.8, new=TRUE)
plot(t.ISO[zoomed], conc[zoomed], 
  type="p", col="blue", cex=1,
  xlim=c(), ylim=c(), las=1,
  xlab="", ylab=""
)
lines(t.ISO[zoomed], conc[zoomed], 
  lwd=2, col="red")

