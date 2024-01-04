
rm(list=ls())

library("MASS")

NPEAKS = 3
SAVE <<- TRUE

PeakIntegral <- function(ind, Zeit, Temp) {
  
  # select index in time range
  # idx <- which(Zeit>Zeitbereich[1] & Zeit<Zeitbereich[2])
  
  # subset time/theta
  t <- Zeit[ind[1]:ind[2]]
  theta <- Temp[ind[1]:ind[2]]
  
  theta0 <- theta[1]
  A <- 0
  
  for (i in 1:(length(t) - 1)) {
    A <- A + (
      (theta[i] + theta[i + 1]) / 2 - theta0
    ) * (t[i+1] - t[i])
  }
  
  abline(v=range(t), lty=2)
  polygon(c(t, rev(t)), c(theta, 0*theta+theta0), col="grey85")
  A
  
}

trans_enthalp <- function (filename, save_to) {

  Messdaten <- read.table(paste("DDR_Messdaten_2/", filename, sep=""))
  
  Zeit <- Messdaten[,1]
  Temp <- Messdaten[,2]
  
  plot(
    Zeit, Temp,
    type="o", pch=16, cex=0.6, las=1,
    xlab=expression("Zeit "*italic(t)*" / s"),
    ylab=expression("Temperatur "*theta*" / "*degree*"C")
  )
  
  indexes = matrix(integer(), nrow=2, ncol=0)
  Areas = c()
  
  for (i in 1:NPEAKS) {
    ind = sort(identify(Zeit, Temp, n=2, plot=FALSE))
    indexes = cbind(indexes, ind)
    print(indexes)
    
    Areas = cbind(Areas, PeakIntegral(ind, Zeit, Temp))
    
  }
  
  if(SAVE) {
    dev.copy2pdf(file=paste(save_to, ".pdf", sep=""), width=7, height=5)
    write.matrix(indexes, file=paste(save_to, "_indexes.csv", sep=""))
    write.table(Areas, file=paste(save_to, "_areas.csv", sep=""))
  }

}

#trans_enthalp("methanolmzjj.dat", "ddr2_exports/methanol")
#trans_enthalp("acetone-2-mzjj.dat", "ddr2_exports/acetone")
trans_enthalp("acetone-mzjj.dat", "ddr2_exports/acetone-old")
#trans_enthalp("n-hexane-mzjj.dat", "ddr2_exports/n-hexane")

