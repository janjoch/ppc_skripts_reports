rm(list=ls())

M <- read.csv(file="https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")

countries <- names(M)
timestamp <- M[,1]
time <- strptime(timestamp, format="%Y-%m-%d")

par(mfrow=c(1,1))
col <- which(countries == "Switzerland")
plot(time, M[,col], type="o", main=countries[col], pch=16, cex=0.7, lwd=2)

W <- data.frame(time, M[,col])
write.table(file="bla.txt", W)

# plot(time[-1], diff(M[,col]), type="h", lwd=3)

