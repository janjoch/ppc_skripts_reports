# Lebenserwartung m/f bei Geburt in der Schweiz
# Bundesamt fuer Statistik

rm(list=ls())

Jahr <- c(1981:2020)
LE.f <- c(79.2, 79.5, 79.6, 80.1, 80.2, 80.3, 80.7, 80.8, 81.0, 80.8, 81.2, 81.4, 81.5, 
  81.8, 81.8, 82.0, 82.1, 82.5, 82.5, 82.6, 83.1, 83.1, 83.2, 83.7, 83.9, 84.0, 84.2, 84.4,
  84.4, 84.6, 84.7, 84.7, 84.8, 85.2, 84.9, 85.3, 85.4, 85.4, 85.6, 85.1)
LE.m <- c(72.4, 72.8, 72.7, 73.4, 73.5, 73.7, 73.9, 73.9, 74.1, 74.0, 74.1, 74.5, 74.9,
  75.2, 75.3, 76.0, 76.3, 76.3, 76.8, 76.9, 77.4, 77.8, 78.0, 78.6, 78.7, 79.1, 79.4, 79.7,
  79.8, 80.2, 80.3, 80.5, 80.5, 81.0, 80.7, 81.5, 81.4, 81.7, 81.9, 81.0)

M <- data.frame(Jahr, LE.f, LE.m)
print(M)

# plot(Jahr, LE.f, pch=21, bg="gray", las=1,
#   xlim=c(),
#   ylim=c(70,86),
#   xlab="Jahr bei Geburt",
#   ylab="Lebenserwartung (Jahre)")
# points(Jahr, LE.m, pch=21)
# 
# model.f <- lm(LE.f ~ Jahr)
# model.m <- lm(LE.m ~ Jahr)
# 
# abline(model.f)
# abline(model.m)
# 
# 
# x <- c(1980:2080)
# Y.f <- predict(model.f, list(Jahr=x), interval="confidence")
# Y.m <- predict(model.m, list(Jahr=x), interval="confidence")
# polygon(c(x, rev(x)), c(Y.f[,2], rev(Y.f[,3])),
#         col=rgb(1,0,0, 0.25), border=NA)
# polygon(c(x, rev(x)), c(Y.m[,2], rev(Y.m[,3])),
#         col=rgb(0,0,1, 0.25), border=NA)
# 
# matlines(x, Y.f, lty=1, col="black", lwd=c(2,0.5,0.5))
# matlines(x, Y.m, lty=1, col="black", lwd=c(2,0.5,0.5))
# 
# points(Jahr, LE.f, pch=21, bg="gray")
# points(Jahr, LE.m, pch=21, bg="white")
