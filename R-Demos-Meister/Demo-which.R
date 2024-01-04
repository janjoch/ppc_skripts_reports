rm(list=ls())

N <- 3000
x <- rnorm(N)
y <- rnorm(N)
r <- sqrt(x^2 + y^2)

par(pty="s", mfrow=c(2,2), cex=0.5)

# Plot 1
plot(x, y, xlim=c(-4,4), ylim=c(-4,4), pch=16, col="blue")

scan()

# Plot 2
plot(x, y, xlim=c(-4,4), ylim=c(-4,4), pch=16, col="blue")
p <- which(r <= 2.0)
points(x[p], y[p], pch=16, col="red")

scan()

# Plot 3
plot(x, y, xlim=c(-4,4), ylim=c(-4,4), pch=16, col="blue")
p <- which(x>0 & y>0)
points(x[p], y[p], pch=16, col="red")

scan()

# Plot 4
plot(x, y, xlim=c(-4,4), ylim=c(-4,4), pch=16, col="blue")
p <- which(r>1.0 & r<2.0 & y<0 | r<0.4)
points(x[p], y[p], pch=16, col="red")
p <- which( abs(abs(x)-0.7)<0.2 & abs(y-1)<0.3 )
points(x[p], y[p], pch=16, col="yellow")

