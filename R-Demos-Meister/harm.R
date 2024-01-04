rm(list=ls())

par(pty="s", bg="black", xpd=TRUE)

p <- 4000
t <- seq(from=0, to=2*pi, length.out=p)

for (n in 0:10){
  r <- 1.0 + 0.4*(cos(n*t))^2.1 + rnorm(p)*0.5
  x <- r * cos(t)
  y <- r * sin(t)
  plot(0, 0, type="n", xlim=c(-2,2), ylim=c(-2,2))
  polygon(x, y, col="green", border=NA)
  lines(0.5*x, 0.5*y, col="red", lwd=1)
  Sys.sleep(0.5)
}

