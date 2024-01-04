rm(list=ls())

x <- seq(-4, 6, 0.01)
y <- exp(-x^2) + rnorm(x)*0.02
x <- x + 31415

plot(x, y, type="l", lwd=2, col="skyblue4")

abline(h = c(0.0, 0.5, 1.0), lty=2)

p <- locator(2)$x

arrows(p[1], 0.5, p[2], 0.5, code=3, angle=90, length=0.25)
arrows(p[1], 0.5, p[2], 0.5, code=3, angle=12, length=0.15)

points(p, c(0.5,0.5), pch=21, bg="pink")
text(mean(p), 0.5, sprintf("%.3f",diff(p)), adj=c(0.5,-0.2))

