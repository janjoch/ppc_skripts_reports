# Lineares Modell mit Vertrauensbereichen

rm(list=ls())

x <- c(3:10)
y <- c(0.56, 0.41, 0.05, 1.10, 1.97, 4.44, 5.63, 9.01)

plot(x, y, xlim=c(0,12), ylim=c(-5,15), pch=21, bg="red")


model <- lm(y ~ 0 + I(x) + I(x^2))
print(summary(model))

# Berechnung der Modellfunktion inkl. Vertrauenskurven
x.c <- seq(0, 11, 0.02)
y.c <- predict(model, list(x=x.c), interval="confidence")
matlines(x.c, y.c, lty=c(1,2,2), lwd=c(2,1,1), col="blue")




