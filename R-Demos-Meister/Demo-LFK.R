# Elektrische Leifaehigkeit KCl Konz-Abhaengigkeit, LJ 10.10.2012

rm(list=ls())

# Volumen Loesung zugegeben aus Buerette:
dV <- c(0,0.1,0.22,0.32,0.43,0.54,0.65,0.73,0.83,0.94,1.03,1.13,1.24,
1.33,1.49,1.54,1.64,1.74,2.74,3.74,4.74,5.74,6.74,7.76,8.76,9.76,10.77)

# Leitfaehigkeit gemessen in uS/cm:
kappa.exp <- c(7.505,9.124,10.63,12.97,15.16,16.66,18.08,18.94,20.44,22.19,
23.03,24.71,26.38,27.21,28.88,30.52,31.34,32.97,46.74,59.64,72.60,85.41,
97.51,110.0,122.2,133.9,145.7)/1000

c0 <- 0.01          # Konzentration der KCl-Loesung in Buerette
V0 <- 100.0         # Volumen Wasser in Becherglas
c <- c0*dV/(V0+dV)  # Konzentration der KCl-Loesung in Becherglas

kappa <- kappa.exp - kappa.exp[1]    # Loesungsmittelkorrektur
lambda <- kappa / c                  # molare Leitfaehigkeit
wurzelc <- sqrt(c)                   # 
plot(wurzelc, lambda,                # alle Messdaten darstellen
  xlim=c(0,0.04), ylim=c(100,200), xaxs="i", las=1,
  pch=21, bg="gray85",
  xlab=expression(sqrt(italic(c))*" / "*M^{1/2}),
  ylab=expression(Lambda*" / "*S~cm^2~mol^{-1})
)    

p <- locator(2)$x        # Konzentrationsbereich fuer Regression waehlen
Bereich <- which(wurzelc>p[1] & wurzelc<p[2])
gerade <- lm(lambda ~ wurzelc, subset=Bereich)

print(summary(gerade))  # Regressionsanalyse ausdrucken in Konsole
abline(v=p, lty=3)      # Bereichsgrenzen markieren
abline(gerade, lwd=2)   # Regressionsgerade einzeichnen
points(wurzelc[Bereich], lambda[Bereich], pch=21, bg="tomato")

L0 <- summary(gerade)$coef[1,1]
sL0 <- summary(gerade)$coef[1,2]
arrows(0, L0-sL0, 0, L0+sL0, 
       code=3, angle=90, length=0.05, lwd=3, col="tomato")

dev.copy2pdf(width=7, height=5)


