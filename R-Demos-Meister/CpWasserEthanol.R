rm(list=ls())

M <- read.table("CpWasserEthanolLit.dat")
mpc.Wasser <- M[,3] * 100
Cp.spez <- M[,6]

par(plt=c(0.15, 0.93, 0.18, 0.90))
plot(mpc.Wasser, Cp.spez, type="l", lwd=2, ylim=c(0,5), las=1,
     xlab=expression("Massen-% "*H[2]*O),
     ylab=expression(italic(C[p])*" / "*J~K^{-1}~g^{-1})
     )
points(mpc.Wasser, Cp.spez, pch=21, bg="white")


