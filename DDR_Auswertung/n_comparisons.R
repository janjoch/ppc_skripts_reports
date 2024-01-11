
rm(list = ls()) # tabula rasa

source("../KAL_Auswertung/helpers.R")

HEIGHT <<- 5
WIDTH <<- 7



par(
  mfrow=c(1,2),
  mar=c(0.7, 5,1,0.7)
)

acet.rho.data = c(
  0.7904,  # acetone measured method 1
  0.7903,  # acetone measured method 2
  0.7899   # acetone source meister
)

nhex.rho.data = c(
  0.6572,  # n-hex measured method 1
  0.6594,  # n-hex measured method 2
  0.6603   # n-hex source meister
)

acet.rho.se = c(
  0.0026,  # acetone measured method 1
  0.0002,  # acetone measured method 2
  1e-5        # acetone source meister
)

nhex.rho.se = c(
  0.0022,  # n-hex measured method 1
  0.0001,  # n-hex measured method 2
  1e-5        # n-hex source meister
)

plot.colorcycle = c("deepskyblue", "red", "green")

plot(
  acet.rho.data,
  xlab="",
  ylab=expression(italic(rho)*" / g "*cm^-1),
  xaxt='n',
  xlim=c(0.5, 3.5),
  ylim=c(acet.rho.data[1] - 1.2 * acet.rho.se[1], acet.rho.data[1] + 1.2 * acet.rho.se[1])
)

plot.grid(nx=NA)

FBy(
  1:3,
  acet.rho.data,
  acet.rho.se,
  bg=plot.colorcycle
)

legend(
  "bottomright",
  legend=c(
    "acetone method 1", "acetone method 2", "acetone reference"
  ),
  #col=c(COL_ACET_PRIM, COL_NHEX_PRIM, "black"),
  pt.bg=plot.colorcycle,
  pch=c( 21,21, 21),
  lty=c(0,0,0),
  bg="white"
)



plot(
  nhex.rho.data,
  xlab="",
  ylab=expression(italic(rho)*" / g "*cm^-1),
  xaxt='n',
  xlim=c(0.5, 3.5),
  ylim=c(nhex.rho.data[1] - 1.2 * nhex.rho.se[1], nhex.rho.data[1] + 1.2 * nhex.rho.se[1] + 0.001)
)

plot.grid(nx=NA)

FBy(
  1:3,
  nhex.rho.data,
  nhex.rho.se,
  bg=plot.colorcycle
)

legend(
  "bottomright",
  legend=c(
    "n-hexane method 1", "n-hexane method 2", "n-hexane reference"
  ),
  #col=c(COL_ACET_PRIM, COL_NHEX_PRIM, "black"),
  pt.bg=plot.colorcycle,
  pch=c( 21,21, 21),
  lty=c(0,0,0),
  bg="white"
)

plot.save("../Reports/", "rho-comparison.pdf")

