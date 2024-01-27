# use radian as better R shell

rm(list=ls())

WIDTH <<- 5
HEIGHT <<- 4

# setwd("LFK_Auswertung")
# quartz(height=HEIGHT, width=WIDTH)

library(readxl)

R <<- 8.314462618    # J K^-1 mol^-1
p0 <<- 1013.25

COL_ACET_PRIM <<- "#458B00"
COL_ACET_SEC <<- "#7FFF00"

COL_NHEX_PRIM <<- "#0000CD"
COL_NHEX_SEC <<- "deepskyblue"



IMPORT_PATH <<- "raw_data/"
EXPORT_PATH <<- "exports/"


# HELPER FUNCTIONS
source("../KAL_Auswertung/helpers.R")
#source("kal_routines.R")


temp.data = read_excel("raw_data/LFK_Messprotokolle.xlsx", sheet="temperatur")

temp = temp.data$"temp (°C)"
lfk = temp.data$"kappa (mS/cm)"



par(mai = c(1,1.1,0.3,0.3))
plot.init.grey(
  temp,
  lfk,
  xlim=c(24, 55),
  ylim=c(19,30.5),
  xlab=expression(italic(theta)*" / "*"°C"),
  ylab=expression(italic(kappa)*" / "*"mS/cm")
)
points(temp, lfk)

reg = plot.regression(
  temp, lfk,
  slope.annot.x.offset=0.2,
  slope.annot.y.offset=0.1,
  slope.annot=(
    function(slope)(
      TeX(paste(r"(slope:)", sprintf("%0.4f", slope), r"(mS/cm/K)"))
    )
  ),
  delta.annot.x=(function(delta) TeX(paste(r"(\Delta\theta =)", sprintf("%0.2f", delta), "K"))),
  delta.annot.y=(function(delta) TeX(paste(r"(\Delta\kappa =)", sprintf("%0.2f", delta), "mS/cm"))),
)
reg.a = reg$coefficients[1]
reg.b = reg$coefficients[2]

lfk.zero = predict.linear(reg.a, reg.b, 25)

alpha = reg.b / lfk.zero
print(summary(reg))
print(alpha)
print(lfk.zero)


lines(
  c(25,25),
  c(0,lfk.zero),
  lw=2,
)
lines(
  c(0,25),
  c(lfk.zero,lfk.zero),
  lw=2,
)


plot.save(EXPORT_PATH, "lfk_temperature.pdf")

