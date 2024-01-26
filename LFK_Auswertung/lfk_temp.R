# use radian as better R shell

rm(list=ls())

WIDTH <<- 7
HEIGHT <<- 5

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

plot.init.grey(
  temp,
  lfk,
  xlim=c(24, 53),
  ylim=c(19,30.5),
  xlab=expression(italic(theta)*" / "*"°C"),
  ylab=expression(italic(kappa)*" / "*"mS/cm")
)
points(temp, lfk)

reg = plot.regression(temp, lfk, slope.annot.x.offset=0.2, slope.annot.y.offset=0.2, slope.unit="mS/cm/K")
reg.a = reg$coefficients[1]
reg.b = reg$coefficients[2]

temp.zero = predict.linear(reg.a, reg.b, 25)

alpha = reg.b / temp.zero
print(alpha)


plot.save(EXPORT_PATH, "lfk_temperature.pdf")

