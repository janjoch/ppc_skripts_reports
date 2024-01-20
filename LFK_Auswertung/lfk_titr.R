rm(list=ls())

library(readxl)

R <<- 8.314462618    # J K^-1 mol^-1
p0 <<- 1013.25

COL_ACET_PRIM <<- "#458B00"
COL_ACET_SEC <<- "#7FFF00"

COL_NHEX_PRIM <<- "#0000CD"
COL_NHEX_SEC <<- "deepskyblue"

WIDTH <<- 7
HEIGHT <<- 5

# quartz(height=HEIGHT, width=WIDTH)


IMPORT_PATH <<- "raw_data/"
EXPORT_PATH <<- "exports/titration/"


# HELPER FUNCTIONS
source("../KAL_Auswertung/helpers.R")
#source("kal_routines.R")


titr.data = read_excel("raw_data/LFK_Messprotokolle.xlsx", sheet="LFK_titration")

v = titr.data$v_delta
lfk = titr.data$kappa

range.prestep = 1:16
range.poststep = 20:40

# regression
reg.pre = lm(lfk ~ v, subset=range.prestep)
reg.post = lm(lfk ~ v, subset=range.poststep)

reg.a.pre = coef(reg.pre)[1]
reg.b.pre = coef(reg.pre)[2]
reg.a.post = coef(reg.post)[1]
reg.b.post = coef(reg.post)[2]

v.intercept = (reg.a.pre - reg.a.post) / (reg.b.post - reg.b.pre)

v.ausgl.pre = seq(0.0, v.intercept + 1, length=50)
v.ausgl.post = seq(v.intercept - 1, max(v) + 1, length=50)

lfk.ausgl.pre = predict(reg.pre, list(v=v.ausgl.pre), interval="confidence")
lfk.ausgl.post = predict(reg.post, list(v=v.ausgl.post), interval="confidence")

plot.init.grey(
  v, lfk,
  xlim=c(7, 11),
  ylim=c(2.23, 2.57),
  xlab=expression(italic(V)*" / "*"mL"),
  ylab=expression(italic(kappa)*" / "*"mS/cm")
)

matlines(v.ausgl.pre, lfk.ausgl.pre, lty=c(1,1,1), lwd=c(3,1,1), col="black")
matlines(v.ausgl.post, lfk.ausgl.post, lty=c(1,1,1), lwd=c(3,1,1), col="black")


points(v, lfk)

points(v[range.prestep], lfk[range.prestep], pch=21, bg="white")
points(v[range.poststep], lfk[range.poststep], pch=21, bg="white")

v.conf = 0.132
lines(c(v.intercept, v.intercept), c(0, 2.28), lwd=2)
lines(c(v.intercept - v.conf, v.intercept - v.conf), c(0, 2.274), lwd=1)
lines(c(v.intercept + v.conf, v.intercept + v.conf), c(0, 2.285), lwd=1)

# plot.save(EXPORT_PATH, "titration_full.pdf")
plot.save(EXPORT_PATH, "titration_zoom.pdf")
