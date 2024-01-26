# use radian as better R shell

rm(list=ls())

WIDTH <<- 7
HEIGHT <<- 5

# setwd("LFK_Auswertung")
# quartz(height=HEIGHT, width=WIDTH)

library(readxl)


IMPORT_PATH <<- "raw_data/"
EXPORT_PATH <<- "exports/"


# HELPER FUNCTIONS
source("../KAL_Auswertung/helpers.R")
#source("kal_routines.R")


temp.data = read_excel("raw_data/LFK_Messprotokolle.xlsx", sheet="molar")
v.start = temp.data$"V (mL)"[1]
lfk.start = temp.data$"kappa (uS/cm)"[1]
temp.data = temp.data[2:nrow(temp.data),]

lfk.brutto = temp.data$"kappa (uS/cm)"
lfk = lfk.brutto - lfk.start

MOLARITY <<- 0.01
CONC.TITER <<- 0.01
v.abs = temp.data$"V (mL)"
v.rel = v.abs - v.start
conc = CONC.TITER * v.rel / v.abs

par(mai = c(1,1.1,0.3,0.3))
par(mgp = c(2.5,0.8,0))
plot.init.grey(
    conc,
    lfk,
    log="xy",
    xlab="Konzentration",
    ylab="Leitfähigkeit",
    xaxt="n", yaxt="n",  # keine Achsenskalen
    xaxs="i", yaxs="i",  # Achsenbreite exakt
    xlim=c(min(conc) / 1.2, max(conc) * 1.2),
    ylim=c(min(lfk) / 1.2, max(lfk) * 1.2),
)

x.line = c(0.00005, max(conc))
y.line = x.line / 0.00005 * 11.4
lines(
    x.line,
    y.line,
    lty=3,
    lw=2,
    col="black"
)

points(conc, lfk)
points(conc, lfk.brutto)

logticks = plot.axis.log()
axis(1, at=logticks$small, labels=FALSE, tcl=-0.25)
axis(1, at=logticks$big, labels=logticks$big.lab, tcl=-0.25)
axis(2, at=logticks$small, labels=FALSE, tcl=-0.25)
axis(2, at=logticks$big, labels=logticks$big.lab, tcl=-0.25, las=1)


plot.save(EXPORT_PATH, "lfk_molar_1.pdf")



conc.sqrt = sqrt(conc)
lfk.molar = lfk / conc / 1000  # S cm^2 / mol
lfk.molar.brutto = lfk.brutto / conc / 1000  # S cm^2 / mol

plot(
    conc.sqrt,
    lfk.molar.brutto,
    xlim=c(0, 0.044),
    ylim=c(0, 320),
    pch=21,
    col="#878787",
    bg="#c4c4c4",
    xlab=expression(italic(sqrt(c))*" / "*"M"^"1/2"),
    ylab=expression(italic(Lambda)*" / "*"S "*"cm"^"-1"*"mol"^"-1"),
)
plot.grid()

points(
    conc.sqrt,
    lfk.molar.brutto,
    pch=21,
    col="#878787",
    bg="#c4c4c4",
)

reg = plot.regression(
    conc.sqrt,
    lfk.molar,
    draw.annotation = FALSE,
    abline.lty=1
)
reg.a = reg$coefficients[1]
reg.b = reg$coefficients[2]

points(
    conc.sqrt,
    lfk.molar,
    pch=21,
    col="#000000",
    bg="#ffffff"
)

plot.annot(
    0.022,
    100,
    join("y = ", sprintf("%0.0f", reg.b), " x + ", sprintf("%0.1f", reg.a)),
    xjust=0.5,
    yjust=0.5,
    adj=0.05
)

plot.save(EXPORT_PATH, "lfk_molar_2.pdf")