

rm(list=ls())

WIDTH <<- 8
HEIGHT <<- 6

if(TRUE) {
  WIDTH <<- 5
  HEIGHT <<- 4
}

# quartz(height=HEIGHT, width=WIDTH)
quartz.start <- function(height=HEIGHT, width=WIDTH) {
  quartz(
    height=height,
    width=width
  )
}

library(readxl)


IMPORT_PATH <<- "raw_data/"
EXPORT_PATH <<- "exports/vinegar/"


# HELPER FUNCTIONS
source("helpers.R")
#source("kal_routines.R")


data.table = read.table("raw_data/vinegar_cont.dat")
time = data.table$V1
temp = data.table$V2

i.start = which.min(temp) + 15
i.end = 380

j.start = 450
j.end = 551


jreg = lm(temp ~ time, subset=c(j.start:j.end))
#jreg.pred = predict(jreg)
#lines(jreg, col="black", lwd=2)



plot.init.grey(
  time,
  temp,
  xlab=expression(italic(t)*" / "*s),
  ylab=expression(italic(T)*" / "*degree*C)
  #xaxs="i",
  #yaxs="i"
)



ireg <- lm(temp ~  I(time) + I(time^2), subset=c(i.start:i.end))
print(summary(ireg))

# Berechnung der Modellfunktion inkl. Vertrauenskurven
x.c <- seq(100, 200, 1)
y.c <- predict(ireg, list(time=x.c)) # , interval="confidence")
matlines(x.c, y.c, lty=c(2,2,2), lwd=c(2,1,1), lw=1)


jreg = plot.regression(
  time,
  temp,
  subset=c(j.start:j.end),
  draw.annotation = FALSE,
)
jreg = lm(temp ~ time, subset=c(j.start:j.end))


# Berechnung der Fitkurven
time.space <- seq(from=90, to=220, by=0.1)
th1 <- predict(ireg, list(time=time.space))
th2 <- predict(jreg, list(time=time.space))


# Interpolation des Schnittpunkts (= Aequivalenzpunkt)
inter.time = approx(th1-th2, time.space, 0)$y
inter.temp = predict.linear(summary(jreg)$coef[1,1], summary(jreg)$coef[2,1], inter.time)

# find starting point of slope
base.temp = mean(temp[120:160])
base.time = approx(th1, time.space, base.temp)$y
 
time.rise = inter.time - base.time
temp.rise = inter.temp - base.temp

# uncertainty estimation
i.unc.pred = predict(ireg, list(time=inter.time), interval="confidence")
j.unc.pred = predict(jreg, list(time=inter.time), interval="confidence")

# assuming y = ax + b
i.a = temp.rise / time.rise
j.a = summary(jreg)$coef[2,1]

i.unc = i.unc.pred[3] - i.unc.pred[1]
j.unc = j.unc.pred[3] - j.unc.pred[1]

time.unc = (i.unc + j.unc) / (i.a - j.a)


print("Schnittpunkt:")
print(c(inter.time, inter.temp))
print("Zeitdauer:")
print(time.rise)
print("Unsicherheit:")
print(time.unc)


plot.line.highlight(time[i.start:i.end], temp[i.start:i.end])

plot.line.highlight(time[j.start:j.end], temp[j.start:j.end], col="red")

# delta t
plot.line.annot(
  c(base.time, inter.time),
  c(base.temp, base.temp)
)
plot.annot(
  mean(c(base.time, inter.time)) + 130, # + 8,
  base.temp + 0.1,
  TeX(paste(r"(\Delta t =)", sprintf("%0.2f", time.rise), "s"))
)

# delta T
plot.line.annot(
  c(inter.time, inter.time),
  c(base.temp, inter.temp)
)
plot.annot(
  inter.time + 90,
  mean(c(base.temp, inter.temp)),
  TeX(paste(r"(\Delta T =)", sprintf("%0.2f", inter.temp - base.temp), "°C"))
)

points(inter.time, inter.temp, pch=21, bg="red")
points(base.time, base.temp, pch=21, bg="red")

plot.save(EXPORT_PATH, "vinegar_cont.pdf")

if(FALSE) {
  plot((time[2:length(time)] + time[1:length(time) - 1]) / 2 , temp[2:length(temp)] - temp[1:(length(temp)-1)], type="l", col="red")
}