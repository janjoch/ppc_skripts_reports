
library(latex2exp)

rm(list=ls())


# CONSTANTS
# switch to False to reduce processing time :)
DRAW.PLOTS = FALSE

THRES <<- 0.2

HEIGHT <<- 5
WIDTH <<- 7

IMPORT_PATH <<- "raw_data/"
EXPORT_PATH_CAL <<- "exports/calib/"
EXPORT_PATH_ETH <<- "exports/ethanol/"

ref.V <<- 0.1  # L

water.c.sp <<- 4182  # J / K / kg
water.rho <<- 0.9982  # kg / L
water.m <<- water.rho * ref.V  # kg

eth.rho <<- 0.7893  # kg / L

heater.U <<- 10.0  # V
heater.I <<- 1.61  # A
heater.P <<- heater.U * heater.I  # W

# for clean display:
# quartz(height=HEIGHT, width=WIDTH)


# HELPER FUNCTIONS
join <- function(...) {
  paste(..., sep="")
}

annotation <- function(x, y, text, xjust=0.5, yjust=0.5) {
  legend(
    x,
    y,
    text,
    bg="white",
    box.col="white",
    adj=0.15,
    xjust=xjust,
    yjust=yjust,
  )
}


# LINEAR REGRESSION MAIN FUNCTION
calibration.linreg <- function(import.datfile, export.path, export.plot, export.stats) {
  
  calib = read.table(join(IMPORT_PATH, import.datfile))
  time = calib$V1
  temp = calib$V2
  
  temp.equib = mean(temp[1:10])
  print("initial equilibrium temperature")
  print(temp.equib)
  index.max = which.max(temp)
  temp.max = temp[index.max]
  
  index.slopestart = which(temp > (temp.equib + THRES))[1]
  index.slopestop = tail(which(temp < temp.max - THRES), n=1)
  
  time.slope = time[index.slopestart:index.slopestop]
  temp.slope = temp[index.slopestart:index.slopestop]
  n.slope = index.slopestop - index.slopestart + 1
  time.slope.min = time.slope[1]
  temp.slope.min = temp.slope[1]
  time.slope.max = time.slope[n.slope]
  temp.slope.max = temp.slope[n.slope]
  
  
  time.delta = time.slope.max - time.slope.min
  temp.delta = temp.slope.max - temp.slope.min
  
  reg = lm(temp.slope ~ time.slope)
  print(summary(reg))
  reg.a = summary(reg)$coef[1,1]
  reg.b = summary(reg)$coef[2,1]
  reg.sa = summary(reg)$coef[1,2]
  reg.sb = summary(reg)$coef[2,2]
  
  
  # SUMMARISE STATS
  statistics = data.frame(
    n.slope,
    index.slopestart,
    index.slopestop,
    time.delta,
    temp.delta,
    time.slope.min,
    temp.slope.min,
    time.slope.max,
    temp.slope.max,
    reg.a,
    reg.b,
    reg.sa,
    reg.sb
  )
  write.csv(statistics, join(export.path, export.stats))
  
  
  # PLOT
  if(DRAW.PLOTS) {
    # init plot
    plot(time, temp,
         type = "l",
         lwd=2,
         col="darkgrey",
         xlab=expression(italic(t)*" / "*"s"),
         ylab=expression(italic(T)*" / "*"°C")
    )
    grid(nx = NULL, ny = NULL,
         lty = 1,      # Grid line type
         col = "lightgray", # Grid line color
         lwd = 1)      # Grid line width
    
    # mark slope
    lines(
      time.slope,
      temp.slope,
      col="blue",
      lw=3
    )
    
    # delta t
    lines(
      c(time.slope.min, time.slope.max),
      c(temp.slope.min, temp.slope.min),
      lty=3,
      lw=2,
      col="black"
    )
    annotation(
      mean(time.slope),
      temp.slope.min,
      TeX(paste(r"(\Delta t =)", time.delta, "s"))
    )
    
    # delta T
    lines(
      c(time.slope.max, time.slope.max),
      c(temp.slope.min, temp.slope.max),
      lty=3,
      lw=2,
      col="black"
    )
    annotation(
      time.slope.max,
      mean(temp.slope),
      TeX(paste(r"(\Delta T =)", sprintf("%0.2f", temp.delta), "K"))
    )
  
    # slope
    annotation(
      time.slope.min,
      temp.slope.max,
      TeX(paste(r"(slope:)", sprintf("%0.4f", reg.b), "K / s")),
      xjust=0.3,
      yjust=1
    )
    
    # Regressionsgerade
    abline(reg, lty=2, lw=2)
    
    
    # EXPORT
    dev.copy2pdf(file=join(export.path, export.plot), width=WIDTH, height=HEIGHT)
  }
  
  statistics
  
}


# PROCESS ETHANOL MIXTURE DATA
ethanol_mixture <- function(import.csv) {
  mix = read.csv(join(IMPORT_PATH, "ethanol_composition.csv"), dec=".")
  
  mix$eth.m = mix$m2 - mix$m1
  mix$water.m = mix$m3 - mix$m2
  mix$total.m = mix$eth.m + mix$water.m
  mix$share.m = mix$eth.m / mix$total.m
  mix$eth.V = mix$eth.m / eth.rho
  mix$water.V = mix$water.m / water.rho
  mix$total.V = mix$eth.V + mix$water.V
  mix$share.V = mix$eth.V / mix$total.V
  mix$eth.m.perV = mix$eth.m * (ref.V / mix$total.V)
  mix$water.m.perV = mix$water.m * (ref.V / mix$total.V)
  mix$total.m.perV = mix$eth.m.perV + mix$water.m.perV
  
  mix
}



slopes.water = c()

for (i in 1:3) {
  st = calibration.linreg(join("cal",i,".dat"), EXPORT_PATH_CAL, join("cal",i,".pdf"), join("cal",i,"_stats.csv"))
  slopes.water = cbind(slopes.water, st$reg.b)
}

# calculate heat capacity of setup
slope.water = mean(slopes.water)
slope.water.se = sd(slopes.water) / sqrt(2)

system.C = heater.P / slope.water
water.C = water.m * water.c.sp

dewar.C = system.C - water.C

mix = ethanol_mixture("ethanol_composition.csv")
mix$C = c(0,0,0)
mix$c.sp = c(0,0,0)

for (i in 1:3) {
  target = mix$mass_percent_aim[i]
  st = calibration.linreg(join("ethanol",target,".dat"), EXPORT_PATH_ETH, join("ethanol",target,".pdf"), join("ethanol",target,"_stats.csv"))
  mix$C[i] = heater.P / st$reg.b - dewar.C  # J
  mix$c.sp[i] = mix$C[i]/ mix$total.m.perV[i]
}

write.csv(mix, join(EXPORT_PATH_ETH, "ethanol_results.csv"))

