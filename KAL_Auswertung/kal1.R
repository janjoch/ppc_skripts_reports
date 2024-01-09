
rm(list=ls())

# CONSTANTS
# switch to False to reduce processing time :)
DRAW.PLOTS = TRUE

THRES <<- 0.2

HEIGHT <<- 7
WIDTH <<- 10

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
source("helpers.R")
source("kal_routines.R")


# LINEAR REGRESSION MAIN FUNCTION
calibration.linreg <- function(
  import.datfile,
  export.path,
  export.plot,
  export.stats,
  plot.init=TRUE
) {
  
  calib = read.table(join(IMPORT_PATH, import.datfile))
  time = calib$V1
  temp = calib$V2
  
  statistics = process.slope(
    time, temp,
    export.path=export.path,
    export.stats=export.stats,
    plot.init=plot.init
  )
  
  if(DRAW.PLOTS) {
    plot.save(export.path, export.plot)
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

