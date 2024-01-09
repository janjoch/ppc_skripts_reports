
rm(list=ls())

# CONSTANTS
DRAW.PLOTS = TRUE

THRES <<- 0.2
THRES.DROP <<- 0.1

EQUIB.WIDTH = 40

HEIGHT <<- 7
WIDTH <<- 10

IMPORT_PATH <<- "raw_data/"
EXPORT_PATH_SOL <<- "exports/solution_enthalpy/"


# HELPER FUNCTIONS
source("helpers.R")
source("kal_routines.R")


analyse.solv.enth <- function(
  import.datfile,
  export.path,
  export.plot,
  export.stats.drop,
  export.stats.slope
) {
  
  sol = read.table(import.datfile)
  time = sol$V1
  temp = sol$V2
  
  # init plot
  plot.init.grey(time, temp)
  
  stats.drop = process.drop(
    temp,
    time,
    export.path=export.path,
    export.stats=export.stats.drop
  )
  
  index.last = length(time)
  time.forslope = time[(stats.drop$index.min):index.last]
  temp.forslope = temp[(stats.drop$index.min):index.last]
  
  stats.slope = process.slope(
    time.forslope,
    temp.forslope,
    export.path=export.path,
    export.stats=export.stats.slope,
    plot.init=FALSE
  )
  
  plot.save(export.path, export.plot=export.plot)

  return(list(stats.drop, stats.slope))
}


# Massen eingewogen
masses = c(0.600, 0.595, 0.601)  # g

st.d.a = c() # stats.drop.all
st.s.a = c() # stats.slope.all

for(i in 1:3) {
  c(st.d, st.s) := analyse.solv.enth(
    join("raw_data/sol_ent", i, ".dat"),
    EXPORT_PATH_SOL,
    export.plot=join("sol", i, ".pdf"),
    export.stats.drop=join("sol", i, "_drop.csv"),
    export.stats.slope=join("sol", i, "_slope.csv")
  )
  st.d.a = rbind(st.d.a, st.d)
  st.s.a = rbind(st.s.a, st.s)
}


write.csv(st.d.a, join(EXPORT_PATH_SOL, "stats_drop.csv"))
write.csv(st.s.a, join(EXPORT_PATH_SOL, "stats_slope.csv"))

