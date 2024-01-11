
rm(list=ls())

# raw = read.table(join(IMPORT_PATH, "sol_ent1.dat"))
# > time = raw$V1
# > temp = raw$V2


# CONSTANTS
DRAW.PLOTS = TRUE

THRES <<- 0.2
THRES.DROP <<- 0.1
THRES.DIFF <<- 0.007

EQUIB.WIDTH = 40

HEIGHT <<- 7
WIDTH <<- 10

IMPORT_PATH <<- "raw_data/"
EXPORT_PATH_SOL <<- "exports/melting_enthalpy/"


# HELPER FUNCTIONS
source("helpers.R")
source("kal_routines.R")


analyse.ice <- function(
  import.datfile,
  export.path,
  export.stats,
  export.plot,
  detect.offset.high=10,
  detect.width.high=40,
  detect.offset.low=70,
  detect.width.low=40
) {
  raw = read.table(import.datfile)
  time = raw$V1
  temp = raw$V2
  
  #plot(time, temp, type="l")
  
  
  # init plot
  plot.init.grey(time, temp)
  
  stats.drop = process.drop(
    temp,
    time,
    export.path=export.path,
    export.stats=export.stats,
    detect.mode="min.after",
    detect.offset.high=detect.offset.high,
    detect.width.high=detect.width.high,
    detect.offset.low=detect.offset.low,
    detect.width.low=detect.width.low,
    label.position="center"
  )
  
  plot.save(export.path, export.plot=export.plot)
  
  stats.drop

}



import.datfile = join(IMPORT_PATH, "ice1.dat")
export.path = EXPORT_PATH_SOL
export.stats = "icewater1_stats.csv"
export.plot = "icewater1.pdf"

#analyse.ice(import.datfile, export.path, export.stats, export.plot)

st.w = c()  # water
st.i = c()  # ice (solid)

for(i in 1:3) {
  stats = analyse.ice(
    join(IMPORT_PATH, "ice", i, ".dat"),
    EXPORT_PATH_SOL,
    export.plot=join("icewater", i, ".pdf"),
    export.stats=join("icewater", i, "_stats.csv")
  )
  st.w = rbind(st.w, stats)
}

detect.widths.high = c(40,40,20)

for(i in 1:3) {
  stats = analyse.ice(
    join(IMPORT_PATH, "ice", i+3, ".dat"),
    EXPORT_PATH_SOL,
    export.plot=join("icecubes", i, ".pdf"),
    export.stats=join("icecubes", i, "_stats.csv"),
    detect.offset.high=10,
    detect.width.high=detect.widths.high[i],
    detect.offset.low=40
  )
  st.i = rbind(st.i, stats)
}

