

# PROCESS SLOPE
process.slope <- function(time, temp, export.path, export.stats, plot.init=TRUE) {
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
    if(plot.init) {
      # init plot
      plot.init.grey(time, temp)
    }
    
    # mark slope
    plot.line.highlight(time.slope, temp.slope)
    
    # delta t
    plot.line.annot(
      c(time.slope.min, time.slope.max),
      c(temp.slope.min, temp.slope.min)
    )
    plot.annot(
      mean(time.slope),
      temp.slope.min,
      TeX(paste(r"(\Delta t =)", time.delta, "s"))
    )
    
    # delta T
    plot.line.annot(
      c(time.slope.max, time.slope.max),
      c(temp.slope.min, temp.slope.max)
    )
    plot.annot(
      time.slope.max,
      mean(temp.slope),
      TeX(paste(r"(\Delta T =)", sprintf("%0.2f", temp.delta), "K"))
    )
    
    # slope
    plot.annot(
      time.slope.min,
      temp.slope.max,
      TeX(paste(r"(slope:)", sprintf("%0.4f", reg.b), "K / s")),
      xjust=0.3,
      yjust=1
    )
    
    # Regressionsgerade
    abline(reg, lty=2, lw=2)
  }
  
  statistics
  
}


# PROCESS TEMP DROP
process.drop <- function(
  temp,
  time,
  export.path,
  export.stats
) {
  temp.equib.start = mean(temp[1:10])
  index.drop = which(temp < temp.equib.start - THRES.DROP)[1]
  time.equib = time[(index.drop - 10 - EQUIB.WIDTH):(index.drop - 10)]
  temp.equib = temp[(index.drop - 10 - EQUIB.WIDTH):(index.drop - 10)]
  temp.equib.mean = mean(temp.equib)
  
  index.min = which.min(temp)
  time.min = time[index.min]
  temp.min = temp[index.min]
  
  time.low = time[index.min : (index.min + EQUIB.WIDTH)]
  temp.low = temp[index.min : (index.min + EQUIB.WIDTH)]
  temp.low.mean = mean(temp.low)
  
  temp.delta = temp.low.mean - temp.equib.mean
  
  statistics = data.frame(
    index.drop,
    temp.equib.mean,
    EQUIB.WIDTH,
    index.min,
    temp.low.mean,
    temp.delta
  )
  write.csv(statistics, join(export.path, export.stats))
  
  if(DRAW.PLOTS) {
    plot.line.highlight(time.equib, temp.equib, col="orange")
    plot.line.highlight(time.low, temp.low, col="orange")
    
    plot.line.annot(
      c(time[index.drop - 10], time[index.min + EQUIB.WIDTH]),
      c(temp.equib.mean, temp.equib.mean)
    )
    
    plot.line.annot(
      c(
        (time[index.min] + time[index.min + EQUIB.WIDTH]) / 2,
        (time[index.min] + time[index.min + EQUIB.WIDTH]) / 2
      ),
      c(temp.equib.mean, temp.low.mean)
    )
    
    plot.annot(
      (time[index.min] + time[index.min + EQUIB.WIDTH]) / 2,
      temp.equib.mean,
      TeX(paste(r"(\Delta T =)", sprintf("%0.3f", (temp.delta)), "K")),
      xjust=0.5,
      yjust=-0.3
    )
    
  }
  
  statistics
}


