

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
    
    # mark slope
    plot.line.highlight(time.slope, temp.slope)
    
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
  export.stats,
  detect.offset.high=10,
  detect.width.high=40,
  detect.offset.low=20,
  detect.width.low=40,
  detect.mode="slope.next",
  label.position="above"
) {
  temp.equib.start = mean(temp[1:10])
  index.drop = which(temp < temp.equib.start - THRES.DROP)[1]
  time.equib = time[(index.drop - detect.offset.high - detect.width.high):(index.drop - detect.offset.high)]
  temp.equib = temp[(index.drop - detect.offset.high - detect.width.high):(index.drop - detect.offset.high)]
  temp.equib.mean = mean(temp.equib)
  
  if(detect.mode == "slope.next") {
    dtemp = temp[2:length(temp)] - temp[1:(length(temp) - 1)]
    slope.indexstart = which(dtemp > THRES.DIFF)[1]
    if(length(slope.indexstart) == 0) {
      print("WARNING: NEXT SLOPE NOT DETECTED. ADJUST THRES.DIFF?")
      PRINT("    falling back to detect.mode='min.after'")
      detect.mode = "min.after"
    } else {
      index.detected = slope.indexstart - detect.offset.low - detect.width.low
    }
    
  }
  if(detect.mode == "min.after") {
    index.detected = which.min(temp) + detect.offset.low
  }
  
  time.min = time[index.detected]
  temp.min = temp[index.detected]
  
  time.low = time[index.detected : (index.detected + detect.width.low)]
  temp.low = temp[index.detected : (index.detected + detect.width.low)]
  temp.low.mean = mean(temp.low)
  
  temp.delta = temp.low.mean - temp.equib.mean
  
  statistics = data.frame(
    index.drop,
    temp.equib.mean,
    EQUIB.WIDTH,
    index.detected,
    temp.low.mean,
    temp.delta
  )
  write.csv(statistics, join(export.path, export.stats))
  
  if(DRAW.PLOTS) {
    plot.line.highlight(time.equib, temp.equib, col="orange")
    plot.line.highlight(time.low, temp.low, col="orange")
    
    plot.line.annot(
      c(time[index.drop - 10], time[index.detected + EQUIB.WIDTH]),
      c(temp.equib.mean, temp.equib.mean)
    )
    
    plot.line.annot(
      c(
        (time[index.detected] + time[index.detected + EQUIB.WIDTH]) / 2,
        (time[index.detected] + time[index.detected + EQUIB.WIDTH]) / 2
      ),
      c(temp.equib.mean, temp.low.mean)
    )
    
    if(label.position=="above") {
      label.y = temp.equib.mean
      label.yjust = -0.3
    } else if(label.position=="center") {
      label.y = (temp.equib.mean + temp.low.mean) / 2
      label.yjust = 0.5
    } else {
      print(join("WARNING: label.position='", label.position, "' not implemented!"))
      print("    falling back to label.position='above'")
      label.y = temp.equib.mean
      label.yjust = -0.3
    }
    
    plot.annot(
      (time[index.detected] + time[index.detected + EQUIB.WIDTH]) / 2,
      label.y,
      TeX(paste(r"(\Delta T =)", sprintf("%0.3f", (temp.delta)), "K")),
      xjust=0.5,
      yjust=label.yjust
    )
    
  }
  
  statistics
}


