# by github.com/janjoch, 2023

library(latex2exp)

# HELPER FUNCTIONS
join <- function(...) {
  paste(..., sep="")
}

# hack copied from https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

# usage
# func_that_returns_three_values <- function(a,b,c) {
#   return(list(a,b,c))
# }
# c(a,b,c) := func_that_returns_three_values(1,2,3)


predict.linear <- function(reg.a, reg.b, x) {
  y = reg.a + reg.b * x
  y
}


FBy <- function(x, y, sy, ...) {
  # copied from Meister  
  arrows(x, y - sy, x, y + sy, code=3, angle=90, length=0.02)
  points(x, y, pch=21, ...)
}



# PLOT MODULAR FUNCTIONS
plot.init.grey <- function(
  x,
  y,
  xlim=NULL,
  ylim=NULL,
  xlab=expression(italic(t)*" / "*"s"),
  ylab=expression(italic(T)*" / "*"°C"),
  type="l",
  lwd=2,
  col="darkgrey",
  ...
) {
  plot(
    x,
    y,
    type=type,
    lwd=lwd,
    col=col,
    xlim=xlim,
    ylim=ylim,
    xlab=xlab,
    ylab=ylab,
    ...
  )
  plot.grid()
}


plot.grid <- function(nx=NULL, ny=NULL, lty=1, col="lightgray", lwd=1, ...) {
  grid(
    nx = nx,
    ny = ny,
    lty = lty,      # Grid line type
    col = col, # Grid line color
    lwd = lwd,      # Grid line width
    ...
  )
}

plot.axis.log <- function(begin=1e-10, end=1e10) {
  d = 10^c(-99:99)
  d = d[d>=begin & d<=end]
  dd = outer(c(1:9), 10^c(-99:99))
  dd = dd[dd>=begin & dd<=end]
  dlab = do.call(
    "expression",
    lapply(seq(along=log10(d)), function(i) substitute(10^E, list(E=log10(d)[i])))
  )
  list("big"=d, "small"=dd, "big.lab"=dlab)
}

plot.line.highlight <- function(x, y, col="blue") {
  lines(
    x,
    y,
    col=col,
    lw=3
  )
  
}

plot.line.annot <- function(x, y) {
  lines(
    x,
    y,
    lty=3,
    lw=2,
    col="black"
  )
  
}

plot.annot <- function(x, y, text, xjust=0.5, yjust=0.5, adj=0.15, ...) {
  legend(
    x,
    y,
    text,
    bg="white",
    box.col="white",
    adj=adj,
    xjust=xjust,
    yjust=yjust,
    ...
  )
}

plot.regression <- function(
  x,
  y,
  draw.annotation=TRUE,
  slope.annot=(function(slope)(TeX(paste(r"(slope:)", sprintf("%0.4f", slope), slope.unit)))),
  slope.annot.x.offset=0,
  slope.annot.y.offset=0,
  abline.lty=2,
  delta.annot.x=(function(delta)(TeX(paste(r"(\Delta t =)", sprintf("%0.2f", delta), "s")))),
  delta.annot.y=(function(delta)(TeX(paste(r"(\Delta T =)", sprintf("%0.2f", delta), "K")))),
  ...
) {
    reg <- lm(y ~ x)
    y.pred <- predict(reg)

    x.min = min(x)
    x.max = max(x)
    y.pred.min = min(y.pred)
    y.pred.max = max(y.pred)

    x.delta = x.max - x.min
    y.pred.delta = y.pred.max - y.pred.min


    if(draw.annotation) {
      # delta t
      plot.line.annot(
        c(min(x), max(x)),
        c(min(y.pred), min(y.pred))
      )
      plot.annot(
        mean(c(min(x), max(x))),
        min(y.pred),
        delta.annot.x(x.delta)
      )
      
      # delta T
      plot.line.annot(
        c(max(x), max(x)),
        c(min(y.pred), max(y.pred))
      )
      plot.annot(
        max(x),
        mean(c(min(y.pred), max(y.pred))),
        delta.annot.y(y.pred.delta)
      )
      
      # slope
      plot.annot(
        min(x) + slope.annot.x.offset * x.delta,
        max(y.pred) - slope.annot.y.offset * y.pred.delta,
        slope.annot(summary(reg)$coef[2,1]),
        xjust=0.3,
        yjust=1
      )
    }
    
    # Regressionsgerade
    abline(reg, lty=abline.lty, lw=2, ...)

    reg
}

plot.save <- function(export.path, export.plot) {
  dev.copy2pdf(file=join(export.path, export.plot), width=WIDTH, height=HEIGHT)
}
