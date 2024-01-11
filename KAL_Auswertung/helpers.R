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


FBy <- function(x, y, sy, ...) {
  # copied from Meister  
  arrows(x, y - sy, x, y + sy, code=3, angle=90, length=0.02)
  points(x, y, pch=21, ...)
}



# PLOT MODULAR FUNCTIONS
plot.init.grey <- function(x, y) {
  plot(x, y,
       type = "l",
       lwd=2,
       col="darkgrey",
       xlab=expression(italic(t)*" / "*"s"),
       ylab=expression(italic(T)*" / "*"°C")
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

plot.annot <- function(x, y, text, xjust=0.5, yjust=0.5, ...) {
  legend(
    x,
    y,
    text,
    bg="white",
    box.col="white",
    adj=0.15,
    xjust=xjust,
    yjust=yjust,
    ...
  )
}

plot.save <- function(export.path, export.plot) {
  dev.copy2pdf(file=join(export.path, export.plot), width=WIDTH, height=HEIGHT)
}
