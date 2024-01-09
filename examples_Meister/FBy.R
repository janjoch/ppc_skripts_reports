# diese Datei integriern
# source("FBy.R")

# Fehlerbalken y
FBy <- function(x, y, sy, ...) {
  
  arrows(x, y - sy, x, y + sy, code=3, angle=90, length=0.02)
  points(x, y, pch=21, ...)
  
}
