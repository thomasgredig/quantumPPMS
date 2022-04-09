#################################################
## functions for analyzing VSM data from the PPMS
## Date: August 23, 2015
## Author: Thomas Gredig
#################################################
# decreasing = TRUE (decrease) or FALSE (increase)
make_monotonic <- function(x, y, m.decreasing = FALSE) {
  q = data.frame(x,y)
  q = q[order(q[,1], decreasing = m.decreasing),]
  val.min = q[1,2]
  for (i in 2:nrow(q)) {
    val = q[i,2]
    if (val > val.min) {
      q[i-1,2] = val
    }
    val.min = val
  }
  q
}
