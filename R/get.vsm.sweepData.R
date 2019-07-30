#' Return only data of magnetic sweeps (hyst loop)
#'
#' @param hyst VSM data frame
#' @return VSM data frame with sweepData column
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.load(filename)
#' d = get.vsm.sweepData(d)
#' d1 = subset(d, sweepData==1)
#' @export
get.vsm.sweepData <- function(d) {
  n1 = d
  n1$delta.time = c(0,diff(n1$time))
  n1$delta.H = c(0, diff(n1$H))
  n1$delta.T = c(0, diff(n1$T))
  n1$T.dot = n1$delta.T / n1$delta.time
  n1$H.dot = n1$delta.H / n1$delta.time
  # plot(n1$time, n1$H.dot)
  # n2 = subset(n1, abs(H.dot)>10)
  # points(n2$time, n2$H.dot, col='red')
  #
  # #plot(n1$time, n1$T.dot)
  # n3 = subset(n1, abs(T.dot)<0.01)
  # points(n3$time, n3$H.dot, col='green')
  d$sweepData = 0
  d$sweepData[which(abs(n1$H.dot)>10 & abs(n1$T.dot)<0.01)]=1
  d
}
