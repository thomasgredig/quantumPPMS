#library(plyr)
#' extract hysteresis loops (OBSOLETE, use ppms.vsm.hystLoops)
#'
#' @param data data frame with time, T, H, M, Merr
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' h = vsm.get.HystLoops(d)
#' @export
#' @importFrom plyr count
vsm.get.HystLoops <- function(data) {
  data$H.change = c(data$H[1], diff(data$H))
  data$H.change[1] = data$H.change[2]
  d = subset(data, abs(H.change) > 1)
  d$dir = sign(d$H.change)

  q = diff(d$time)
  diff(c(0,which(q > mean(q) + 2*sd(q)), nrow(d))) -> ln
  d$loop = rep(1:length(ln), ln)
  d$loop = factor(d$loop)
  d$dir = factor(d$dir)

  if(nrow(d)<5) { return(d) }

  # find all the loops with less than 5 data points
  count(d, "loop") -> m    # plyr
  d[which(d$loop %in% which(m$freq < 5) ),] <- NA

  na.omit(d) -> d
  d$loop <- factor(d$loop)
  levels(d$loop) <- 1:length(levels(d$loop))

  # re-arrange the loops
  d$dir2 = as.numeric(levels(d$dir)[d$dir])
  ln = diff(c(which(diff(c(1,d$dir2))/2==-1),length(d$dir2)))
  ln[length(ln)] = ln[length(ln)] +1
  d$loop = rep(1:length(ln), ln)
  d$loop = factor(d$loop)

  # find all the parts
  which(diff(c(as.numeric(levels(d$dir))[d$dir],3)) != 0) -> m
  rep(1:length(m),diff(c(0,m))) -> d$part
  d$part = as.factor(d$part)

  d
}
