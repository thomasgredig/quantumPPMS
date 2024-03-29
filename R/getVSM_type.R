#' Categorize VSM measurement
#'
#' use this function to explore the cut-off for separating the type of
#' VSM file, whether a particular loop is either
#' MvsT or MvsH.
#'
#' @param filename name of the VSM file
#' @returns data frame with Tchg and Hchg
#' @export
getVSM.type <- function(filename) {
  d = read.csv(filename, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  d = na.omit(d)


  dir = .getSweepDirection(d$time,d$H,d$T)
  lp = .getLoop(dir)

  d = data.frame(T=d$T, H=d$H, loop = lp)
  ty = data.frame()
  for(l in levels(factor(lp))) {
    d1 = subset(d,loop==l)
    if (nrow(d1) <= 10) next
    Tchg = mean(abs(diff(d1$T)))
    Hchg = mean(abs(diff(d1$H)))
    y1 = "Mvstime"
    if (Hchg > 0.1) y1 = "MvsH"
    if (Hchg < 0.01 & Tchg>0.01) y1 = "MvsT"
    y = data.frame(
      filename = basename(filename),
      loop = l,
      numData = nrow(d1),
      Tchg = Tchg,
      Hchg = Hchg,
      type = y1
    )
    ty = rbind(ty, y)
  }

  ty
}
