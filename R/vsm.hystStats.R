#' Statistics for one Hysteresis Loop
#'
#' Retrieve statistics for one loop, if you need to get statistics for
#' all loops in the VSMdata object, then use vsm.hystStat() instead
#'
#' @param obj VSMdata object
#' @param loop number of the loop
#' @param direction +1 or -1 for direction of applied field
#' @return list or NULL if not a "MvsH" type
#'
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' t(vsm.hystStatsLoop(d))
#'
#' @importFrom stats spline coef lm
#' @importFrom utils tail
#' @export
vsm.hystStatsLoop <- function(obj, loop = 1, direction = 1) {
  #cat("Loop:",loop,"\n")
  obj = vsm.getLoop(obj, loop, direction)
  if (length(obj@time) < 5) return(NULL)
  if (obj@type[1] != 'MvsH') return(NULL)

  # find data points and time delta
  time.delta = max(obj@time)-min(obj@time)
  data.points = length(obj@time)

  # find the susceptibility
  suscept = vsm.getSusceptibility(obj, singleLoop=TRUE)
  slope = suscept[1]
  slope.err = suscept[2]

  if (is.na(slope)) warning("Slope not found.")

  # average step size
  H.step = abs(mean(abs(diff(obj@H))))

  # get remanence
  data = data.frame(
    H = obj@H,
    M = obj@M,
    Mcorr = obj@M - slope*obj@H
  )
  if (is.na(slope)) data$Mcorr = obj@M


  n = subset(data, H > (-4*H.step) & H < (4*H.step))
  #plot(n$H, n$Mcorr)
  if(nrow(n)>3) {
    fit <- lm(n$Mcorr ~ n$H)
    # plot(obj@H, obj@Mcorr)
    # points(n$H, n$Mcorr, col='blue')
    # abline(fit, col='red')
    Mrem = fit$coefficients[[1]]
    Mrem.err <- coef(summary(fit))[[3]]
  } else {
    Mrem = NA
    Mrem.err = NA
  }

  # get coercivity
  Hc = NA
  Hc.err = NA
  Hc.nFit = 0
  n = subset(data, Mcorr<max(data$Mcorr)*0.5 & Mcorr>min(data$Mcorr)*0.5 & H<0.4*max(obj@H) & H>0.4*min(obj@H))
  M.step = abs(mean(abs(diff(data$Mcorr))))
  #n = subset(data, Mcorr>-5*M.step & Mcorr<5*M.step & H<0.6*max(obj@H) & H>0.6*min(obj@H))

  # debug:
  # plot(n$H, n$Mcorr)
  # points(n$H, n$Mcorr, col='blue')
  # abline(h=0, col='darkgreen', lwd=3)

  if (nrow(n)>3) {
    n4 = make_monotonic(n$H, n$Mcorr, TRUE)
    n2 = as.data.frame(approx(n4$x, n4$y))

    # n3 = spline(x=n4$x, n4$y, method='hyman')
    # plot(n$H, n$Mcorr, pch=19, cex=2)
    # points(n4$x, n4$y, col='red',pch=18)
    # points(n2, col='green')
    # lines(n3, col='blue', pch=18, cex=2)
    # plot(n2$x, n2$y)
    # points(n2$x, abs(n2$y),col='red')
    minN2 = which(min(abs(n2$y))==abs(n2$y))[1]
    n = n2[max(1,(minN2-3)):min(nrow(n2),(minN2+3)),]
    #points(n, col='blue',pch=19)
    # n = subset(data.frame(H=n2$x, Mcorr=n2$y), Mcorr >= (-4*M.step) & Mcorr < (4*M.step))
    # points(n$H, n$Mcorr, col='red', pch=17, cex=3)
    # abline(h=0)
    names(n) = c('H','Mcorr')
    if(nrow(n) >= 3) {
      fit <- lm(n$H ~ n$Mcorr)
      # plot(n$Mcorr, n$H)
      # abline(fit, col='blue')
      Hc = signif(fit$coef[[1]],4)
      Hc.err = signif(abs(mean(diff(n4$x)))/2,3)
      Hc.nFit = nrow(n)
    }
  }

  # get saturation values
  H.max = max(obj@H)
  H.min = min(obj@H)
  Ms1 = NA
  Ms1.err = NA
  Ms2 = NA
  Ms2.err = NA


  n = subset(data, H>0.75*H.max & H<0.95*H.max)
  # plot(obj@H, obj@Mcorr)
  # points(n$H, n$Mcorr, col='red')
  Ms1 = mean(n$Mcorr)
  Ms1.err = sd(n$Mcorr)

  n = subset(data, H<0.75*H.min & H>0.95*H.min)
  Ms2 = mean(n$Mcorr)
  Ms2.err = sd(n$Mcorr)


  # compute sweep speed
  if (nrow(data)>5) {
    lm(obj@H~ obj@time) -> fit
    speed <- fit$coefficients[[2]]
    speed.err <- coef(summary(fit))[[4]]
    if (speed.err/speed > 0.003) {
      speed = NA
      speed.err = NA
    }
  } else {
    speed = NA
    speed.err = NA
  }

  list(H.first = obj@H[1],
       H.last = tail(obj@H,1),
       H.max = H.max,
       H.min = H.min,
       Ms1 = Ms1,
       Ms1.sd = Ms1.err,
       Ms2 = Ms2,
       Ms2.sd = Ms2.err,
       Hc = Hc,
       Hc.err = Hc.err,
       Hc.nFit = Hc.nFit,
       Mrem = Mrem,
       Mrem.sd = Mrem.err,
       Susceptibility = slope,
       Susceptibility.sd = slope.err,
       T = mean(obj@T),
       T.sd = sd(obj@T),
       speed = speed,
       speed.sd = speed.err,
       time.delta = time.delta,
       data.points = data.points,
       dir = obj@dir[1],
       type = obj@type[1],
       loop = obj@loop[1]
  )
}





#' Statistics for all Loops in VSM object
#'
#' This function loops through all hyst loops in VSMdata
#' using vsm.hystStats() and adds those parameters
#'
#' @param obj VSMdata object
#'
#' @author Thomas Gredig
#'
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' t(vsm.hystStats(d))
#' @importFrom stats spline coef lm
#' @importFrom utils tail
#' @export
vsm.hystStats <- function(obj) {
  loops = levels(factor(obj@loop))
  r = data.frame()
  for(l in loops) {
    q = vsm.hystStatsLoop(obj, loop=l, dir = 1)
    if (!is.null(q)) r = rbind(r, q)
    q = vsm.hystStatsLoop(obj, loop=l, dir = -1)
    if (!is.null(q)) r = rbind(r, q)
  }

  r
}
