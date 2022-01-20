#' Statistics from Hysteresis Loop
#'
#' @param hyst hyst data frame
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' h = vsm.get.HystLoops(d)
#' m = vsm.hyst.stats(h)
#' @export
vsm.hyst.stats <- function(hyst) {
  # find data points and time delta
  time.delta = max(hyst$time)-min(hyst$time)
  data.points = nrow(hyst)

  # find the susceptibility
  slope = ppms.getSusceptibility(hyst)
  slope.err = NA
  hyst$Mcorr = hyst$M - slope*hyst$H

  # average step size
  H.step = abs(mean(abs(diff(hyst$H))))

  # get remanence
  n = subset(hyst, H>-5*H.step & H<5*H.step)
  if(nrow(n)>3) {
    fit <- lm(n$Mcorr ~ n$H)
    # plot(hyst$H, hyst$Mcorr)
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
  n = subset(hyst, Mcorr<max(hyst$Mcorr)*0.5 & Mcorr>min(hyst$Mcorr)*0.5 & H<0.4*max(hyst$H) & H>0.4*min(hyst$H))
  M.step = abs(mean(abs(diff(n$Mcorr))))
  n = subset(hyst, Mcorr>-5*M.step & Mcorr<5*M.step & H<0.6*max(hyst$H) & H>0.6*min(hyst$H))

  # debug:
  # plot(hyst$H, hyst$Mcorr)
  # points(n$H, n$Mcorr, col='blue')
  # abline(h=0, col='darkgreen', lwd=3)

  if (nrow(n)>3) {
    n4 = make_monotonic(n$H, n$Mcorr, TRUE)
    n2 = approx(n4$x, n4$y)

    #n3 = spline(x=n4$x, n4$y, method='hyman')
    # plot(n$H, n$Mcorr, pch=19, cex=2)
    # points(n4$x, n4$y, col='red',pch=18)
    # points(n2, col='green')
    # lines(n3, col='blue', pch=18, cex=2)
    n = subset(data.frame(H=n2$x, Mcorr=n2$y), Mcorr > -M.step & Mcorr < M.step)
    # points(n$H, n$Mcorr, col='red', pch=17, cex=3)
    # abline(h=0)

    if(nrow(n) >= 3) {
      fit <- lm(n$H ~ n$Mcorr)
      # plot(n$Mcorr, n$H)
      # abline(fit, col='blue')
      Hc = fit$coef[[1]]
      Hc.err = summary(fit)$coeff[3]
      Hc.nFit = nrow(n)
    }
  }

  # get saturation values
  H.max = max(hyst$H)
  H.min = min(hyst$H)
  Ms1 = NA
  Ms1.err = NA
  Ms2 = NA
  Ms2.err = NA


  n = subset(hyst, H>0.75*H.max & H<0.95*H.max)
  # plot(hyst$H, hyst$Mcorr)
  # points(n$H, n$Mcorr, col='red')
  Ms1 = mean(n$Mcorr)
  Ms1.err = sd(n$Mcorr)

  n = subset(hyst, H<0.75*H.min & H>0.95*H.min)
  Ms2 = mean(n$Mcorr)
  Ms2.err = sd(n$Mcorr)


  # compute sweep speed
  if (nrow(hyst)>5) {
    lm(hyst$H~ hyst$time) -> fit
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

  #

  list(H.first = hyst$H[1],
       H.last = hyst$H[nrow(hyst)],
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
       T = mean(hyst$T),
       T.sd = sd(hyst$T),
       speed = speed,
       speed.sd = speed.err,
       time.delta = time.delta,
       data.points = data.points
  )
}
