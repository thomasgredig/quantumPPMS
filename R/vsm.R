#################################################
## functions for analyzing VSM data from the PPMS
## Date: August 23, 2015
## Author: Thomas Gredig
#################################################


#' extract hysteresis loops
#'
#' @param data data frame with time, T, H, M, Merr
#' @return list
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.load(filename)
#' h = vsm.get.HystLoops(d)
#' @export
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
  count(d, .(loop)) -> m    # plyr
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

# decreasing = TRUE (decrease) or FALSE (increase)
make_monotonic <- function(x, y, m.decreasing = FALSE) {
  # x=n$H
  # y=n$Mcorr
  # plot(x,y, pch=19,cex=2)
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

#' Statistics from Hysteresis Loop
#'
#' @param hyst hyst data frame
#' @return list
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
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



vsm.stats <- function(hyst) {
  title=c()
  result=list()
  direction=c('left','right')
  #for(l in levels(hyst$loop)) {
  #l = as.numeric(l)
  #for(dir.sel in c(-1,1)) {
  for(mypart in levels(hyst$part)) {
    d = subset(hyst, part==mypart)
    if (nrow(d)>5) {
      dir.sel = as.numeric(levels(d$dir))[d$dir[1]]
      l=as.numeric(levels(d$loop))[d$loop[1]]

      vsm.hyst.stats(d) -> a
      result=cbind(result,rbind(part=mypart, loop=l, dir=dir.sel, cbind(a)))
      title=c(title, paste('Loop',l,direction[((dir.sel+1)/2)+1]))
    }
  }
  #}
  result = as.data.frame(result)
  names(result) = title
  result
}

ppms.removeSubstrateMagnetization <- function(hyst) {
  Mcorr = c()
  for (j in as.numeric(levels(hyst$part))) {
    levels(hyst$part)[j]->mypart
    h = subset(hyst, part==mypart)
    #ggplot(h, aes(H,M, color=part)) + geom_point()
    ppms.getSusceptibility(h) -> suscept
    Mcorr = c(Mcorr, h$M-suscept*h$H)
    #ggplot(h, aes(H,Mcorr, color=part)) + geom_point()
  }
  Mcorr
}

ppms.getSusceptibility <- function(h) {
  slope=0
  if(nrow(h)>5) {
    mydir = as.numeric(levels(h$dir))[1]
    if (mydir<0) {
      hm = subset(h, H>0.7*max(H) & H<0.95*max(H))
    } else {
      hm = subset(h, H<0.7*min(H) & H>0.95*min(H))
    }
    lm(hm$M ~ hm$H)->fit
    fit$coefficients[[2]] -> slope
  }
  slope
  #plot(hm$H, hm$M)
  #abline(fit)
}




