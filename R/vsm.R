#################################################
## functions for analyzing VSM data from the PPMS
## Date: August 23, 2015
## Author: Thomas Gredig
#################################################


#' Loads QD PPMS data from file
#'
#' @param fname filename including path
#' @return data frame with 5 data columns: time, T, H, M, Merr
#' @examples
#' d = ppms.load('ppms.dat')
#' @export
ppms.load <- function(fname) {
  d = read.csv(fname, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  d
}


#' Reads QD PPMS Header File Data
#'
#' @param fname filename including path
#' @return list
#' @examples
#' d = ppms.dat.info('ppms.dat')
#' @export
ppms.dat.info <- function(fname) {
  if (!file.exists(fname)) {
    warning(paste('Cannot find file:',fname))
    return;
  }

  scan(file = fname, nlines=23, what=character(0), sep='\n') -> header

  if ((length(header)>0) && (header[1]=='[Header]')) {
    title = substr(header[4],7,1000)
    measurement.date = strsplit(header[5],',')[[1]][3]
    measurement.time = strsplit(header[5],',')[[1]][4]
    measurement.type = strsplit(header[6],',')[[1]][2]
    mass = substr(header[9],6,1000)
    sample =substr(header[10],6,1000)
    comment = substr(header[11],6,1000)

    info = cbind(ppms = TRUE, title=title,
                 sample = sample,
                 type= measurement.type,
                 date = measurement.date,
                 time = measurement.time,
                 comment= comment,
                 mass = mass)
  } else {
    info = cbind(ppms = FALSE,
                 title=NA,
                 sample = NA,
                 type= NA,
                 date = NA,
                 time = NA,
                 comment= NA,
                 mass = NA)
  }
  sample.name=NA
  if (info[1]==TRUE) {
    pattern='.*(\\w\\w\\d{6}[A-Za-z0-9]*)'
    sample.name = str_match(paste(info[2],
                                  info[7],
                                  info[3],
                                  fname),pattern)[,2]
  }

  cbind(sample.name = sample.name, info)
}




#' extract hysteresis loops
#'
#' @param fname data frame with time, T, H, M, Merr
#' @return list
#' @examples
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
  count(d, .(loop)) -> m
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
#' @param fname hyst data frame
#' @return list
#' @examples
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



# analyzes a single
# hysteresis loop and
# return two graphs along
# with essential data
analyze.single.VSM.loop <- function(data) {
  # d = ppms.load(fname)
  # d2 = get.HystLoops(d)
  #
  # numLoops = nlevels(d2$loop)
  #
  # data = subset(d2, loop == 1)
  T.mn = signif(mean(data$T),3)
  T.sd = signif(sd(data$T),3)
  exp.uemu = expression(paste('M (10'^-6,' emu)'))
  m1 = ggplot(data, aes(H/1E4,M*1E6)) +
    geom_point() +
    xlab('H (T)') +
    ylab(exp.uemu) +
    ggtitle(paste("RAW:",fname)) +
    annotate("text", x = 0.9*max(data$H)/1E4, y = 0.9*max(data$M)*1E6,
             label = paste('T=',T.mn,'+/-',T.sd,'K'), hjust = 1) +
    theme_bw(base_size = 14)
  # print(m1)
  q = vsm.hyst.stats(subset(data,dir == -1))
  unlist(q) -> q2
  q = vsm.hyst.stats(subset(data,dir == 1))
  unlist(q) -> q3


  data$Mcorr = data$M - data$H*q2['Susceptibility']
  slope = signif(q2['Susceptibility'],3)
  #plot(data$H, data$Mcorr)

  m2 = ggplot(data, aes(H/1E4, Mcorr*1E6)) +
    geom_point() +  xlab('H (T)') +
    ylab(exp.uemu) +
    geom_vline(xintercept = q2['Hc']/1E4, col='red') +
    geom_vline(xintercept = q3['Hc']/1E4, col='red') +
    geom_hline(yintercept = q2['Ms1']*1E6, col='blue') +
    geom_hline(yintercept = q2['Ms2']*1E6, col='blue') +
    ggtitle(paste("Bgd removed:",fname)) +
    annotate("text", x = min(data$H)/1E4, y = 0.9*max(data$Mcorr)*1E6,
             label = paste('T=',T.mn,'+/-',T.sd,'K'), hjust = 0) +
    annotate("text", x = min(data$H)/1E4, y = 0,
             label = paste('chi=',slope,'emu/Oe'), hjust = 0) +
    theme_bw(base_size = 14)
  #print(m2)

  list(m1,
       m2,
       T.mn,
       T.sd,
       (q3['Hc']-q2['Hc'])/2,
       q2['Ms1'],
       q2['Ms2'])
}

