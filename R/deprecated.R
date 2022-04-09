#' Deprecated functions of quantumPPMS
#'
#'
#' get.vsm.sweepData,ppms.removeSubstrateMagnetization and ppms.getSusceptibility are depreacted, so avoid
#' using this or downgrade to an older function; these functions may disappear
#' from this package
#'
#' 'get.vsm.sweepData' Return only data of magnetic sweeps (hyst loop)
#'
#' 'ppms.seqType': Returns characteristics of a PPMS data file.
#'
#' 'ppms.vsm.hystLoops': Returns a vector that separates different hysteresis loops
#' 'vsm.get.HystLoops' extract hysteresis loops (OBSOLETE, use ppms.vsm.hystLoops)
#'
#' use summary(PPMSdata) instead
#'
#' The category number can be used to check whether the
#' same sequence was used to generated the data.
#'
#' @name quantumPPMS-deprecated
#'
#' @param data data frame with time, T, H, M, Merr
#'
#' @param H applied magnetic field (Oe)
#' @param M magnetization (emu)
#' @param obj PPMSdata object
#' @param Temp temperature vector (K)
#' @param Happ applied field vector (Oe)
#'
NULL



#' @rdname quantumPPMS-deprecated
#' @export
get.vsm.sweepData <- function(obj) {
  m1 = which(obj@dir != 0)
  .subVSMdata(obj, m1)
}

#' @rdname quantumPPMS-deprecated
#' @export
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

#' @rdname quantumPPMS-deprecated
#' @export
ppms.removeSubstrateMagnetization <- function(hyst) {
  if (!("part" %in% names(hyst))) warning("Need part in hyst for ppms.removeSubstrateMagnetization()")
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



#' @rdname quantumPPMS-deprecated
#' @export
ppms.seqType <- function(Temp, Happ) {
  warning("Obsolete: use summary(PPMSdata) instead.")
  n = data.frame(
    len = length(Temp),
    T.low = signif(min(Temp),2),
    T.high = signif(max(Temp),2),
    H.low = signif(min(Happ),3),
    H.high = signif(max(Happ), 3),
    numHystLoop = length(which(abs(diff(Happ))>10)),
    numSuscept = length(which(abs(diff(Temp))>0.03))
  )
  n$cat = signif(log10(n$len/n$numHystLoop)+log10(n$T.high-n$T.low) + log10(n$H.high-n$H.low),2)
  n
}


#' @rdname quantumPPMS-deprecated
#' @export
ppms.vsm.hystLoops <- function(H,M) {
  # separate all hyst loops
  dH = c(0,diff(H))
  # for(j in 1:3) {
  #   dH3 = c(0,abs(diff(dH)))
  #   q = which(dH3 > sd(dH3)*2)
  #   dH[q] <- dH[q-2]
  # }
  dH2 = c(0, diff(sign(dH)))
  q = c(0,which(abs(dH2)==2),length(H))
  f1 = diff(c(q[which(diff(q)>2)],length(H)))
  rep(1:length(f1),times=f1)
}




#' @rdname quantumPPMS-deprecated
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
  m = as.data.frame(table(d$loop))
  # count(d, "loop") -> m    # plyr
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


#' @rdname quantumPPMS-deprecated
#' @export
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
