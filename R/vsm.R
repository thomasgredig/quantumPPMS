#################################################
## functions for analyzing VSM data from the PPMS
## Date: August 23, 2015
## Author: Thomas Gredig
#################################################




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




