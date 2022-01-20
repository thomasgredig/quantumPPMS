#' returns coercive field of hysteresis loop
#' @param H applied magnetic field vector (Oe)
#' @param M magnetization vecotr (emu)
#' @return coercivity (Oe)
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' d$loop = vsm.get.hyst.loop(d$H, d$M)
#' d1 = subset(d, loop==1)
#' vsm.get.Coercivity(d1$H, d1$M)
#' @export
vsm.get.Coercivity <- function(H, M) {
  dM = abs(c(0,diff(M)))
  k = which(dM==max(dM))[1]
  k1 = which(abs(diff(sign(M)))==2)
  Hguess = H[k1]
  Hmin = min(abs(Hguess))
  k.pos = k1[which(abs(Hguess)==Hmin)]
  spline(M[(k.pos-2):(k.pos+2)],
         H[(k.pos-2):(k.pos+2)], xout=c(0))$y
}



#' returns saturation magnetization of hysteresis loops
#' @param data data frame with H, M, and T
#' @return matrix with 5 columns, Msat, Msat.sd, intercept, susceptibility, Temeprature
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' vsm.get.Msat(d)
#' @export
vsm.get.Msat <- function(data) {
  subset(data, (H>0.75*max(data$H) & H<0.95*max(data$H)) | (H< -0.75*max(data$H) & H> -0.95*max(data$H)))-> d3
  q = c(0,which(diff(d3$time)>5), nrow(d3))
  d3$loop = factor(rep(1:(length(q)-1),diff(q)))
  m1= split(d3,d3$loop)
  t(sapply(m1,function(x) {
    f=lm(data=x,M~H)$coeff[1:2]
    mcorr=x$M-x$H*f[2]
    c(M.sat=mean(mcorr),M.sat.sd=sd(mcorr),f[1],f[2],T=mean(x$T))
  }))
}
