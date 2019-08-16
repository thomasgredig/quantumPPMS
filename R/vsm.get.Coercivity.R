#' returns coercive field of hysteresis loop
#' @param H applied magnetic field vector (Oe)
#' @param M magnetization vecotr (emu)
#' @return coercivity (Oe)
#' @examples
#' filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
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
