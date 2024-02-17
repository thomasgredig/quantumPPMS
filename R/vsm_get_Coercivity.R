#' returns coercive field of hysteresis loop
#' @param H applied magnetic field vector (Oe)
#' @param M magnetization vecotr (emu)
#' @return coercivity (Oe)
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' d1 = vsm.getLoop(d, lp=1, direction=1 )
#' df = vsm.data.frame(d1)
#' vsm.get.Coercivity(df$H, df$M)
#' @importFrom stats spline
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
