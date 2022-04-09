#' returns saturation magnetization of hysteresis loops
#' @param data data frame with H, M, and T
#' @return matrix with 5 columns, Msat, Msat.sd, intercept, susceptibility, Temeprature
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename, dataFrame=TRUE)
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
