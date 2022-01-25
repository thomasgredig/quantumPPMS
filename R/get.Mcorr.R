#' Removes background susceptibility
#'
#' from all magnetic sweep data
#'
#' @param obj PPMSdata object
#' @return Mcorr
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = ppms.load(filename)
#' get.Mcorr(d)
#' @export
get.Mcorr <- function(obj) {
  # obj@Temp = factor(signif(obj@T,2))
  # obj@dir = c(0,sign(diff(obj@H)/diff(obj@time)))
  d = data.frame(M = obj@M,
                 H = obj@H,
                 dir = obj@dir)
  Temp = obj@Temp
  l1 = split(d, Temp)
  sapply(l1, function(x) {
    x1 = subset(x, H>0.5*max(x$H) & H<0.95*max(x$H) & x$dir == -1)
    lm(x1$M ~ x1$H) -> fit
    x$slope = summary(fit)$coeff[2]
    x$slope
  }) -> m1
  slope = unlist(m1)
  obj@M - slope * obj@H
}
