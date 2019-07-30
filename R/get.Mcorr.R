#' removes background susceptibility from all
#' magnetic sweep data
#'
#' @param d VSM data frame
#' @return added column Mcorr with the corrected magnetization value
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.load(filename)
#' d = get.vsm.sweepData(d)
#' d1 = subset(d, sweepData==1)
#' d1$Mcorr = get.Mcorr(d1)
#' @export
get.Mcorr <- function(d) {
  d$Temp = factor(signif(d$T,2))
  d$dir = c(0,sign(diff(d$H)/diff(d$time)))
  l1 = split(d, d$Temp)
  sapply(l1, function(x) {
    x1 = subset(x, H>0.5*max(x$H) & H<0.95*max(x$H) & dir == -1)
    lm(x1$M ~ x1$H) -> fit
    x$slope = summary(fit)$coeff[2]
    x$slope
  }) -> m1
  d$slope = unlist(m1)
  d$M - d$slope*d$H
}
