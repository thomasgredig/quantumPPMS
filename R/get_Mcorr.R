#' removes background susceptibility
#'
#' from all magnetic sweep data
#'
#' @param d VSM data frame
#' @return added column Mcorr with the corrected magnetization value
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename, dataFrame=TRUE)
#' head(get.Mcorr(d))
#' @export
get.Mcorr <- function(d) {
  if (!('H' %in% names(d))) {
    warning("data frame d not compatible with get.Mcorr()")
  }

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
