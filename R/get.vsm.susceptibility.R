#' Returns the susceptibility background (high H) for
#' each temperature of the magnetic field sweep data
#'
#' @param data VSM data frame
#' @param my.SweepDirection +1 or -1 for the direction to use for fit (-1 default)
#' @return VSM data frame with sweepData column
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.load(filename)
#' d = get.vsm.sweepData(d)
#' d1 = subset(d, sweepData==1)
#' get.vsm.susceptibility(d1)
#' @export
get.vsm.susceptibility <- function(d, my.SweepDirection = -1) {
  d$Temp = factor(signif(d$T,2))
  d$dir = c(0,sign(diff(d$H)/diff(d$time)))
  sapply(split(d, d$Temp), function(x) {
    x1 = subset(x, H>0.6*max(x$H) & H<0.95*max(x$H) & dir == my.SweepDirection)
    lm(x1$M ~ x1$H) -> fit
    signif(summary(fit)$coeff[c(2,4)],3)
  })
}