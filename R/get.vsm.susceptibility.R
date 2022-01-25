#' Returns the susceptibility background (high H)
#'
#' for each temperature of the magnetic field sweep data
#'
#' @param obj VSMdata object
#' @param sweepDirection +1 or -1 for the direction to use for fit (-1 default)
#' @return VSM data frame with sweepData column
#' @examples
#' filename = vsm.getSampleFiles()
#' d = vsm.load(filename)
#' d1 = vsm.getLoop(d,1)
#' vsm.getSusceptibility(d1)
#' @export
vsm.getSusceptibility <- function(obj, sweepDirection = -1) {
  Temp = obj@Temp[1]
  d = data.frame(
    H = obj@H,
    M = obj@M,
    dir = obj@dir
  )

  sapply(split(d, Temp), function(x) {
    x1 = subset(x, H>0.6*max(x$H) & H<0.95*max(x$H) & dir == sweepDirection)
    if(nrow(x1)>10){
      lm(x1$M ~ x1$H) -> fit
      signif(summary(fit)$coeff[c(2,4)],3)
    } else {
      c(NA,NA)
    }
  })
}
