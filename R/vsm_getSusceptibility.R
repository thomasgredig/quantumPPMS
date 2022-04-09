#' Returns the susceptibility background (high H)
#'
#' for each temperature of the magnetic field sweep data
#'
#' @param obj VSMdata object
#' @param sweepDirection +1 or -1 for the direction to use for fit (-1 default)
#' @param singleLoop if \code{TRUE}, will evaluate only one loop regardless of temperature
#' @return VSM data frame with sweepData column
#'
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' d1 = vsm.getLoop(d,direction = -1)
#' vsm.getSusceptibility(d1, direction=-1)
#' @export
vsm.getSusceptibility <- function(obj,
                                  direction = -1,
                                  singleLoop=FALSE) {

  d = data.frame(
    H = obj@H,
    M = obj@M,
    Temp = obj@Temp,
    dir = obj@dir
  )

  if (singleLoop) {
    x1 = subset(d, H>0.7*max(d$H) & H<0.98*max(d$H))
    if (nrow(x1)>3) {
      lm(x1$M ~ x1$H) -> fit
      r = signif(summary(fit)$coeff[c(2,4)],4)
    } else {
      sort(d$H)[nrow(d)-2] -> maxH
      x1 = subset(d, H>0.7*maxH & H<0.98*maxH)
      if (nrow(x1)>3) {
        lm(x1$M ~ x1$H) -> fit
        r = signif(summary(fit)$coeff[c(2,4)],4)
      } else {
        r = c(NA,NA)
      }
    }
  } else {
    r = sapply(split(d, d$Temp), function(x) {
      x1 = subset(x, H>0.6*max(x$H) & H<0.95*max(x$H) & dir == direction)
      if(nrow(x1)>10){
        lm(x1$M ~ x1$H) -> fit
        r = signif(summary(fit)$coeff[c(2,4)],4)
      } else {
        r = c(NA,NA)
      }
    })
  }

  r
}
