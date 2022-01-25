#' Tries to find out the deposition temperature
#'
#' Retrieve deposition temperature of sample
#'
#' @param obj VSMdata object
#' @return deposition temperature in degrees Celsius as a number
#' @examples
#' d = vsm.load(vsm.getSampleFiles()[1])
#' vsm.guessTdep(d)
#' @export
vsm.guessTdep <- function(obj) {
  Tdep = NA
  f = paste(obj@description, obj@sampleName, obj@fullFilename)
  k = grep('(\\d{2,3})C',f)
  if(length(k)>0) {
    n = gsub('.*\\D(\\d+)C.*','\\1',f[k])
    Tdep  = as.numeric(n[sort(factor(n), decreasing = TRUE)[1]])
  } else {
    k = grep('\\W+RT',f)
    if (length(k)>0) {
      Tdep = 30
    }
  }
  Tdep
}
