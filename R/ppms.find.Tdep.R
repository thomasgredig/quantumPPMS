#' Tries to find out the deposition temperature of the sample
#'
#' @param path.raw path to RAW data files
#' @param sample.name sample name string
#' @return deposition temperature in degrees Celsius as a number
#' @examples
#' ppms.find.Tdep('SF170517')
#' @export
ppms.find.Tdep <- function(path.raw, sample.name) {
  Tdep = NA
  f = dir(path.raw, pattern=sample.name)
  k = grep('(\\d{2,3})C',f)
  if(length(k)>0) {
    n = gsub('.*\\D(\\d+)C.*','\\1',f[k])
    Tdep  = sort(factor(n), decreasing = TRUE)[1]
  } else {
    k = grep('\\W+RT',f)
    if (length(k)>0) {
      Tdep = 30
    }
  }
  Tdep
}
