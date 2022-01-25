#' OBSOLETE: Return only data of magnetic sweeps (hyst loop)
#'
#' @param obj PPMSdata object
#' @return VSM data frame with sweepData column
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' d = get.vsm.sweepData(d)
#' @export
get.vsm.sweepData <- function(obj) {
  m1 = which(obj@dir != 0)
  .subVSMdata(obj, m1)
}
