#' Subset of VSMdata object
#'
#' @param obj VSMdata object
#' @param lp number of the loop
#' @param direction +1 or -1 for direction of applied field
#' @return VSMdata object with subset of selected loop
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' plot(d)
#' d1 = vsm.getLoop(d)
#' plot(d1)
#' @export
vsm.getLoop <- function(obj, lp=1, direction=1) {
  m1 = which(obj@loop==lp & obj@dir == direction)
  .subVSMdata(obj, m1)
}
