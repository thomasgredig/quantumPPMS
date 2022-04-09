#' Statistics for all Loops in VSM object
#'
#' This function loops through all hyst loops in VSMdata
#' using vsm.hystStats() and adds those parameters
#'
#' @param obj VSMdata object
#'
#' @author Thomas Gredig
#'
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' t(vsm.hystStats(d))
#' @importFrom stats spline coef lm
#' @importFrom utils tail
#' @export
vsm.hystStats <- function(obj) {
  loops = levels(factor(obj@loop))
  r = data.frame()
  for(l in loops) {
    q = vsm.hystStatsLoop(obj, loop=l, direction = 1)
    if (!is.null(q)) r = rbind(r, q)
    q = vsm.hystStatsLoop(obj, loop=l, direction = -1)
    if (!is.null(q)) r = rbind(r, q)
  }

  r
}
