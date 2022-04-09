#' plot VSMdata object
#'
#' @param x VSMdata object
#' @param ... other summary parameters
#' @return summary of VSMdata object
#' @author Thomas Gredig
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' plot(d)
#' @export
plot.VSMdata <- function(x,...) {
  if (length(x@H)==0) { return("VSMdata object has no data to plot.") }
  if (x@type[1]=="MvsH") {
    plot(x@H, x@M*1e6, col='red', xlab='H (Oe)', ylab="M (uemu)")
  } else {
    plot(x@T, x@M, col='red', xlab='T (K)', ylab="M (uemu)")
  }
}
