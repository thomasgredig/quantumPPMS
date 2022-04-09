#' Convert VSMdata object to data.frame()
#'
#' @param obj VSMdata object
#'
#' @returns data frame with VSM data
#' @author Thomas Gredig
#'
#' @examples
#' library(ggplot2)
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' q = vsm.data.frame(d)
#' head(q)
#' ggplot(q, aes(H, M, col=loop)) + geom_point()
#' @export
vsm.data.frame <- function(obj) {
  df = data.frame(
    time = obj@time,
    T = obj@T,
    H = obj@H,
    M = obj@M,
    Mcorr = obj@Mcorr,
    loop = factor(obj@loop)
  )

  df
}
