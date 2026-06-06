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
  if (is.null(obj)) {
    return(data.frame())
  }

  required.slots <- c("time", "T", "H", "M", "Merr", "loop")

  missing.slots <- required.slots[!required.slots %in% slotNames(obj)]

  if (length(missing.slots) > 0) {
    stop(
      "Object is missing required slot(s): ",
      paste(missing.slots, collapse = ", "),
      call. = FALSE
    )
  }

  n <- length(obj@time)

  slot.lengths <- vapply(
    required.slots,
    function(s) length(slot(obj, s)),
    integer(1)
  )

  if (any(slot.lengths != n)) {
    stop(
      "Required slots do not have equal lengths: ",
      paste(names(slot.lengths), slot.lengths, sep = "=", collapse = ", "),
      call. = FALSE
    )
  }

  data.frame(
    time = obj@time,
    T.K = obj@T,
    H.Oe = obj@H,
    M.emu = obj@M,
    Merr.emu = obj@Merr,
    loop = factor(obj@loop)
  )
}
