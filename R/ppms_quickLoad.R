#' Loads several Quantum Design PPMS files at once.
#'
#' It will add a column with "no" numbering each file
#' The "type" column returns "hyst" for hysteresis loops
#' and "temp" for dc susceptiblility (M vs. T)
#'
#' @param ppms.file.list vector with full path, filenames
#' @param verbose if \code{TRUE} additional information
#' @return data frame with 7 data columns: time, T, H, M, Merr, type, no
#' @examples
#' file.list = vsm.getSampleFiles(type = 'all')[1]
#' d = ppms.quickLoad(file.list, verbose = FALSE)
#' @export
ppms.quickLoad <- function(ppms.file.list, verbose=TRUE) {
  r = data.frame()
  j = 0
  for(f in ppms.file.list) {
    if (vsm.validFile(f)) {
      d = vsm.import(f, dataFrame=TRUE, verbose=verbose)
      d$type = 'na'
      if (sd(d$T)<0.5) d$type='hyst'
      if (sd(d$H)<10) d$type='temp'
      j = j + 1
      d$no = j
      r=rbind(r, d)
    }
  }
  r
}
