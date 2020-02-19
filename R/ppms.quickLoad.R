#' Loads several Quantum Design PPMS files at once.
#'
#' It will add a column with "no" numbering each file
#' The "type" column returns "hyst" for hysteresis loops
#' and "temp" for dc susceptiblility (M vs. T)
#'
#' @param ppms.file.list vector with full path, filenames
#' @return data frame with 7 data columns: time, T, H, M, Merr, type, no
#' @examples
#' file.list = c(system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS"))
#' d = ppms.quickLoad(file.list)
#' @export
ppms.quickLoad <- function(ppms.file.list) {
  r = data.frame()
  j = 0
  for(f in ppms.file.list) {
    if (file.info(f)$size>2000) {
      d = ppms.load(f)
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
