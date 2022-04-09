#' Loads QD PPMS ACMS data from file
#'
#' @param filename filename including path for PPMS file
#' @return data frame with 9 data columns: time, T, M, freq, Ampl, Mdc, M1, M2, Mtot
#' @importFrom utils read.csv
#' @export
ppms.load.acms <- function(filename) {
  if ((!file.exists(filename)) | (file.info(filename)$size<2000)) {
    warning(paste("The following file cannot be loaded:",filename,'in', getwd()))
    return(NULL)
  }
  d = read.csv(filename, skip=20, header=TRUE)
  q = c(2,3,4,5,6,7,9,10,11)
  names(d)[q] =
    c('time.s','T.K','M.Oe','f.Hz','A.Oe','Mdc.emu','M1.emu','M2.emu','M.emu')
  d[,2]=d[,2]-d[1,2]
  d$f.Hz = factor(signif(d$f.Hz,3))
  d[,q]
}
