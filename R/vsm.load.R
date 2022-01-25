#' Loads QD VSM data from file
#'
#' @param filename filename including path for VSM file
#' @param dataFrame if \code{TRUE}, then will return data.frame compatible with v0.2 and before
#' @return VSMdata object
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = ppms.load(filename)
#' summary(d)
#' @export
vsm.load <- function(filename, dataFrame=FALSE) {
  if ((!file.exists(filename)) | (file.info(filename)$size<2000)) {
    warning(paste("The following file cannot be loaded:",filename))
    return(NULL)
  }
  d = read.csv(filename, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  d = na.omit(d)
  if (dataFrame) return(d)

  VSMdata(
    time = d$time,
    T = d$T,
    H = d$H,
    M = d$M,
    Merr = d$Merr,
    fullFilename = filename
  )
}
