#' Import VSM data from file
#'
#' @param filename filename including path for VSM file
#' @param dataFrame if \code{TRUE}, then will return data.frame compatible with v0.2 and before
#' @return VSMdata object
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' summary(d)
#' @importFrom utils read.csv
#' @importFrom stats "na.omit"
#' @export
vsm.import <- function(filename, dataFrame=FALSE) {
  # check whether it is a valid VSM file
  if (!vsm.validFile(filename)) {
    warning(paste("VSM file cannot be loaded:",filename))
    return(NULL)
  }

  d = read.csv(filename, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  d = na.omit(d)
  if (dataFrame) return(d)

  d1 = vsm.info(filename)

  VSMdata(
    time = d$time,
    T = d$T,
    H = d$H,
    M = d$M,
    Merr = d$Merr,
    description = d1$title,
    sampleName = d1$sample.name,
    fullFilename = filename
  )
}
