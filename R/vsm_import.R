#' Import VSM data from file
#'
#' @param filename filename including path for VSM file
#' @param dataFrame if \code{TRUE}, then will return data.frame compatible with v0.2 and before
#' @param verbose if \code{TRUE} output additional information
#' @return VSMdata object
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' summary(d)
#' @importFrom utils read.csv
#' @importFrom stats "na.omit"
#' @export
vsm.import <- function(filename, dataFrame=FALSE, verbose=FALSE) {
  # check whether it is a valid VSM file
  if (!vsm.validFile(filename)) {
    if (verbose) cat("This is not a valid VSM file.")
    warning(paste("VSM file cannot be loaded:",filename))
    return(NULL)
  }

  if (file.info(filename)$size < 1700) {
    if (verbose) cat("VSM file is too short or empty.")
    warning(paste("VSM file is too short or empty:",filename))
    return(NULL)
  }

  v = vsm.version(filename,verbose=verbose)
  skipLEN = NULL

  if (v==1.0914) skipLEN = list(20,20,TRUE, cols=c(2,3,4,11,8))
  if (v==1.2401) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.36) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.3702) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.4601) skipLEN = list(30,30,TRUE, cols=c(1,4,3,5,6))  ## not fully tested
  if (v==1.5667) skipLEN = list(30,30,TRUE, cols=c(1,4,3,5,6))
  if (v==1.54) skipLEN = list(31,31,TRUE, cols=c(1,4,3,5,6))
  if (v==1.56) skipLEN = list(19,19,TRUE, cols=c(2,4,5,12,15))


  if (is.null(skipLEN)) {
    warning("Unknown PPMS version:", v)
    return(NULL)
  }

  # find starting point
  readLines(filename, n=35) -> q
  skipLength = which(q=='[Data]')
  if (!skipLEN[[3]]) skipLength = skipLength + 1
  if (skipLEN[[2]] != skipLength) {
    skipLEN[[2]] = skipLength
    warning(paste('VSM.IMPORT: Unusual header in file:', filename))
  }

  d = read.csv(filename, skip = skipLEN[[2]], header=skipLEN[[3]])
  d = d[,skipLEN$cols]
  if (verbose) cat("Original headings:",names(d))
  if ("AC.Mag" == names(d)[1]) {
    if (verbose) cat("AC susceptibility file; cannot VSM import.")
    return(NULL)
  }

  names(d)=c('time', 'T','H','M','Merr')
  if (v==1.56) names(d)=c('time', 'T','H','I.uA','V')
  d[,'time']=  d[,'time']-d[1,'time']

  d = na.omit(d)
  if (dataFrame) return(d)
  if (nrow(d)==0) return(NULL)

  if (v==1.56) {
    warning("Not VSM data, but resistance data; use dataFrame=TRUE.")
    return(NULL)
  }

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
