#' VSM DAT file
#'
#' @param filename name of the file to check (include path)
#' @return \code{TRUE} if it is a MPMS or VSM DAT file
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' vsm.validFile(filename)
#' @export
vsm.validFile <- function(filename) {
  if (!file.exists(filename)) return(FALSE)
  if (file.info(filename)$size<2000) return(FALSE)
  scan(file = filename, nlines=5, what=character(0), sep='\n', quiet = TRUE) -> header
  length(grep('[VSM|MPMS]', header))>0
}


#' VSM DAT file
#'
#' @param filename name of the file to check (include path)
#' @param verbose output app name
#' @return version of VSM file
#'
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' vsm.version(filename)
#'
#' @export
vsm.version <- function(filename, verbose=FALSE) {
  if (!file.exists(filename)) return(0)
  scan(file = filename, nlines=8, what=character(0), sep='\n', quiet = TRUE) -> header

  grep('APPNAME', header) -> no
  appName = header[no]
  if (verbose) cat("Appname = ",appName,"\n")
  ver = as.numeric(gsub('.*(1\\.\\d+)\\.*(\\d*).*', '\\1\\2',  appName))

  if (grepl('Build',appName)) ver = ver + 0.0001*as.numeric(gsub('.*Build (\\d+).*','\\1',appName))
  if (verbose) cat("Version = ",ver,"\n")

  signif(ver,5)
}
