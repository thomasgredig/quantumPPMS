#' VSM file version
#'
#' reads the version as a number, if there are a different builds, those are added in the 3rd and 4th digit of
#' the number; i.e. 1.3702 for 1.37 Build 2
#'
#' @param filename name of the file to check (include path)
#' @param verbose output app name
#' @return version of VSM file
#'
#' @examples
#' filename = vsm.getSampleFiles()
#' q = sapply(filename, vsm.version)
#' names(q)=basename(names(q))
#' q
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
