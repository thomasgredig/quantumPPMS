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
