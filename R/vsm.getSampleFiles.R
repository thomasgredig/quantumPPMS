#' Sample PPMS sample data files
#'
#' @description
#' returns sample data files with PPMS data
#' @return vector with path/filename to data files
#' @author Thomas Gredig
#' @examples
#' file.list = vsm.getSampleFiles()
#' print(paste("Found",length(file.list),"data files."))
#' @export
vsm.getSampleFiles <- function(type='*') {
  pfad = system.file("extdata",package="quantumPPMS")
  file.list = dir(system.file("extdata",package="quantumPPMS"))
  file.path(pfad, file.list)
}
