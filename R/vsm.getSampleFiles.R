#' Sample PPMS sample data files
#'
#' @description
#' returns sample data files with PPMS data
#' @param type can be "all" or "version" for set of different version files for testing
#' @return vector with path/filename to data files
#' @author Thomas Gredig
#' @examples
#' vsm.getSampleFiles("version")
#' file.list = vsm.getSampleFiles()
#' print(paste("Found",length(file.list),"data files."))
#' @export
vsm.getSampleFiles <- function(type='all') {
  pfad <- system.file("extdata",package="quantumPPMS")
  file.list <- dir(system.file("extdata",package="quantumPPMS"))
  file.list <- file.path(pfad, file.list)

  if (type=='version') file.list = file.list[grep('sample',file.list)]
  if (type=='empty') file.list = file.list[grep('empty',file.list)]

  file.list
}
