#' Reads the VSM file header
#' @param filename VSM filename usually ends with .dat
#' @export
vsm.readHeader <- function(filename, returnLinesOnly = FALSE) {
  if (!file.exists(filename)) return(character(0))

  con <- file(filename, open = "r")
  nLines <- 0
  on.exit(close(con), add = TRUE)


  header <- character(0)

  repeat {
    line <- readLines(con, n = 1, warn = FALSE)
    nLines = nLines + 1

    if (length(line) == 0) break
    if (trimws(line) == "[Data]") break


    header <- c(header, line)
  }

  if (returnLinesOnly) return(nLines)
  header
}
