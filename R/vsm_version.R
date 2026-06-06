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

  header <- vsm.readHeader(filename)

  # Newer files: version is on INFO line with APPNAME
  no <- grep('APPNAME', header)
  appName <- if (length(no) > 0) header[no[1]] else NA_character_

  if (verbose) cat("Appname = ", appName, "\n")

  if (!is.na(appName)) {
    ver <- as.numeric(gsub('.*(1\\.\\d+)\\.*(\\d*).*', '\\1\\2', appName))

    if (grepl('Build', appName)) {
      ver <- ver + 0.0001 * as.numeric(gsub('.*Build (\\d+).*', '\\1', appName))
    }

  } else {
    # Older files: no APPNAME; version is in BYAPP line, e.g.
    # BYAPP,MPMS Measurement,1.1,Summary
    no <- grep('^BYAPP,', header)
    byApp <- if (length(no) > 0) header[no[1]] else NA_character_

    if (verbose) cat("BYAPP = ", byApp, "\n")

    if (is.na(byApp)) return(0)

    fields <- strsplit(byApp, ',', fixed = TRUE)[[1]]

    # Expected: BYAPP,<application>,<version>,...
    if (length(fields) < 3) return(0)

    ver <- as.numeric(fields[3])
  }

  if (is.na(ver)) return(0)

  if (verbose) cat("Version = ", ver, "\n")

  signif(ver, 5)
}

# Example Headers:
# ================
# OLDER version:
#
# [1] "[Header]"
# [2] "TITLE,MPMS DC Measurement"
# [3] "BYAPP,MPMS Measurement,1.1,Summary"
# [4] "FILEOPENTIME, 992641015.230000 6/15/2001, 4:36:55 PM"
# [5] "INFO, NAME, 053001B - Ta/Co(120)/CoO(20)/Ta/Si"
#
#
# NEWER version:
#
# [1] "[Header]"
# [2] "; VSM Data File (default extension .dat)"
# [3] "; Copyright (c) 2003-2018, Quantum Design, Inc. All rights reserved."
# [4] "TITLE,"
# [5] "FILEOPENTIME,3942573000.557,12/05/2024,2:07 pm"
# [6] "BYAPP,VSM,2.0,3.0"
# [7] "INFO,PPMS VSM Option Release 1.5.2 Build 1,APPNAME"



