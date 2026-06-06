#' VSM/MPMS file information
#'
#' Reads metadata from the file header.
#'
#' Supports:
#'   Newer PPMS/VSM: INFO,value,FIELDNAME
#'   Legacy MPMS:    INFO,FIELDNAME,value
#'   Legacy MPMS:    INFO,FIELDNAME: value
#'
#' @param filename name of the file to read
#' @return one-row data.frame with file metadata
#' @export
vsm.info <- function(filename) {
  if (!file.exists(filename)) {
    warning(paste("Cannot open file:", filename))
    return(data.frame())
  }

  header <- vsm.readHeader(filename)

  if (length(header) == 0 || trimws(header[1]) != "[Header]") {
    warning(paste("No valid VSM/MPMS header found in:", filename))
    return(data.frame())
  }

  get_first_line <- function(pattern, x = header) {
    m <- grep(pattern, x)
    if (length(m) == 0) return(NA_character_)
    x[m[1]]
  }

  clean_name <- function(x) {
    x <- trimws(x)
    x <- gsub("[^[:alnum:]_.]+", ".", x)
    x <- gsub("^\\.+|\\.+$", "", x)
    x
  }

  parse_file_time <- function(line) {
    if (is.na(line)) return(NA_character_)

    fields <- trimws(strsplit(line, ",", fixed = TRUE)[[1]])

    # Newer format:
    # FILEOPENTIME,3942573000.557,12/05/2024,2:07 pm
    if (length(fields) >= 4) {
      dt <- paste(fields[3], fields[4])

      out <- suppressWarnings(strptime(dt, format = "%m/%d/%Y %I:%M:%S %p"))
      if (is.na(out)) {
        out <- suppressWarnings(strptime(dt, format = "%m/%d/%Y %I:%M %p"))
      }

      if (!is.na(out)) return(as.character(out))
    }

    # Older format:
    # FILEOPENTIME, 1164777384.812000 11/28/2006, 9:16:24 PM
    txt <- sub("^FILEOPENTIME,\\s*", "", line)
    txt <- gsub(",", "", txt)
    parts <- strsplit(trimws(txt), "\\s+")[[1]]

    if (length(parts) >= 4) {
      dt <- paste(parts[(length(parts) - 2):length(parts)], collapse = " ")

      out <- suppressWarnings(strptime(dt, format = "%m/%d/%Y %I:%M:%S %p"))
      if (is.na(out)) {
        out <- suppressWarnings(strptime(dt, format = "%m/%d/%Y %I:%M %p"))
      }

      if (!is.na(out)) return(as.character(out))
    }

    NA_character_
  }

  parse_byapp <- function(line) {
    if (is.na(line)) {
      return(list(option = NA_character_, byapp.version = NA_character_))
    }

    fields <- trimws(strsplit(line, ",", fixed = TRUE)[[1]])

    option <- if (length(fields) >= 2) fields[2] else NA_character_
    version <- if (length(fields) >= 3) fields[3] else NA_character_

    list(
      option = gsub("\\s+", "", option),
      byapp.version = version
    )
  }

  is_legacy_info <- function(header) {
    info.lines <- grep("^INFO,", header, value = TRUE)

    if (length(info.lines) == 0) return(FALSE)

    info2 <- vapply(
      strsplit(info.lines, ",", fixed = TRUE),
      function(x) if (length(x) >= 2) trimws(x[2]) else "",
      character(1)
    )

    legacy.keys <- c(
      "APPNAME",
      "NAME",
      "WEIGHT",
      "AREA",
      "LENGTH",
      "SHAPE",
      "COMMENT"
    )

    any(info2 %in% legacy.keys) ||
      any(grepl("^\\s*(SEQUENCE FILE|BACKGROUND DATA FILE)\\s*:", info2))
  }

  parse_appname <- function(header, legacy = FALSE) {
    line <- get_first_line("APPNAME", header)

    if (is.na(line)) {
      return("MPMS (Legacy)")
    }

    fields <- trimws(strsplit(line, ",", fixed = TRUE)[[1]])

    if (legacy) {
      # Legacy:
      # INFO, APPNAME, MPMS MultiVu Application, Revision 1.56,  Build 67
      if (length(fields) >= 3) {
        return(trimws(paste(fields[-c(1, 2)], collapse = ",")))
      }
    } else {
      # Newer:
      # INFO,PPMS VSM Option Release 1.5.2 Build 1,APPNAME
      if (length(fields) >= 3) {
        return(fields[2])
      }
    }

    x <- line
    x <- gsub("^INFO,?", "", x)
    x <- gsub(",?APPNAME$", "", x)
    trimws(x)
  }

  parse_info_lines <- function(header, legacy = FALSE) {
    info.lines <- grep("^INFO,", header, value = TRUE)

    # APPNAME is handled separately
    info.lines <- info.lines[!grepl("APPNAME", info.lines)]

    if (length(info.lines) == 0) {
      return(list(values = character(0), names = character(0)))
    }

    parts <- strsplit(info.lines, ",", fixed = TRUE)

    if (legacy) {
      # Legacy formats:
      #   INFO, NAME, FePc powder in capsule
      #   INFO, WEIGHT, 0.000
      #   INFO, SEQUENCE FILE: FePc_MvsT.seq
      #   INFO, BACKGROUND DATA FILE:, None

      names <- vapply(
        parts,
        function(x) {
          if (length(x) >= 3 && grepl(":", x[2], fixed = TRUE)) {
            trimws(sub(":.*$", "", x[2]))
          } else if (length(x) >= 3) {
            trimws(x[2])
          } else if (length(x) == 2 && grepl(":", x[2], fixed = TRUE)) {
            trimws(sub(":.*$", "", x[2]))
          } else if (length(x) == 2) {
            trimws(x[2])
          } else {
            NA_character_
          }
        },
        character(1)
      )

      values <- vapply(
        parts,
        function(x) {
          if (length(x) >= 3 && grepl(":", x[2], fixed = TRUE)) {
            v1 <- trimws(sub("^[^:]*:\\s*", "", x[2]))
            v2 <- trimws(paste(x[-c(1, 2)], collapse = ","))
            trimws(paste(c(v1, v2)[nzchar(c(v1, v2))], collapse = ", "))
          } else if (length(x) >= 3) {
            trimws(paste(x[-c(1, 2)], collapse = ","))
          } else if (length(x) == 2 && grepl(":", x[2], fixed = TRUE)) {
            trimws(sub("^[^:]*:\\s*", "", x[2]))
          } else {
            ""
          }
        },
        character(1)
      )

    } else {
      # Newer format:
      #   INFO,MoI3,SAMPLE_MATERIAL
      #   INFO,MoI3 Diana Lopez...,SAMPLE_COMMENT

      names <- vapply(
        parts,
        function(x) {
          if (length(x) >= 3) trimws(x[length(x)]) else NA_character_
        },
        character(1)
      )

      values <- vapply(
        parts,
        function(x) {
          if (length(x) >= 3) {
            trimws(paste(x[2:(length(x) - 1)], collapse = ","))
          } else {
            ""
          }
        },
        character(1)
      )
    }

    keep <- !is.na(names) & nzchar(names)
    names <- clean_name(names[keep])
    values <- values[keep]

    names <- make.unique(names, sep = ".")

    list(values = values, names = names)
  }

  byapp <- parse_byapp(get_first_line("^BYAPP,", header))

  title.line <- get_first_line("^TITLE", header)
  title <- if (is.na(title.line)) {
    NA_character_
  } else {
    sub("^TITLE,?", "", title.line)
  }

  filedate <- parse_file_time(get_first_line("^FILEOPENTIME,", header))

  legacy <- is_legacy_info(header)

  appname <- parse_appname(header, legacy = legacy)

  info <- parse_info_lines(header, legacy = legacy)

  values <- c(
    option = byapp$option,
    title = title,
    file.open.time = filedate,
    AppName = appname,
    info$values
  )

  names(values) <- c(
    "option",
    "title",
    "file.open.time",
    "AppName",
    info$names
  )

  d <- as.data.frame(as.list(values), stringsAsFactors = FALSE)

  guess_sample_name <- function(d) {
    has_value <- function(nm) {
      nm %in% names(d) && !is.na(d[[nm]]) && nzchar(trimws(d[[nm]]))
    }

    # Examples:
    #   SF20170517
    #   TS100928Si1
    #   TS101218Au
    #
    # Two letters followed by 4-8 digits, optionally followed by letters/digits.
    sample_id_pattern <- "[A-Za-z]{2}[0-9]{4,8}[A-Za-z0-9]*"

    if (has_value("NAME")) {
      return(trimws(d$NAME))
    }

    # Prefer SAMPLE_COMMENT over SAMPLE_MATERIAL when COMMENT contains
    # a real sample identifier.
    if (has_value("SAMPLE_COMMENT") &&
        grepl(sample_id_pattern, d$SAMPLE_COMMENT)) {
      return(trimws(d$SAMPLE_COMMENT))
    }

    if (has_value("SAMPLE_MATERIAL")) {
      return(trimws(d$SAMPLE_MATERIAL))
    }

    if (has_value("SAMPLE_COMMENT")) {
      return(trimws(d$SAMPLE_COMMENT))
    }

    if (has_value("title") &&
        grepl(sample_id_pattern, d$title)) {
      return(trimws(d$title))
    }

    ""
  }

  d$sample.name <- guess_sample_name(d)

  d
}



