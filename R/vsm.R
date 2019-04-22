#################################################
## functions for analyzing VSM data from the PPMS
## Date: August 23, 2015
## Author: Thomas Gredig
#################################################


#' Loads QD PPMS data from file
#'
#' @param fname filename including path
#' @return data frame with 5 data columns: time, T, H, M, Merr
#' @examples
#' d = ppms.load('ppms.dat')
#' @export
ppms.load <- function(fname) {
  d = read.csv(fname, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  d
}


#' Reads QD PPMS Header File Data
#'
#' @param fname filename including path
#' @return list
#' @examples
#' d = ppms.vsmdat.info('ppms.dat')
#' @export
ppms.vsmdat.info <- function(fname) {
  if (!file.exists(fname)) {
    warning(paste('Cannot find file:',fname))
    return;
  }

  scan(file = fname, nlines=23, what=character(0), sep='\n') -> header

  if ((length(header)>0) && (header[1]=='[Header]')) {
    title = substr(header[4],7,1000)
    measurement.date = strsplit(header[5],',')[[1]][3]
    measurement.time = strsplit(header[5],',')[[1]][4]
    measurement.type = strsplit(header[6],',')[[1]][2]
    mass = substr(header[9],6,1000)
    sample =substr(header[10],6,1000)
    comment = substr(header[11],6,1000)

    info = cbind(ppms = TRUE, title=title,
                 sample = sample,
                 type= measurement.type,
                 date = measurement.date,
                 time = measurement.time,
                 comment= comment,
                 mass = mass)
  } else {
    info = cbind(ppms = FALSE,
                 title=NA,
                 sample = NA,
                 type= NA,
                 date = NA,
                 time = NA,
                 comment= NA,
                 mass = NA)
  }
  sample.name=NA
  if (info[1]==TRUE) {
    pattern='.*(\\w\\w\\d{6}[A-Za-z0-9]*)'
    sample.name = str_match(paste(info[2],
                                  info[7],
                                  info[3],
                                  fname),pattern)[,2]
  }

  cbind(sample.name = sample.name, info)
}
