#' Reads QD PPMS Header File Data (General)
#' also returns the PPMS option ("VSM","ACMS","LogData","Resistivity")
#'
#' @param fname filename including path
#' @return data frame
#' @examples
#' filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
#' d = ppms.dat.info2(filename)
#' @export
ppms.dat.info2 <- function(fname) {
  # check if file exists
  if (!file.exists(fname)) {
    warning(paste('Cannot find file:',fname))
    return()
  }

  scan(file = fname, nlines=35, what=character(0), sep='\n') -> header

  d=data.frame()
  if ((length(header)>0) && (header[1]=='[Header]')) {
    ppms.option = gsub(' ','',strsplit(header[grep('^BYAPP,',header)],',')[[1]][2])

    title = gsub('TITLE,','',header[grep('^TITLE', header)])
    filedate = as.Date(strsplit(header[grep('FILEOPENTIME,',header)],',')[[1]][3], format='%m/%d/%Y')
    dl.appname = grep('APPNAME',header)
    appname = gsub(',','',gsub('INFO','',gsub('APPNAME','',header[dl.appname])))
    header = header[-dl.appname]

    info.str = gsub('^INFO,','',header[grep('INFO',header)])
    if (ppms.option == 'ACMS') {
      attr = info.str[1:4]
      attr.names = paste0('ACMS.INFO',1:4)
    } else {
      attr = gsub('(.*),[^,]+','\\1',info.str)
      attr.names = gsub('.*,([^,]+)','\\1',info.str)
    }

    d = data.frame(rbind(c(ppms.option, title, filedate, appname, attr)), stringsAsFactors = FALSE)
    names(d) = c('option','title','file.open.time','AppName', attr.names)

    # guess the sample name
    d$sample.name = gsub('.*([A-Z]{2,3}\\d{6,8}[a-zA-Z]{0,2}\\d{0,1}).*','\\1',
                         paste(paste(d, collapse=' == '),
                               fname))
  }
  d
}

#' OBSOLETE: Reads QD PPMS Header File Data
#'
#' @param fname filename including path (OBSOLETE)
#' @return list
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.dat.info(filename)
#' @export
ppms.dat.info <- function(fname) {
  if (!file.exists(fname)) {
    warning(paste('Cannot find file:',fname))
    return()
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
    sample.name = gsub('.*(\\w\\w\\d{8}[A-Za-z0-9]*).*','\\1', paste(info[2],
                                                                   info[7],
                                                                   info[3],
                                                                   fname))
    # sample.name = str_match(paste(info[2],
    #                               info[7],
    #                               info[3],
    #                               fname),pattern)[,2]
  }

  cbind(sample.name = sample.name, info)
}
