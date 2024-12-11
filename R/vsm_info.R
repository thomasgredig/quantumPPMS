#' Reads VSM data file header
#'
#' also returns the PPMS option (VSM,ACMS,LogData,Resistivity)
#'
#' @param filename filename including path
#' @return data frame
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' vsm.info(filename)
#' @export
vsm.info <- function(filename) {
  # check if file exists
  if (!file.exists(filename)) {
    warning(paste('Cannot find file:',filename))
    return()
  }

  v = vsm.version(filename)
  # if (v==1.5667) skipLEN = list(30,30,TRUE, cols=c(1,4,3,5,6))
  # if (v==1.56) skipLEN = list(19,19,TRUE, cols=c(2,4,5,7,8))
  # if (v==1.0914) skipLEN = list(20,20,TRUE, cols=c(2,3,4,7,8))
  # if (v==1.2401) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  # if (v==1.36) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  # if (v==1.3702) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))

  if (v==1.0914) skipLEN = list(20,20,TRUE, cols=c(2,3,4,11,8))
  if (v==1.2401) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.36) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.3702) skipLEN = list(22,23,FALSE, cols=c(2,3,4,5,6))
  if (v==1.4601) skipLEN = list(30,30,TRUE, cols=c(1,4,3,5,6))  ## not fully tested
  if (v==1.5201) skipLEN = list(34,35,FALSE, cols=c(2,3,4,5,6))
  if (v==1.54) skipLEN = list(31,31,TRUE, cols=c(1,4,3,5,6))
  if (v==1.56) skipLEN = list(19,19,TRUE, cols=c(2,4,5,12,15))
  if (v==1.5667) skipLEN = list(20,21,FALSE, cols=c(1,4,3,5,6))


  scan(file = filename, nlines=skipLEN[[1]], what=character(0), sep='\n', quiet = TRUE) -> header

  d=data.frame()
  if ((length(header)>0) && (header[1]=='[Header]')) {
    ppms.option = gsub(' ','',strsplit(header[grep('^BYAPP,',header)],',')[[1]][2])

    title = gsub('TITLE,','',header[grep('^TITLE', header)])
    # [1] "FILEOPENTIME" "5500334.30"   "09/21/2018"   "4:50 pm"
    filedate =   as.character(strptime(paste(gsub(',','',strsplit(header[grep('FILEOPENTIME,',header)],' ')[[1]][c(3,4,5)]), collapse=' '),
                                       format='%m/%d/%Y %I:%M:%S %p'))

    # filedate =   as.character(strptime(paste(strsplit(header[grep('FILEOPENTIME,',header)],',')[[1]][c(3,4)], collapse=' '),
    #                                    format='%m/%d/%Y %I:%M %p'))
    dl.appname = grep('APPNAME',header)
    appname = gsub(',\\s*','',gsub('INFO','',gsub('APPNAME','',header[dl.appname])))
    header = header[-dl.appname]

    info.str = gsub('^INFO,','',header[grep('INFO',header)])
    if (ppms.option == 'ACMS') {
      attr = info.str[1:4]
      attr.names = paste0('ACMS.INFO',1:4)
    } else {
      attr = gsub('\\s*(.*)[,:][^,]+','\\1',info.str)
      attr.names = gsub('.*[,:]\\s*([^,]+)','\\1',info.str)
      if (v==1.5667) {
        tmp = attr
        attr = attr.names
        attr.names = tmp
      }
    }

    d = data.frame(rbind(c(ppms.option, title, filedate, appname, attr)), stringsAsFactors = FALSE)
    names(d) = c('option','title','file.open.time','AppName', attr.names)

    # guess the sample name
    sample_name = gsub('.*([A-Z]{2,3}\\d{6,8}[a-zA-Z]{0,2}\\d{0,1}).*','\\1',
         paste(paste(d, collapse=' == '),
               filename))
    if(nchar(sample_name)>20) {
      m <- grep('SAMPLE_COMMENT',header)
      sample_name = ""
      if(length(m)>0L) {
        sample_name = header[m[1]]
        gsub("INFO,","",sample_name) -> sample_name
        gsub(",SAMPLE_COMMENT","",sample_name) -> sample_name
      } else {
        m <- grep('COMMENT',header)
        if(length(m)>0L) {
          sample_name = header[m[1]]
        }
      }
    }
    d$sample.name = sample_name
  }

  d
}
