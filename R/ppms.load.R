empty.frame = data.frame(
  time = c(),  # time
  T = c(),  # temperature
  H = c(),  # applied field
  M = c(),  # magnetization
  Merr = c()  # uncertainty of magnetization
)

#' Loads QD PPMS data from file
#'
#' @param filename filename including path for PPMS file
#' @return data frame with 5 data columns: time, T, H, M, Merr
#' @examples
#' filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
#' d = ppms.load(filename)
#' print(paste('PPMS data loaded with ',nrow(d),'rows.'))
#' @export
ppms.load <- function(filename) {
  if ((!file.exists(filename)) | (file.info(filename)$size<2000)) {
    warning(paste("The following file cannot be loaded:",filename,'in', getwd()))
    return(empty.frame)
  }
  d = read.csv(filename, skip=23, header=F)[,2:6]
  names(d)=c('time', 'T','H','M','Merr')
  d[,'time']=d[,'time']-d[1,'time']
  na.omit(d)
}
