#' Loads several Quantum Design PPMS files at once.
#'
#' It will add a column with "no" numbering each file
#' The "type" column returns "hyst" for hysteresis loops
#' and "temp" for dc susceptiblility (M vs. T)
#'
#' @param ppms.file.list vector with full path, filenames
#' @return data frame with 7 data columns: time, T, H, M, Merr, type, no
#' @examples
#' file.list = vsm.getSampleFiles()
#' d = ppms.quickLoad(file.list)
#' @export
ppms.quickLoad <- function(ppms.file.list) {
  r = data.frame()
  j = 0
  for(f in ppms.file.list) {
    if (file.info(f)$size>2000) {
      d = ppms.load(f)
      d$type = 'na'
      if (sd(d$T)<0.5) d$type='hyst'
      if (sd(d$H)<10) d$type='temp'
      j = j + 1
      d$no = j
      r=rbind(r, d)
    }
  }
  r
}


#' OBSOLETE: Returns characteristics of a PPMS data file.
#'
#' use summary(PPMSdata) instead
#'
#' The category number can be used to check whether the
#' same sequence was used to generated the data.
#'
#' @param Temp temperature vector (K)
#' @param Happ applied field vector (Oe)
#' @return data.frame() with length, lowest/highest temperature, lowest/highest magnetic field, number of
#' data points for hysteresis loops and number for susceptibility, cateogory value
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' ppms.seqType(d$T, d$H)
#' @export
ppms.seqType <- function(Temp, Happ) {
  warning("Obsolete: use summary(PPMSdata) instead.")
  n = data.frame(
    len = length(Temp),
    T.low = signif(min(Temp),2),
    T.high = signif(max(Temp),2),
    H.low = signif(min(Happ),3),
    H.high = signif(max(Happ), 3),
    numHystLoop = length(which(abs(diff(Happ))>10)),
    numSuscept = length(which(abs(diff(Temp))>0.03))
  )
  n$cat = signif(log10(n$len/n$numHystLoop)+log10(n$T.high-n$T.low) + log10(n$H.high-n$H.low),2)
  n
}




#' OBSOLETE: Returns a vector that separates different hysteresis loops
#'
#' @param H applied magnetic field (Oe)
#' @param M magnetization (emu)
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' d$loop = ppms.vsm.hystLoops(d$H, d$M)
#' head(d)
#' @export
ppms.vsm.hystLoops <- function(H,M) {
  # separate all hyst loops
  dH = c(0,diff(H))
  # for(j in 1:3) {
  #   dH3 = c(0,abs(diff(dH)))
  #   q = which(dH3 > sd(dH3)*2)
  #   dH[q] <- dH[q-2]
  # }
  dH2 = c(0, diff(sign(dH)))
  q = c(0,which(abs(dH2)==2),length(H))
  f1 = diff(c(q[which(diff(q)>2)],length(H)))
  rep(1:length(f1),times=f1)
}
