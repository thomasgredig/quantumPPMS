#' Returns characteristics of a PPMS data file. The
#' category number can be used to check whether the
#' same sequence was used to generated the data.
#'
#' @param Temp temperature vector (K)
#' @param Happ applied field vector (Oe)
#' @return data.frame() with length, lowest/highest temperature, lowest/highest magnetic field, number of
#' data points for hysteresis loops and number for susceptibility, cateogory value
#' @examples
#' filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
#' d = ppms.load(filename)
#' ppms.seqType(d$T, d$H)
#' @export
ppms.seqType <- function(Temp, Happ) {
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
