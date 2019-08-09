#' Returns a vector that separates different hysteresis loops
#'
#' @param hyst hyst data frame
#' @return list
#' @examples
#' filename = dir(pattern='DAT$', recursive=TRUE)[1]
#' d = ppms.load(filename)
#' d$loop = ppms.vsm.hystLoops(d$H, d$M)
#' @export
ppms.vsm.hystLoops <- function(H,M) {
  # separate all hyst loops
  dH = c(0,diff(H))
  dH2 = c(0, diff(sign(dH)))
  q = c(0,which(abs(dH2)==2),length(H))
  f1 = diff(c(q[which(diff(q)>2)],length(H)))
  rep(1:length(f1),times=f1)
}