#' Subset of VSMdata object
#'
#' @param obj VSMdata object
#' @param loop number of the loop
#' @param direction +1 or -1 for direction of applied field
#' @return VSMdata object with subset of selected loop
#' @examples
#' filename = vsm.getSampleFiles()
#' d = vsm.import(filename)
#' plot(d)
#' d1 = vsm.getLoop(d)
#' plot(d1)
#' @export
vsm.getLoop <- function(obj, loop=1, direction=1) {
  m1 = which(obj@loop==loop & obj@dir == direction)
  .subVSMdata(obj, m1)
}

###
NULL

.subVSMdata <- function(obj, m1) {
  nObj = obj

  nObj@time = nObj@time[m1]
  nObj@T = nObj@T[m1]
  nObj@H = nObj@H[m1]
  nObj@M = nObj@M [m1]
  nObj@Merr = nObj@Merr[m1]
  nObj@Temp = droplevels(nObj@Temp[m1])
  nObj@dir = nObj@dir[m1]
  nObj@loop = nObj@loop[m1]
  nObj@Mcorr = nObj@Mcorr[m1]
  nObj@type = nObj@type[m1]

  nObj
}
