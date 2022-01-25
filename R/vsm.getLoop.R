#' Subset of VSMdata object
#'
#' @param obj VSMdata object
#' @param loop number of the loop
#' @return VSMdata object with subset of selected loop
#' @examples
#' filename = vsm.getSampleFiles()
#' d = ppms.load(filename)
#' vsm.getLoop(d)
#' @export
vsm.getLoop <- function(obj, loop=1) {
  nObj = obj
  l = .getVsmLoop(obj)
  m1 = which(l==loop)

  nObj@time = nObj@time[m1]
  nObj@T = nObj@T[m1]
  nObj@H = nObj@H[m1]
  nObj@M = nObj@M [m1]
  nObj@Merr = nObj@Merr[m1]
  nObj@Temp = nObj@Temp[m1]
  nObj@dir = nObj@dir[m1]

  nObj
}
