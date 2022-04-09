#' summary of VSMdata object
#'
#' @param object VSMdata object
#' @param ... other summary parameters
#' @return summary of VSMdata object
#' @author Thomas Gredig
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' summary(d)
#' @export
summary.VSMdata <- function(object,...) {
  numHyst = length(which(abs(diff(object@dir)) >= 1))
  nLen = length(object@T)
  r = data.frame(
    dataPoints = nLen,
    maxT.K = max(object@T),
    minT.K = min(object@T),
    maxH.Oe = max(object@H),
    minH.Oe = min(object@H),
    numHystLoop = numHyst,
    numSuscept = length(which(abs(diff(object@T))>0.03)),
    numLoops = nlevels(factor(object@loop)),
    categoryID = signif(log10(nLen/numHyst) +
                          log10(max(object@T) - min(object@T)) +
                          log10(max(object@H) - min(object@H)),2),
    sample = object@sampleName
  )
  r
}
