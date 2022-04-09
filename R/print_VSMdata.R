#' print VSMdata object
#'
#' @param x VSMdata object
#' @param ... other summary parameters
#' @return summary of VSMdata object
#' @author Thomas Gredig
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' print(d)
#' @export
print.VSMdata <- function(x,...) {
  cat("Data points:           ",length(x@T),"\n")
  cat("Lowest T:              ",signif(min(x@T),4),"K\n")
  cat("Highest T:             ",signif(max(x@T),4),"K\n")
  cat("Total time:            ",round(max(x@time)/60,1),"min\n")
  cat("Number of loops:       ",nlevels(factor(x@loop)),"\n")
  cat("Number of M vs H loops:",.getNumberMvsHLoops(x),"\n")
  cat("Sample:                ",x@sampleName,"\n")
  cat("Description:           ",x@description,"\n")
  cat("Filename:              ",x@fullFilename,"\n")
}
