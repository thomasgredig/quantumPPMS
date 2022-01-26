#' VSM data class
#'
#' A S4 class to store and manipulate VSM data
#'
#' @slot time time in seconds
#' @slot T temperature in Kelvin
#' @slot H applied field in Oe
#' @slot M magnetization in emu
#' @slot Merr std. dev. of magnetization in emu
#' @slot Temp temperature as factor
#' @slot dir direction of sweeping field, +1 (more positive), -1 (more negative), 0 (not sweeping)
#' @slot loop loop number
#' @slot description description or note
#' @slot sampleName sample name
#' @slot fullFilename name of file
VSMdata<-setClass("VSMdata",
                   slots = c(
                     time = "vector",
                     T = "vector",
                     H = "vector",
                     M = "vector",
                     Merr = "vector",
                     Temp = "vector",
                     dir = "vector",
                     loop = "vector",
                     description="character",
                     sampleName = "character",
                     fullFilename="character"
                   ),
                   validity =
                     function(object) {
                       errors <- character()

                       if (max(T)>1000) {
                         msg <- paste('Measurement temperature too high:',max(T))
                         errors <- c(errors,msg)
                       }
                       if (length(errors) == 0) TRUE else errors
                     }
)

#' Constructor method of VSMdata Class
#'
#' @param .Object VSMdata object
#' @param time time in seconds
#' @param T temperature in Kelvin
#' @param H applied field in Oe
#' @param M magnetization in emu
#' @param Merr std. dev. of magnetization in emu
#' @param Temp temperature as factor
#' @param dir direction of sweeping field, +1 (more positive), -1 (more negative), 0 (not sweeping)
#' @param loop loop number
#' @param description description or note
#' @param sampleName sample name
#' @param fullFilename name of file
#' @export
#' @importFrom methods setMethod initialize new validObject
setMethod(f="initialize",
          signature="VSMdata",
          definition= function(.Object,
                               time,
                               T,
                               H,
                               M,
                               Merr,
                               Temp,
                               dir,
                               loop,
                               description="",
                               sampleName="",
                               fullFilename)
          {
            if (!missing(time)) .Object@time<-time
            if (!missing(T)) .Object@T <- T
            if (!missing(H)) .Object@H <- H
            if (!missing(M)) .Object@M <- M
            if (!missing(Merr)) .Object@Merr <- Merr
            if (!missing(Temp)) .Object@Temp <- Temp else .Object@Temp = factor(signif(T,2))
            if (!missing(dir)) .Object@dir <- dir else .Object@dir = .getSweepDirection(time,H,T)
            .Object@loop = .getLoop(.Object@dir)

            if (!missing(description)) .Object@description <-description
            if (!missing(sampleName)) .Object@sampleName<-sampleName
            if (!missing(fullFilename)) .Object@fullFilename<-fullFilename
            validObject(.Object)
            return(.Object)
          })



#' Initialize the AFMdata object
#'
#' @param time time in seconds
#' @param T temperature in Kelvin
#' @param H applied field in Oe
#' @param M magnetization in emu
#' @param Merr std. dev. of magnetization in emu
#' @param Temp temperature as factor
#' @param dir direction of sweeping field, +1 (more positive), -1 (more negative), 0 (not sweeping)
#' @param loop loop number
#' @param description description or note
#' @param sampleName sample name
#' @param fullFilename name of file
#' @export
VSMdata <- function(time,
                     T,
                     H,
                     M,
                     Merr,
                     Temp,
                     dir,
                     loop=0,
                     description="",
                     sampleName="",
                     fullFilename) {
  return(new("VSMdata",
             time,
             T,
             H,
             M,
             Merr,
             Temp,
             dir,
             loop,
             description,
             sampleName,
             fullFilename))
}

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


#' print VSMdata object
#'
#' @param x VSMdata object
#' @param ... other summary parameters
#' @return summary of VSMdata object
#' @author Thomas Gredig
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vms.import(filename)
#' print(d)
#' @export
print.VSMdata <- function(x,...) {
  cat("Data points:    ",length(x@T),"\n")
  cat("Lowest T:       ",min(x@T),"K\n")
  cat("Highest T:      ",max(x@T),"K\n")
  cat("Total time:     ",max(x@time)/60,"min\n")
  cat("Number of loops:",nlevels(factor(x@loop)),"\n")
  cat("Sample:         ",x@sampleName,"\n")
  cat("Description:    ",x@description,"\n")
}

#' plot VSMdata object
#'
#' @param x VSMdata object
#' @param ... other summary parameters
#' @return summary of VSMdata object
#' @author Thomas Gredig
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' plot(d)
#' @export
plot.VSMdata <- function(x,...) {
  if (length(x@H)==0) { return("VSMdata object has no data to plot.") }
  plot(x@H, x@M*1e6, col='red', xlab='H (Oe)', ylab="M (uemu)")
}


###
NULL

# determine the direction for sweeping the magnetic field
# -1 :: going to lower fields
# +1 :: going to higher fields
# 0 :: constant temperature
.getSweepDirection <- function(time, H, T) {
  n1 = data.frame(time=time, H=H, T=T)
  n1$delta.time = c(0,diff(time))
  n1$delta.H = c(0, diff(H))
  n1$delta.T = c(0, diff(T))
  n1$T.dot = n1$delta.T / n1$delta.time
  n1$H.dot = n1$delta.H / n1$delta.time

  dir = c(0,sign(diff(n1$H)/diff(n1$time)))
  dir[which(abs(n1$H.dot)<10 & abs(n1$T.dot)>0.01)]=0
  dir[1] = dir[2]
  dir
}

# numbers the loops
.getLoop <- function(dir) {
  chg = c(0,diff(dir))
  chg[which(chg==2)]=0
  cumsum(ceiling(abs(chg)/2))+1
}

.getVsmLoop <- function(obj) {
  floor(c(0,cumsum(diff(obj@dir)/2)) / 2) + 1
}
