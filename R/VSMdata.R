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
#'
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
#' @slot type "MvsH" or "MvsT"
#' @slot Mcorr magnetization with substrate susceptibility removed for "MvsH" type
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
                    type = "vector",
                    Mcorr = "vector",
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
#' @param type "MvsH" or "MvsT"
#' @param Mcorr magnetization with substrate susceptibility removed for "MvsH" type
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
            if (!missing(T)) .Object@T <- as.numeric(T)
            if (!missing(H)) .Object@H <- as.numeric(H)
            if (!missing(M)) .Object@M <- as.numeric(M)
            if (!missing(Merr)) .Object@Merr <- Merr
            if (!missing(Temp)) .Object@Temp <- Temp else .Object@Temp = factor(signif(T,2))
            if (!missing(dir)) .Object@dir <- dir else .Object@dir = .getSweepDirection(time,H,T)
            .Object@loop = .getLoop(.Object@dir)
            .Object@type = .getType(.Object@T, .Object@H, .Object@loop)
            .Object@Mcorr = .getMcorr(.Object@H, .Object@M, .Object@type, .Object@loop)

            if (!missing(description)) .Object@description <-description
            if (!missing(sampleName)) .Object@sampleName<-sampleName
            if (!missing(fullFilename)) .Object@fullFilename<-fullFilename
            validObject(.Object)
            return(.Object)
          })



########
NULL


.getVsmLoop <- function(obj) {
  floor(c(0,cumsum(diff(obj@dir)/2)) / 2) + 1
}


# numbers the loops
.getLoop <- function(dir) {
  chg = c(0,diff(dir))
  chg[which(chg==2)]=0
  cumsum(ceiling(abs(chg)/2))+1
}

# returns the number of MvsH hysteresis loops in VSMdata object
.getNumberMvsHLoops <- function(obj) {
  loops = levels(factor(obj@loop))
  cnt=0
  for(l in loops) {
    x <- vsm.getLoop(obj, l)
    if ((length(x@time)>0) & (x@type[1]=='MvsH')) cnt=cnt+1
  }
  cnt
}


# assumes loops are sequentially ordered
.getType <- function(T,H,loop) {
  d = data.frame(T,H,loop)
  ty = c()
  for(l in levels(factor(loop))) {
    d1 = subset(d,loop==l)
    y = "Mvstime"
    if (nrow(d1) >= 3) {
      Tchg = mean(abs(diff(d1$T)))
      Hchg = mean(abs(diff(d1$H)))

      if (Hchg > 0.1) y = "MvsH"
      if (Hchg < 0.01 & Tchg>0.01) y = "MvsT"
    }
    ty = c(ty,rep(y, nrow(d1)))
  }

  ty
}


###
# NULL
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



# assumes loops are sequentially ordered
.getMcorr <- function(H,M,type,loop) {
  d = data.frame(H,M,type,loop)
  ty = c()
  for(l in levels(factor(loop))) {
    d1 = subset(d,loop==l)
    if ((d1$type[1]=='MvsH') & (nrow(d1)>10)) {
      # try to correct slope
      d2 = subset(d1, H<0.98*max(H) & H>0.85*max(H))
      if (nrow(d2) > 3) {
        lm(data = d2, M ~ H) -> fit
        slope=coef(fit)[2]
        y = d1$M - slope*d1$H
      } else {
        # could be one data point is way off, so that max(H) is not relevant
        sort(H)[length(H)-3] -> maxH
        d2 = subset(d1, H<0.98*maxH & H>0.85*maxH)
        if (nrow(d2)>3) {
          lm(data = d2, M ~ H) -> fit
          slope=coef(fit)[2]
          y = d1$M - slope*d1$H
        } else {
          y = d1$M
        }
      }
    } else {
      y = d1$M
    }
    ty = c(ty,y)
  }

  ty
}



