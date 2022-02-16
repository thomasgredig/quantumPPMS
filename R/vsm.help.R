#' VSM Help
#'
#' Prints instructions on how to convert from obsolete functions to
#' new functions
#' @param name deprecated function name
#'
#' @export
vsm.help <- function(name='ppms.load') {
  switch(name,
             'ppms.load' = "for ppms.load(), instead use vsm.import()",
             'ppms.removeSubstrateMagnetization' = 'integrated in vsm.data.frame(), for example',
             'ppms.vsm.hystLoops' = "for ppms.vsm.hystLoops(), loops are already included in class, but you can use vsm.data.frame() to convert"
  )

}
