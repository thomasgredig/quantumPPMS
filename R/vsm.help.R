#' VSM Help
#'
#' Prints instructions on how to convert from obsolete functions to
#' new functions
#' @param name deprecated function name
#'
#' @export
vsm.help <- function(name='ppms.load') {
  switch(name,
        'ppms.load' = "for ppms.load(), instead use vsm.import(), which returns a class VSMdata object, for the data frame, you could then call vsm.data.frame()",
        'vsm.hyst.stats' = 'vsm.hyst.stats() gets a list of statistics from a hystersis loop, use vsm.hystStatsLoop() instead; the function vsm.hystStats() returns the stats for all loops',
        'vsm.stats' = "vsm.stats() compiles a list of statistics for each loop with the help of vsm.hyst.stats() -> now called vsm.hystStats()",
        'ppms.removeSubstrateMagnetization' = 'integrated in vsm.data.frame(), for example',
        'ppms.vsm.hystLoops' = "for ppms.vsm.hystLoops(), loops are already included in class, but you can use vsm.data.frame() to convert"
  )

}
