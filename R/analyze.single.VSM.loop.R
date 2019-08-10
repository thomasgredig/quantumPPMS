library(plyr)
library(ggplot2)
#' analyzes a single hysteresis loop and return two graphs
#' along with essential data
#' @param data hyst data frame
#' @return list
#' @examples
#' filename = system.file("extdata", "20170620_BITHERMAL_SF_VSM_SF170517SI2_MVSH_3K.DAT", package="quantumPPMS")
#' d = ppms.load(filename)
#' d2 = vsm.get.HystLoops(d)
#' data = subset(d2, loop == 1)
#' m = analyze.single.VSM.loop(data)
#' @export
analyze.single.VSM.loop <- function(data) {
  T.mn = signif(mean(data$T),3)
  T.sd = signif(sd(data$T),3)
  exp.uemu = expression(paste('M (10'^-6,' emu)'))
  m1 = ggplot(data, aes(H/1E4,M*1E6)) +
    geom_point() +
    xlab('H (T)') +
    ylab(exp.uemu) +
    ggtitle(paste("RAW:",fname)) +
    annotate("text", x = 0.9*max(data$H)/1E4, y = 0.9*max(data$M)*1E6,
             label = paste('T=',T.mn,'+/-',T.sd,'K'), hjust = 1) +
    theme_bw(base_size = 14)

  q = vsm.hyst.stats(subset(data,dir == -1))
  unlist(q) -> q2
  q = vsm.hyst.stats(subset(data,dir == 1))
  unlist(q) -> q3


  data$Mcorr = data$M - data$H*q2['Susceptibility']
  slope = signif(q2['Susceptibility'],3)
  #plot(data$H, data$Mcorr)

  m2 = ggplot(data, aes(H/1E4, Mcorr*1E6)) +
    geom_point() +  xlab('H (T)') +
    ylab(exp.uemu) +
    geom_vline(xintercept = q2['Hc']/1E4, col='red') +
    geom_vline(xintercept = q3['Hc']/1E4, col='red') +
    geom_hline(yintercept = q2['Ms1']*1E6, col='blue') +
    geom_hline(yintercept = q2['Ms2']*1E6, col='blue') +
    ggtitle(paste("Bgd removed:",fname)) +
    annotate("text", x = min(data$H)/1E4, y = 0.9*max(data$Mcorr)*1E6,
             label = paste('T=',T.mn,'+/-',T.sd,'K'), hjust = 0) +
    annotate("text", x = min(data$H)/1E4, y = 0,
             label = paste('chi=',slope,'emu/Oe'), hjust = 0) +
    theme_bw(base_size = 14)

  list(m1,
       m2,
       T.mn,
       T.sd,
       (q3['Hc']-q2['Hc'])/2,
       q2['Ms1'],
       q2['Ms2'])
}
