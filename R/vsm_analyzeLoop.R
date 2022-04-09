#' Analyzes a single hysteresis loop
#'
#' return two graphs along with essential data
#' @param obj VSMdata object
#' @param loop number of the hysteresis loop to display
#' @return list
#' @examples
#' filename = vsm.getSampleFiles()[1]
#' d = vsm.import(filename)
#' vsm.analyzeLoop(d)
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot geom_point xlab ylab ggtitle annotate theme_bw geom_vline aes geom_hline theme
#' @export
vsm.analyzeLoop <- function(obj, loop = 1) {
  T.mn = signif(mean(obj@T),3)
  T.sd = signif(sd(obj@T),3)
  exp.uemu = expression(paste('M (emu)'))
  fname = basename(obj@fullFilename)

  data = data.frame(
    H = obj@H,
    M = obj@M,
    dir = obj@dir
  )
  data$l = .getVsmLoop(obj)
  data = subset(data, l == loop)

  m1 = ggplot(data, aes(H,M,col=factor(dir))) +
    geom_point() +
    xlab('H (Oe)') +
    ylab(exp.uemu) +
    ggtitle(paste("RAW:",fname)) +
    annotate("text", x = 0.95*max(data$H), y = 0.95*max(data$M),
             label = paste('T=',T.mn,'+/-',signif(T.sd,1),'K'), hjust = 1) +
    theme_bw(base_size = 14) +
    theme(legend.position='none')

  q2 = vsm.hystStatsLoop(obj, loop = loop, direction = -1)
  #unlist(q) -> q2
  q3 = vsm.hystStatsLoop(obj, loop = loop, direction = 1)
  #unlist(q) -> q3


  data$Mcorr = data$M - data$H*q2$Susceptibility
  slope = signif(q2$Susceptibility,3)
  #plot(data$H, data$Mcorr)

  m2 = ggplot(data, aes(H, Mcorr)) +
    geom_point() +  xlab('H (Oe)') +
    ylab(exp.uemu) +
    geom_vline(xintercept = q2$Hc, col='red') +
    geom_vline(xintercept = q3$Hc, col='red') +
    geom_hline(yintercept = q2$Ms1, col='blue') +
    geom_hline(yintercept = q2$Ms2, col='blue') +
    ggtitle(paste("Bgd removed:",fname)) +
    annotate("text", x = min(data$H), y = 0.9*max(data$Mcorr),
             label = paste('T=',T.mn,'+/-',T.sd,'K'), hjust = 0) +
    annotate("text", x = min(data$H), y = 0,
             label = paste('chi=',slope,'emu/Oe'), hjust = 0) +
    theme_bw(base_size = 14)

  list(graph.MvsH= m1,
       graph.McorrvsH = m2,
       T.mean = T.mn,
       T.sd = T.sd,
       Hc = (q3$Hc-q2$Hc)/2,
       Ms1 = q2$Ms1,
       Ms2 = q2$Ms2)
}
