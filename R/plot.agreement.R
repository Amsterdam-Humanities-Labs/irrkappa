#' Plot Agreement Rates
#'
#' This function plots the agreement rates from an agreement dataframe.
#' 
#' @param agreement A dataframe with agreement scores
#' @param save.plot A boolean specifiying whether or not the plot should be saved
#' @param frame A boolean specifying if the method is frame-based or not. Should be removed.
#' @return A bar graph of the agreement rates for each annotation value. This is either saved to a folder or shown.
#' @export
plot.agreement <- function(agreement, save.plot = FALSE) {
  # Data for plots
  agreement <- t(agreement)
  colnames(agreement) <- gsub("\\.", "-", colnames(agreement))
  
  if(max(agreement) < 0.92) {
    ylim.upper <- max(agreement) * 1.3
  } else {ylim.upper <- max(agreement) * 1.19}
  ylim.lower <- min(min(agreement) * 1.3, 0)
  
  if(save.plot) {
    fname <- paste("plots/kappa_", tier, "_event_based.jpeg",sep="")
    jpeg(fname, width = 500, height = 550, quality = 100)
    barplot(agreement , beside=T, legend.text = T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement", xlab = "Category")
    dev.off()
  } else {
    barplot(agreement , beside=T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement", xlab = "Category", 
            cex.names = 0.8, cex.axis = 0.8)
    legend("topleft", legend = c("Raw", "Chance", "Kappa"),
           col=c("tomato4", "steelblue" ,"grey90"),
           bty = "n", pch=20 , pt.cex = 2, cex = 0.75, 
           horiz = TRUE, inset = c(0.05, -0.02))
  }
}