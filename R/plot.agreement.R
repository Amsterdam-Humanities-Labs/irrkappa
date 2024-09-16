#' Plot Cohen's kappa Agreement Rates
#'
#' This function plots the agreement rates from an agreement dataframe.
#' 
#' @param agreement A dataframe with agreement scores
#' @param save.plot A boolean specifiying whether or not the plot should be saved
#' @param frame A boolean specifying if the method is frame-based or not. Should be removed.
#' @return A bar graph of the agreement rates for each annotation value. This is either saved to a folder or shown.
#' @export
plot.agreement_kappa <- function(agreement, save.plot = FALSE) {
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


#' Plot Krippendorff's alpha Agreement Rates
#'
#' This function plots the agreement rates from an agreement dataframe.
#' 
#' @param agreement A dataframe with agreement scores
#' @param save.plot A boolean specifiying whether or not the plot should be saved
#' @param frame A boolean specifying if the method is frame-based or not. Should be removed.
#' @return A bar graph of the agreement rates for each annotation value. This is either saved to a folder or shown.
#' @export
plot.agreement_alpha_by_category <- function(agreement, save.plot = FALSE) {
  # Data for plots
  agreement <- t(agreement)
  colnames(agreement) <- gsub("\\.", "-", colnames(agreement))
  
  if(max(agreement) < 0.92) {
    ylim.upper <- max(agreement) * 1.3
  } else {ylim.upper <- max(agreement) * 1.19}
  ylim.lower <- min(min(agreement) * 1.3, 0)
  
  if(save.plot) {
    fname <- paste("plots/alpha_", tier, "_event_based.jpeg",sep="")
    jpeg(fname, width = 500, height = 550, quality = 100)
    barplot(agreement , beside=T, legend.text = T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement", xlab = "Category")
    dev.off()
  } else {
    barplot(agreement , beside=T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement", xlab = "Category", 
            cex.names = 0.8, cex.axis = 0.8)
    legend("topleft", legend = c("Do", "De", "Alpha"),
           col=c("tomato4", "steelblue" ,"grey90"),
           bty = "n", pch=20 , pt.cex = 2, cex = 0.75, 
           horiz = TRUE, inset = c(0.05, -0.02))
  }
}

#' Plot Krippendorff's alpha Agreement Rates
#'
#' This function plots the agreement rates from an agreement dataframe.
#' 
#' @param agreement A dataframe with agreement scores
#' @param save.plot A boolean specifiying whether or not the plot should be saved
#' @param frame A boolean specifying if the method is frame-based or not. Should be removed.
#' @return A bar graph of the agreement rates for each annotation value. This is either saved to a folder or shown.
#' @export
plot.agreement_alpha <- function(agreement, save.plot = FALSE) {
  # Data for plots
  agreement <- as.numeric(agreement[1, c("Do", "De", "Alpha")])
  
  if(max(agreement) < 0.92) {
    ylim.upper <- max(agreement) * 1.3
  } 
  else {
    ylim.upper <- max(agreement) * 1.19
  }
  ylim.lower <- min(min(agreement) * 1.3, 0)
  
  if(save.plot) {
    fname <- paste("plots/alpha_weighted_", tier, "_event_based.jpeg",sep="")
    jpeg(fname, width = 500, height = 550, quality = 100)
    barplot(agreement , beside=T, legend.text = T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement")
    dev.off()
  } else {
    barplot(agreement, beside=T, col=c("tomato4", "steelblue" ,"grey90"), 
            ylim = c(ylim.lower, ylim.upper), ylab="Agreement", 
            cex.names = 0.8, cex.axis = 0.8)
    legend("topleft", legend = c("Do", "De", "Alpha"),
           col=c("tomato4", "steelblue" ,"grey90"),
           bty = "n", pch=20 , pt.cex = 2, cex = 0.75, 
           horiz = TRUE, inset = c(0.05, -0.02))
  }
}