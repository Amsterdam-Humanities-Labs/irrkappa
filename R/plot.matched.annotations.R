#' Plot Annotations
#'
#' This function plots annotations for a given question.
#' 
#' @param df The pre-processed dataframe
#' @param question A string of a question ID
#' @param threshold A boolean indicating whether matches below the overlap threshold should be dropped.
#' @return p: A plot of the annotations by both annotators for the question. The plot includes a line between overlapping annotations and indicates the percentage overlap between them.
#' @export
plot.matched.annotations <- function(df, question = "01-NeutNeg-01", threshold = FALSE) {
  df <- suppressWarnings(overlap(df))
  if(threshold) {
    df <- filter.drop.levels(df, df$Overlap >= Overlap.threshold)
  }
  
  df.question <- df %>% filter(Question == question)
  
  bg_colors <- c("#C5CAE9", "#B2DFDB", "#FFE0B2", "#F8BBD0", "#F0F4C3", "#BBDEFB")
  
  p <- ggplot(df.question, 
              aes(xmin = Begin.Frame.x, 
                  xmax = End.Frame.x, 
                  ymin = as.numeric(Annotator.x)-0.4, 
                  ymax = as.numeric(Annotator.x)+0.4, 
                  fill = NMM.x)) + 
    geom_rect(alpha = 1, color = "black", size = 0.2) +
    geom_segment(data = df.question, 
                 aes(x = (Begin.Frame.x + Frames.x/2), 
                     y = as.numeric(Annotator.x), 
                     xend = (Begin.Frame.y + Frames.y/2), 
                     yend = as.numeric(Annotator.y)), 
                 color = "red", linewidth = 0.5) +
    geom_text(data = df.question, 
              aes(x = ((Begin.Frame.x + Begin.Frame.y + Frames.x/2 + Frames.y/2)/2)+6 , 
                  y = (as.numeric(Annotator.x) + as.numeric(Annotator.y))/2, 
                  label = paste0(round(Overlap, 1), "%")), 
              color = "red", 
              size = 2.8, 
              fontface = "bold") +
    labs(x = "Time (frames)", y = "Annotator") +
    ggtitle(paste(tier, "Tier, Question", question)) +
    scale_fill_manual(values = bg_colors) +  
    theme_bw() +
    theme(panel.spacing = unit(0, "lines"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_y_continuous(limits = c(0.55, 2.45), 
                       breaks = c(1, 2), 
                       labels=c(Annotator.a, Annotator.b))
  
  return(p)
}