#' Calculate overlap between annotations
#' 
#' This function calculates the overlap between annotations for each question. Overlap is calculated as the percentage of overlapping frames relative to the annotation with the highest number of frames.  
#' 
#' @param df The pre-processed dataframe.
#' @return overlapping: A dataframe denoting the percentage overlap of between annotations.
#' @export
overlap <- function(df) {
  overlapping <- df %>% 
    inner_join(df, by = "Question", suffix = c(".x", ".y")) %>% 
    select(Question, ID.x, ID.y, NMM.x, NMM.y, Annotator.x, Begin.Frame.x, End.Frame.x, Frames.x, Annotator.y, Begin.Frame.y, End.Frame.y, Frames.y) %>%
    mutate(end = pmin(End.Frame.x, End.Frame.y),
           begin = pmax(Begin.Frame.x, Begin.Frame.y),
           Overlap = round((end - begin) / pmax(Frames.x, Frames.y) * 100, 0),
           Overlap.x = round((end - begin) / Frames.x * 100, 0),
           Overlap.y = round((end - begin) / Frames.y * 100, 0),
           Overlap.frame = round((end - begin), 0), .before = Annotator.x) %>%
    filter(Annotator.x != Annotator.y,
           End.Frame.x > Begin.Frame.y,
           End.Frame.y > Begin.Frame.x,
           Overlap > 0) %>%
    select(-c(end, begin))
  
  return(overlapping)
}