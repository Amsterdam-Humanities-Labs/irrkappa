#' Prepare Dataframe for Event-based IRR Calculations
#'
#' This function prepares the dataframe for event-based IRR calculations. In this case, we remove the "neutral" annotations. Any annotations that do not have at least the specified overlap threshold are labeled as "unmatched", and are commission/omission errors.
#'
#' @param df The pre-processed dataframe.
#' @return df.event.matches: The dataframe that will be used to create the confusion matrix.
#' @export
matches <- function(df) {
  df.overlapping <- suppressWarnings(overlap(df))
  df.overlapping.filtered <- filter.drop.levels(df.overlapping, df.overlapping$Overlap >= Overlap.threshold)

  df.matches <- df.overlapping.filtered %>%
    right_join(df, by = c("Question" = "Question", "ID.x" = "ID", "Annotator.x" = "Annotator", "NMM.x" = "NMM")) %>%
    arrange(ID.x) %>%
    # Fill in unmatched annotations
    mutate(NMM.y = if_else(is.na(NMM.y), "unmatched", NMM.y),
           Annotator.y = if_else(is.na(Annotator.y), if_else(Annotator.x == Annotator.a, Annotator.b, Annotator.a), Annotator.y),
           ID.y = as.integer(if_else(is.na(ID.y), 0, ID.y))) %>%
    # Swap values in ID, NMM, and Annotator columns if Annotator.x is Annotator.b
    mutate(swap = if_else(Annotator.x == Annotator.b, TRUE, FALSE),
           ID.xx = ID.x,
           ID.yy = ID.y,
           ID.x = if_else(swap, ID.yy, ID.x),
           ID.y = if_else(swap, ID.xx, ID.y),
           NMM.xx = NMM.x,
           NMM.yy = NMM.y,
           NMM.x = if_else(swap, NMM.yy, NMM.x),
           NMM.y = if_else(swap, NMM.xx, NMM.y),
           Annotator.x = if_else(swap, Annotator.a, Annotator.x),
           Annotator.y = if_else(swap, Annotator.b, Annotator.y)) %>%
    mutate_if(is.character, as.factor) %>%
    select(Question, ID.x, ID.y, NMM.x, NMM.y, Annotator.x, Annotator.y) %>%
    distinct()

  return(df.matches)
}
