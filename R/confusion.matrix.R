#' Create Event-based Confusion Matrix
#'
#' This function creates the event-based confusion matrix. The rows in the matrix represent Annotator.a, and the columns represent Annotator.b.
#' 
#' @param df Dataframe as processed and prepared by the match function.
#' @return confusion: The confusion matrix for the event-based approach
#' @return confusion.total: The confusion matrix for the event-based approach, with a 'total' row and column.
#' @export
confusion.matrix <- function(df) {
  categories <- unique(c(levels(df$NMM.x), levels(df$NMM.y)))
  combinations <- crossing(NMM.x = categories, NMM.y = categories)
  
  anno.mat <- df %>%
    count(NMM.x, NMM.y) 
  
  confusion <- merge(combinations, anno.mat, by = c("NMM.x", "NMM.y"), all.x = TRUE) %>%
    replace_na(list(n = 0)) %>%
    pivot_wider(names_from = NMM.y, values_from = n, values_fill = 0, names_sort = TRUE) %>%
    rename(NMM = NMM.x) 
  
  # Move these columns to the back/bottom columns and rows
  for (to_move in c("other", "unmatched")) {
    if (to_move %in% colnames(confusion)) {
      confusion <- confusion %>%
        arrange(if_else(NMM == to_move, 1, 0)) %>%
        select(-to_move, everything(), to_move)
    }
  }
  
  confusion.total <- confusion %>% 
    bind_rows(summarise(., across(where(is.numeric), sum), across(where(is.character), ~'total'))) %>%
    rowwise() %>%
    mutate(total = sum(c_across(where(is.numeric))),
           NMM = as.factor(NMM)) %>%
    ungroup()
  
  return(list(confusion = confusion, 
              confusion.total = confusion.total))
}