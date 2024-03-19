#' Filter dataframe and drop levels
#' 
#' This function filters a dataframe on a given condition, and drops the levels in factor columns.
#' 
#' @param df A dataframe
#' @param condition A string specifying a condition.
#' @return df.filtered:  The filtered dataframe.
#' @export
filter.drop.levels <- function(df, condition) {
  df.filtered <- df %>%
    filter(condition) %>%
    droplevels()
  
  return(df.filtered)
}