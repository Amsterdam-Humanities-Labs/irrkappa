#' Calculate Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#' 
#' @param df A confusion matrix, without a row/column with total scores
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @return agreement: A dataframe with scores for raw agreement, chance agreement, and Cohen's kappa for each annotation value.
#' @export
calculate_agreement <- function(df, nmm_values) {
  nmm_values <- droplevels(nmm_values)
  results <- list()
  
  for (value in nmm_values) {
    result <- cohens_kappa(df = df, cat = value)
    results[[value]] <- result
  }
  
  agreement <- data.frame(Raw = sapply(results, function(x) round(x$p_o, 2)),
                          Chance = sapply(results, function(x) round(x$p_e, 2)),
                          Kappa = sapply(results, function(x) round(x$kappa, 2)))
  
  return(agreement)
}