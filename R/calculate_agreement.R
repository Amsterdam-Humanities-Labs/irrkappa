#' Calculate Cohen's Kappa Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#'
#' @param df A confusion matrix, without a row/column with total scores
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @return agreement: A dataframe with scores for raw agreement, chance agreement, and Cohen's kappa for each annotation value.
#' @export
calculate_agreement_kappa <- function(df, nmm_values) {
  nmm_values <- as.factor(nmm_values)
  nmm_values <- droplevels(nmm_values)
  results_kappa <- list()

  for (value in nmm_values) {
    result_kappa <- cohens_kappa(df = df, cat = value)
    results_kappa[[value]] <- result_kappa
  }

  agreement <- data.frame(Raw = sapply(results_kappa, function(x) round(x$p_o, 2)),
                          Chance = sapply(results_kappa, function(x) round(x$p_e, 2)),
                          Kappa = sapply(results_kappa, function(x) round(x$kappa, 2)))

  return(agreement)
}

#' Calculate Krippendorff's Alpha Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#' The annotation value pairs have equal weights.
#'
#' @param df A confusion matrix, without a row/column with total scores
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @return agreement: A dataframe with scores for observed disagreement, expected disagreement, and Krippendorff's alpha for each annotation value.
#' @export
calculate_krippendorff_alpha_by_category <- function(df, nmm_values) {
  nmm_values <- as.factor(nmm_values)
  nmm_values <- droplevels(nmm_values)
  wm <- create_equal_weight_matrix(nmm_values)
  results_alpha <- list()
  
  for (value in nmm_values) {
    if (value != "unmatched"){
      result_alpha <- krippendorffs_alpha_by_category(df = df, categories=nmm_values, cat = value, wm = wm)
      results_alpha[[value]] <- result_alpha
    }
  }

  agreement <- data.frame(Do = sapply(results_alpha, function(x) round(x$Do, 2)),
                          De = sapply(results_alpha, function(x) round(x$De, 2)),
                          Alpha = sapply(results_alpha, function(x) round(x$alpha, 2)))

  return(agreement)
}

#' Calculate Weighted Krippendorff's Alpha Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#' The annotation value pairs have custom weights.
#'
#' @param df A confusion matrix, without a row/column with total scores.
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @param wm A matrix of weights between annotation values.
#' @return agreement: A dataframe with scores for observed disagreement, expected disagreement, and Krippendorff's alpha for each annotation value.
#' @export
calculate_krippendorff_alpha_by_category_weighted <- function(df, nmm_values, wm) {
  nmm_values <- as.factor(nmm_values)
  nmm_values <- droplevels(nmm_values)
  results_alpha <- list()
  
  for (value in nmm_values) {
    if (value != "unmatched"){
      result_alpha <- krippendorffs_alpha_by_category(df = df, categories=nmm_values, cat = value, wm = wm)
      results_alpha[[value]] <- result_alpha
    }
  }
  
  agreement <- data.frame(Do = sapply(results_alpha, function(x) round(x$Do, 2)),
                          De = sapply(results_alpha, function(x) round(x$De, 2)),
                          Alpha = sapply(results_alpha, function(x) round(x$alpha, 2)))
  
  return(agreement)
}


#' Calculate Krippendorff's Alpha Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#' The annotation value pairs have equal weights.
#'
#' @param df A confusion matrix, without a row/column with total scores
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @return agreement: A dataframe with scores for observed disagreement, expected disagreement, and Krippendorff's alpha for each annotation value.
#' @export
calculate_krippendorff_alpha <- function(df, nmm_values) {
  nmm_values <- as.factor(nmm_values)
  nmm_values <- droplevels(nmm_values)
  wm <- create_equal_weight_matrix(nmm_values)
  results_alpha <- krippendorffs_alpha(df = df, cat = nmm_values, wm = wm)
  
  agreement <- data.frame(Do = round(results_alpha$Do, 2),
                          De = round(results_alpha$De, 2),
                          Alpha = round(results_alpha$alpha, 2))
  
  return(agreement)
}

#' Calculate Weighted Krippendorff's Alpha Agreement Rates for Annotation Values
#'
#' This function calculates the agreement rates for the specified annotation values.
#' The annotation value pairs have custom weights.
#'
#' @param df A confusion matrix, without a row/column with total scores
#' @param nmm_values A list of the annotation values for which agreement rates should be calculated.
#' @param wm A matrix of weights between annotation values.
#' @return agreement: A dataframe with scores for observed disagreement, expected disagreement, and Krippendorff's alpha for each annotation value.
#' @export
calculate_krippendorff_alpha_weighted <- function(df, nmm_values, wm) {
  nmm_values <- as.factor(nmm_values)
  nmm_values <- droplevels(nmm_values)
  results_alpha <- krippendorffs_alpha(df = df, cat = nmm_values, wm = wm)
  
  agreement <- data.frame(Do = round(results_alpha$Do, 2),
                          De = round(results_alpha$De, 2),
                          Alpha = round(results_alpha$alpha, 2))
  
  return(agreement)
}

