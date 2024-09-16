#' Calculate Cohen's Kappa
#'
#' This function calculates the raw agreement, expected agreement, and Cohen's Kappa for a given category.
#' 
#' @param df A confusion matrix (without a row and column for "total")
#' @param cat A string specifying a category.
#' @return p_o: The raw agreement score
#' @return p_e: The expected agreement score
#' @return kappa: The Cohen's Kappa score
#' @return collapsed:  The collapsed confusion matrix
#' @export
cohens_kappa <- function(df, cat) {
  collapsed <- df %>% 
    pivot_longer(cols = -NMM, names_to = "category", values_to = "value") %>% 
    group_by(if_else(NMM == cat, "agree", "other")) %>% 
    summarize(agree = sum(value[category == cat]), 
              other = sum(value[category != cat]), 
              .groups = "drop") %>%
    rename(NMM = 1)

  matrx <- as.matrix(collapsed[, -1])
  
  p_o <- sum(diag(matrx)) / sum(matrx)
  p_e <- sum(rowSums(matrx) * colSums(matrx)) / sum(matrx) ^ 2
  kappa <- (p_o - p_e) / (1 - p_e)
  
  return(list(p_o = p_o, 
              p_e = p_e, 
              kappa = kappa,
              collapsed = matrx))
}