#' Calculate Krippendoff's Alpha
#'
#' This function calculates the weight matrix to be used for calculation of alpha
#' By default all weights are 0
#' 
#' @param from A list of categories
#' @param to Another list of categories, should form pairs of categories with from
#' @param weights the weights between the pairs of categories
#' @return weight_matrix: the weight matrix
#' @export
create_weight_matrix <- function (from, to, weights, categories){
  # Define the weights between variable pairs and create empty weight matrix
  weights <- data.frame(var1 = from, var2 = to, weight = weights)
  weight_matrix <- matrix(0, nrow = length(categories), ncol = length(categories))
  rownames(weight_matrix) <- categories
  colnames(weight_matrix) <- categories
  
  # Populate the matrix with the weights
  for (i in 1:nrow(weights)) {
    var1 <- weights$var1[i]
    var2 <- weights$var2[i]
    weight <- weights$weight[i]
    weight_matrix[var1, var2] <- weight
    weight_matrix[var2, var1] <- weight  # Assuming the weight matrix is symmetric
  }
  
  return(weight_matrix)
}

#' Calculate Krippendoff's Alpha
#'
#' This function calculates the weight matrix to be used for calculation of alpha
#' By default all weights are 0
#' 
#' @param from A list of categories
#' @param to Another list of categories, should form pairs of categories with from
#' @param weights the weights between the pairs of categories
#' @return weight_matrix: the weight matrix
#' @export
create_equal_weight_matrix <- function (categories){
  # Define the weights between variable pairs and create empty weight matrix
  weight_matrix <- matrix(0, nrow = length(categories), ncol = length(categories))
  rownames(weight_matrix) <- categories
  colnames(weight_matrix) <- categories
  
  # Populate the matrix with the weights
  for (i in 1:nrow(weight_matrix)) {
    for (j in 1:ncol(weight_matrix)) {
      if(i!=j){
        weight_matrix[i, j] <- 1
        weight_matrix[j, i] <- 1
      }
    }
  }
  
  return(weight_matrix)
}

#' Calculate Krippendoff's Alpha
#'
#' This function calculates the raw agreement, expected agreement, and Krippendorff's Alpha.
#' 
#' @param df A confusion matrix (without a row and column for "total")
#' @param cat A string specifying a category.
#' @param wm A weight matrix that gives weights between annotation values in range 0 to 1
#' @return Do: The observed disagreement score
#' @return De: The expected disagreement score
#' @return alpha: Krippendorff's alpha score
#' @return collapsed: The collapsed confusion matrix
#' @export
krippendorffs_alpha <- function(df, categories, wm) {
  weight_matrix <- wm
  
  # Remove rows with less than 2 annotations from the dataset
  df_only_nmm <- df[ , grepl( "NMM." , names( df ) ) ]
  has_more_than_two_non_empty <- function(row) {
    sum(row != "") >= 2
  }
  df <- df_only_nmm[apply(df_only_nmm, 1, has_more_than_two_non_empty), ]
  rownames(df) = NULL
  
  # Make the agreement table
  df_only_nmm <- df[ , grepl( "NMM." , names( df ) ) ]
  agreement_table <- data.frame(matrix(ncol = length(categories), nrow = nrow(df_only_nmm)))
  colnames(agreement_table) <- categories
  
  for (i in 1:nrow(df_only_nmm)){
    row_vector <- unlist(df_only_nmm[i, ])
    item_counts <- table(row_vector)
    for (cat_name in categories){
      agreement_table[i, cat_name] <- ifelse(cat_name %in% names(item_counts), item_counts[cat_name], 0)
    }
  }
  
  # Count the number of items
  n_items <- nrow(agreement_table)
  n_categories <- length(categories)
  n_coders <- sum(unlist(agreement_table[1, ]))
  
  # Calculate observed disagreement
  a <- 0
  b <- 0
  for (item in 1:n_items){
    for (cat_1 in categories){
      for (cat_2 in categories){
        a <- a + (agreement_table[item, cat_1] * agreement_table[item, cat_2] * weight_matrix[cat_1, cat_2])
      }
    }
  }
  b <- (n_items*n_coders*(n_coders - 1))
  Do <- a / b
  
  # Calculate expected disagreement
  a <- 0
  b <- 0
  for (cat_1 in categories){
    for (cat_2 in categories){
      a <- a + (sum(agreement_table[, cat_1]) * sum(agreement_table[, cat_2]) * weight_matrix[cat_1, cat_2])
    }
  }
  b <- (n_items*n_coders*(n_items*n_coders - 1))
  De <- a / b
  
  # Calculate krippendorff's alpha
  alpha <- 1 - (Do / De)
  
  return(list(Do = Do, De = De, alpha = alpha))
}


#' Calculate Krippendoff's Alpha
#'
#' This function calculates the raw agreement, expected agreement, and Krippendorff's Alpha for a given category.
#' 
#' @param df A confusion matrix (without a row and column for "total")
#' @param cat A string specifying a category.
#' @param wm A weight matrix that gives weights between annotation values in range 0 to 1
#' @return Do: The observed disagreement score
#' @return De: The expected disagreement score
#' @return alpha: Krippendorff's alpha score
#' @return collapsed: The collapsed confusion matrix
#' @export
krippendorffs_alpha_by_category <- function(df, categories, cat, wm) {
  weight_matrix <- wm
  
  # Remove rows with less than 2 annotations from the dataset
  df_only_nmm <- df[ , grepl( "NMM." , names( df ) ) ]
  has_more_than_two_non_empty <- function(row) {
    sum(row != "") >= 2
  }
  df <- df_only_nmm[apply(df_only_nmm, 1, has_more_than_two_non_empty), ]
  rownames(df) = NULL
  
  # Make the agreement table
  df_only_nmm <- df[ , grepl( "NMM." , names( df ) ) ]
  agreement_table <- data.frame(matrix(ncol = length(categories), nrow = nrow(df_only_nmm)))
  colnames(agreement_table) <- categories
  
  for (i in 1:nrow(df_only_nmm)){
    row_vector <- unlist(df_only_nmm[i, ])
    item_counts <- table(row_vector)
    for (cat_name in categories){
      agreement_table[i, cat_name] <- ifelse(cat_name %in% names(item_counts), item_counts[cat_name], 0)
    }
  }
  
  # Count the number of items
  n_items <- nrow(agreement_table)
  n_categories <- length(categories)
  n_coders <- sum(unlist(agreement_table[1, ]))
  
  # Calculate observed disagreement
  a <- 0
  b <- 0
  for (item in 1:n_items){
    for (cat_1 in categories){
      for (cat_2 in categories){
        if(cat_1 == cat | cat_2 == cat){
          a <- a + (agreement_table[item, cat_1] * agreement_table[item, cat_2] * weight_matrix[cat_1, cat_2])
        }
      }
    }
  }
  b <- (n_items*n_coders*(n_coders - 1))
  Do <- a / b
  
  # Calculate expected disagreement
  a <- 0
  b <- 0
  for (cat_1 in categories){
    for (cat_2 in categories){
      if(cat_1 == cat | cat_2 == cat){
        a <- a + (sum(agreement_table[, cat_1]) * sum(agreement_table[, cat_2]) * weight_matrix[cat_1, cat_2])
      }
    }
  }
  b <- (n_items*n_coders*(n_items*n_coders - 1))
  De <- a / b
  
  # Calculate krippendorff's alpha
  alpha <- 1 - (Do / De)
  
  return(list(Do = Do, De = De, alpha = alpha))
}
