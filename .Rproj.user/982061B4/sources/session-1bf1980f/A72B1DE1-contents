#' Calculate Correlation-Weighted Composite Score Weights
#' 
#' @description
#' Create composite score weights of scales by specifying the indicators that
#' go into each respective composite variable. For correlation-weighted
#' composite scores, weights are calculated as the average correlation of each
#' indicator with all other indicators.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' 
#' @return A dataframe table with the indicator weights for their respective
#' composite.
#' 
#' @examples
#' 
#' data(grit)
#' 
#' # Specify the named list with composite names and their respective indicators
#' varlist <- list(extraversion = sprintf("e%01d", seq(10)),
#'                 neuroticism = sprintf("n%01d", seq(10)),
#'                 agreeableness = sprintf("a%01d", seq(10)),
#'                 conscientiousness = sprintf("c%01d", seq(10)),
#'                 openness = sprintf("o%01d", seq(10)),
#'                 grit = sprintf("gs%01d", seq(12)))
#' 
#' # Calculate correlation-weighted composite scores
#' correlation_weights <- correlation_weights(data = grit,
#'                                            varlist = varlist)
#' 
#' @export
correlation_weights <- function(data = .,
                                varlist){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  # Create empty list to save composite weights
  composite_weights <- list()
  
  
  for(i in seq_along(varlist)){
    
    
    # -- DATA PREPARATION -- #
    
    # Get dataframe with just indicator vars
    df <- data %>% 
      dplyr::select(dplyr::all_of(varlist[[i]]))
    
    
    # Calculate correlation matrix
    cor_matrix <- stats::cor(df,
                             use = 'complete.obs')
    
    
    
    # -- COMPOSITE WEIGHTS CALCULATION -- #
    
    # Get the correlation weights
    cor_weights <- colMeans(cor_matrix)
    
    # Save weights
    composite_weights[[names(varlist)[i]]] <- data.frame(weights = cor_weights) %>% 
      
      # Convert rownames to indicator column
      tibble::rownames_to_column(var = "indicator") %>% 
      
      # Create column to inform which composite the indicators reflect
      dplyr::mutate(composite = names(varlist)[i]) %>% 
      
      # Normalize weights
      dplyr::mutate(weights = weights / sum(weights)) %>% 
      
      # Reorder
      dplyr::select(composite, indicator, weights)
    
    
  }
  
  
  # Convert the weights list to a dataframe
  weights_table <- purrr::map_df(composite_weights, ~.x)
  
  
  # Return
  return(weights_table)
  
}
