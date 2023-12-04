#' Calculate Correlation-Weighted Composite Scores
#' 
#' @description
#' Create composite scores of scales by specifying the indicators that go into
#' each respective composite variable. Correlation-weighted composite scores
#' are calculated as the weighted mean of the indicators. Each indicator's
#' correlation weight is calculated as its average correlation with all
#' other indicators. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' 
#' @return A dataframe identical to the input dataframe, with additional columns 
#' appended at the end. These new columns represent the calculated composite 
#' scores.
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
#' correlation_score <- correlation_score(data = grit,
#'                                        varlist = varlist)
#' 
#' @export
correlation_score <- function(data = .,
                              varlist){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  
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

  
    
    # -- COMPOSITE CALCULATION -- #
    
    # Calculate unweighted correlation composite score
    data[[names(varlist)[i]]] <- rowMeans(sweep(df,
                                                2,
                                                cor_weights,
                                                "*"),
                                          na.rm = T)
    

    
  }
  

  # Return
  return(data)
  
}
