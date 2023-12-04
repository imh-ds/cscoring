#' Calculate Correlation-Weighted Composite Score Data & Metrics
#' 
#' @description
#' Create correlation-weighted composite scores of scales by specifying
#' the indicators that go into each respective composite variable. The function
#' will also return the metrics (i.e., weights & loadings) for the composite
#' variables.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' 
#' @return A list of 1. a dataframe identical to the input dataframe, with
#' additional columns appended at the end representing the calculated composite
#' scores and 2. a dataframe table with the indicator weights and loadings for
#' their respective composite.
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
#' # Calculate correlation-weighted composite scores & metrics
#' correlation_summary <- correlation_score_summary(data = grit,
#'                                                  varlist = varlist)
#' 
#' @export
correlation_score_summary <- function(data = .,
                                      varlist){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  # Create empty list to save composite weights
  composite_weights <- list()
  
  # Create empty list to score composite loadings
  composite_loadings <- list()
  
  
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
    
    
    
    # -- COMPOSITE CALCULATION -- #
    
    # Calculate unweighted correlation composite score
    data[[names(varlist)[i]]] <- rowMeans(sweep(df,
                                                2,
                                                cor_weights,
                                                "*"),
                                          na.rm = T)
    
    
    
    # -- LOADINGS CALCULATION -- #
    
    # Get loadings as the correlation between indicator and composite
    df_loadings <- data %>% 
      dplyr::select(names(varlist)[i],
                    dplyr::all_of(varlist[[i]]))
    
    loadings <- cor(df_loadings,
                    use = "complete.obs")[,1][-1]
    
    # Get loadings
    composite_loadings[[names(varlist)[i]]] <- data.frame(loadings = loadings) %>% 
      
      # Convert rownames to indicator column
      tibble::rownames_to_column(var = "indicator") %>% 
      
      # Create column to inform which composite the indicators reflect
      dplyr::mutate(composite = names(varlist)[i]) %>% 
      
      # Reorder
      dplyr::select(composite, indicator, loadings)
    
  }
  
  
  # Convert the weights list to a dataframe
  weights_table <- purrr::map_df(composite_weights, ~.x)
  
  # Convert the loadings list to a dataframe
  loadings_table <- purrr::map_df(composite_loadings, ~.x)
  
  # Merge loadings and weights together
  metrics_table <- dplyr::inner_join(weights_table,
                                     loadings_table,
                                     by = c("composite", "indicator"))
  
  # Create returnable list
  return_results <- list(data = data,
                         metrics = metrics_table)
  
  
  # Return
  return(return_results)
  
}
