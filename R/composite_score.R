#' Composite Scoring
#' 
#' @description
#' Create composite scores of scales by specifying the indicators that go into
#' each respective composite variable.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' @param weight The weight type to be used in calculating the composite score. 
#' For composite scores emulating reflective measurements, specify as
#' `"correlation"` to calculate the composite score with correlation weights.
#' For composite scores emulating formative measurements, specify as
#' `"regression"` to calculate the composite score with regression weights.
#' The default is set to correlation weights.
#' 
#' @return A dataframe identical to the input dataframe, with additional columns 
#' appended at the end. These new columns represent the calculated composite 
#' scores.
#' 
#' @examples
#' 
#' dat <- data(grit)
#' 
#' # Specify the named list with composite names and their respective indicators
#' varlist <- list(extraversion = sprintf("e%01d", seq(10)),
#'                 neuroticism = sprintf("n%01d", seq(10)),
#'                 agreeableness = sprintf("a%01d", seq(10)),
#'                 conscientiousness = sprintf("c%01d", seq(10)),
#'                 openness = sprintf("o%01d", seq(10)),
#'                 grit = sprintf("gs%01d", seq(12)))
#' 
#' # Calculate correlation weighted composite scores
#' correlation_data <- composite_score(data = dat,
#'                                     varlist = varlist,
#'                                     weight = "correlation")
#' 
#' # Calculate regression weighted composite scores
#' regression_data <- composite_score(data = dat,
#'                                    varlist = varlist,
#'                                    weight = "regression")
#' 
#' @export
composite_score <- function(data = .,
                            varlist,
                            weight = "correlation"){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  # Create empty list to score composite scores
  composite_scores <- list()
  
  for(i in seq_along(varlist)){
    
    
    # Get dataframe with just indicator vars
    df <- data %>% 
      dplyr::select(dplyr::all_of(varlist[[i]]))
    
    
    # Calculate correlation matrix
    cor_matrix <- stats::cor(df,
                             use = 'complete.obs')
    
    # Get the correlation weights
    cor_weights <- colMeans(cor_matrix)
    
    
    # If correlation weights, save composite score
    if(weight == "correlation"){
      
      # Calculate correlation weighted composite score
      composite_scores[[names(varlist)[i]]] <- rowMeans(sweep(df,
                                                              2,
                                                              cor_weights,
                                                              "*"),
                                                        na.rm = T)
      
    }
    
    
    # If regression weights, run linear model to get regression weights
    if(weight == "regression"){
      
      # Get names of indicators
      indicators <- varlist[[i]]
      
      # Write linear model formula
      lm_formula <- as.formula(paste(names(varlist)[i], "~",
                                     paste(indicators, collapse = " + ")))
      
      
      # Standardize variables in dataframe to align correlation coefficient with
      # standardized beta coefficient
      df <- df %>% 
        dplyr::mutate(dplyr::across(everything(),
                                    ~ scale(.)))
      
      # Calculate correlation weighted composite score and save to dataframe
      df[[names(varlist)[i]]] <- rowMeans(sweep(df,
                                                2,
                                                cor_weights,
                                                "*"),
                                          na.rm = T)
      
      
      # Calculate regression weights
      lm_model <- lm(lm_formula,
                     data = df)
      
      # Save regression weights
      reg_weights <- coef(lm_model)[setdiff(names(coef(lm_model)), "(Intercept)")]
      
      
      # Calculate regression weighted composite score
      composite_scores[[names(varlist)[i]]] <- rowMeans(df[, -ncol(df)] * reg_weights,
                                                        na.rm = T)
      
    }
    
  }
  
  
  # Convert the list to a dataframe
  composites <- purrr::map_df(composite_scores, ~.x)
  
  # Combine with data
  data <- cbind(data,
                composites)
  
  
  # Return
  return(data)
  
}