# cscoring

## Description

In the realm of psychology and other social behavioral sciences, latent constructs are often measured using scales. These scales commonly undergo psychometric validation through methods such as exploratory factor analysis, confirmatory factor analysis, and principal component analysis. It is also common to observe varying factor loadings among indicators, implying that some are more reflective of their latent construct than others.  

Despite this, many researchers outside of scale development or factor analysis-related studies (e.g., structural equation modeling) tend to create an unweighted composite score by simply averaging or summing the indicator items. This prevalent practice in psychology overlooks the disparity in factor loadings across indicators, thereby assuming that all indicators are equally representative of the latent construct. Although this assumption may hold true for some scales, it is seldom the case in practice. In other words, for many studies, researchers unintentionally apply a factor structure on their scales that have not been validated.  

To address this issue, this package provides a straightforward solution for researchers to generate weighted composite scores. Users can specify the indicators and choose between correlation or regression weights. This approach ensures a more accurate representation of the latent constructs, enhancing the validity of the research findings.


## Installation

This package can be installed directly from GitHub with `devtools`:

```r

# install.packages("devtools")
devtools::install_github("imh-ds/cscoring")

```


## Usage: Composite Scoring With Correlation Weights

The composite score is a weighted average of the indicator variables. In a simple average or sum composite, each variable contributes equally to the final score. However, in a weighted average, each variable contributes proportionally to its weight. In other words, variables with higher weights have a greater influence on the composite score.  

For correlation weights, the weights are determined by the average correlation of each variable. Taking the average correlation of each indicator measures how well each variable represents the common trend among all indicators. Indicators highly correlated with others will have a higher average correlation and thus a higher weight, contributing more to the composite score calculation. This takes into account how well each variable represents the common trend among all variables. This approach provides a more nuanced representation of the latent construct, as it takes into account the varying associations between the indicators.  

By default, the `composite_score` function will use correlation weights. To use the function with correlation weights, you will only need to specify a named list of vectors. Each name in the list should represent the name of the composite score variable, and the corresponding vector should contain the names of the indicator variables.

```r

# Load data
dat <- data(grit)

# Specify the named list with composite names and their respective indicators
varlist <- list(extraversion = sprintf("e%01d", seq(10)),
                neuroticism = sprintf("n%01d", seq(10)),
                agreeableness = sprintf("a%01d", seq(10)),
                conscientiousness = sprintf("c%01d", seq(10)),
                openness = sprintf("o%01d", seq(10)),
                grit = sprintf("gs%01d", seq(12)))

# Calculate correlation weighted composite scores
correlation_data <- composite_score(data = dat,
                                    varlist = varlist,
                                    weight = "correlation")

```


## Usage: Composite Scoring With Regression Weights

In the context of regression weights, the weights are derived from a linear regression model. Specifically, the correlation-weighted composite score is regressed on the indicators, and the resulting standardized regression coefficients (i.e., beta coefficients) are utilized as weights. This implies that the contribution of each indicator to the composite score is commensurate with its influence on the criterion variable, which in this case is the correlation-weighted composite score. Indicators exerting a greater influence on the correlation composite score will possess larger coefficients, thereby contributing more significantly to the regression-weighted composite score.

Several key differences between the correlation-weighted and regression-weighted approaches warrant attention. Firstly, due to the potential discrepancy in rank-order between unstandardized and standardized coefficients, all indicators are standardized prior to their inclusion in the linear regression model. Secondly, the use of indicators in the linear regression model necessitates careful consideration of multicollinearity issues. High correlation among indicators can lead to multicollinearity, which may introduce bias or incorrect regression weights in the composite score computation. Lastly, as the regression weights are the beta coefficients, the proportion of variance in the composite score explained by the indicators will approach 100%.

To use the function with regression weights, you will need to specify a named list of vectors like in the case with correlation weights, and also specify `weight = "regression"`. Each name in the list should represent the name of the composite score variable, and the corresponding vector should contain the names of the indicator variables.

```r

# Load data
dat <- data(grit)

# Specify the named list with composite names and their respective indicators
varlist <- list(extraversion = sprintf("e%01d", seq(10)),
                neuroticism = sprintf("n%01d", seq(10)),
                agreeableness = sprintf("a%01d", seq(10)),
                conscientiousness = sprintf("c%01d", seq(10)),
                openness = sprintf("o%01d", seq(10)),
                grit = sprintf("gs%01d", seq(12)))

# Calculate regression weighted composite scores
regression_data <- composite_score(data = dat,
                                   varlist = varlist,
                                   weight = "regression")

```