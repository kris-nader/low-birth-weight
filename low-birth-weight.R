
# To run in R Studio:
# Drowpdown panel: Session > Set Working Directory > To Source File Location

## Read data
birth.table <- read.table('lowbwt.dat', header=T, row.names=1)
 
## Fitting a logistic regression
# Example: LWT
birth.ht <- glm(formula=LOW ~ HT, data=birth.table, family=binomial(link="logit"))
summary(birth.ht)
#### Results interpretation:
## Deviance residuals: Quartiles
## Coefficients:
# - Estimate: Parameter (Beta) values of the model
# -- In this case: (low birth weight) = -0.877 + 1.214*(history of hypertension)
# -- Log-odds of low birth weight w/o hypertension: -0.877
# -- Log-odds of low birth weight w/ hypertension: 0.337
# - Std. Error & z value:
# -- Shows how Wald's test was computed for both coefficients
# - Pr(>|z|): p-values
# -- p < 0.05, results are statistically significant
# -- In this case, both p < 0.05
# -- In addition to p < 0.05, we also want large effect sizes (given by log-odds/log-odds ratio)
# Null Deviance:
# Residual Deviance: 
# Akaike Information Criterion (AIC): Value used to compare models
 
# Next steps: 
# - Use more parameters in model building
# - eliminate predictors based on p-values

