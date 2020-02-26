# To run in R Studio:
# Drowpdown panel: Session > Set Working Directory > To Source File Location

birth.table <- read.table('lowbwt.dat')

###PART I (magda /// 26.02.2019)
#starting model with main effects and interactions terms we hypothesize might be relevant
summary(glm(LOW ~ AGE + LWT +FTV + RACE + LWT*FTV + AGE*FTV, 
            data=lowbwt.small, 
            family=binomial(link="logit")))
#Residual deviance: 211.99 

#remove LWT*FTV - high p-value
summary(glm(LOW ~ AGE + LWT +FTV + RACE + AGE*FTV,  
            data=lowbwt.small, 
            family=binomial(link="logit")))
#Residual deviance: 212.62

#remove RACE - high p-value (age is higher but we can't remove it if we can't to keep AGE*FTV which appears to be very significant)
summary(glm(LOW ~ AGE + LWT +FTV + AGE*FTV , 
            data=lowbwt.small, 
            family=binomial(link="logit")))
#Residual deviance: 215.37
#these are all significant 

##technically residual deviance is going up as we remove terms (which is expected because this will inevitably happen as we remove variables)
   ## we can test using AIC to see that removing these terms does actually improve the model though

##BUT how is this different from the backwards selection we have to do in part 2 of analysis 1?
