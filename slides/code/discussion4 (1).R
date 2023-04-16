# EC508
# Jimin Oh

rm(list=ls())
setwd("/Users/jimin/Desktop/TA materials/EC508/JJ/Discussions")

library(foreign)
# packages to compute standard errors
library(sandwich) 
library(lmtest)

# Reading in data
data <- read.dta('MLB1.dta') # 'foregin' package

# Run a regresson of log(salary) on years, gamesyr, bavg, hrunsyr, rbisyr
reg1 <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr + rbisyr, data=data)
res1 <- coeftest(reg1, vcov. = vcovHC) # regression results with heteroskedasticity-robust standard errors
print(res1)

# Construct a confidence interval to make an analysis.
vcovHC(reg1) # vocvHC computes a het-robust variance-covariance matrix. 
robse_year <- sqrt(vcovHC(reg1)[2,2]) # So we need to take a sqrt of the diagonal elements to get SEs.
CI_year <- reg1$coef[2]+1.96*robse_year*c(-1,1)
print(CI_year)

robse_hruns <- sqrt(vcovHC(reg1)[5,5])
CI_hruns <- reg1$coef[5]+1.96*robse_hruns*c(-1,1)
print(CI_hruns)

# Run a regression of salary
reg2 <- lm(salary ~ years + gamesyr + bavg + hrunsyr + rbisyr, data=data)
res2 <- coeftest(reg2, vcov. = vcovHC) # regression results with heteroskedasticity-robust standard errors
print(res2)

head(predict(reg2), n=1) # only getting the first predicted value using head() and predict()

reg2$residuals[1] # residuals for the first obs 
