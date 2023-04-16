# EC508
# Jimin Oh

# rm(list=ls())
setwd("/Users/jimin/Desktop/TA materials/EC508/JJ/Discussions/Week 10")

library(AER)
library(foreign)
library(sandwich) 
library(lmtest)

# Load the data
dat = read.dta('wage2.dta') 

# Generate expersq and tenuresq
dat$expersq = dat$exper^2
dat$tenuresq = dat$tenure^2

# Run the first-stage regression
first_stage = lm(educ ~ sibs + exper + expersq + tenure + tenuresq,data=dat)
fs_res = coeftest(first_stage, vcov. = vcovHC)

print(fs_res)

# Run both OLS and 2SLS:

ols = lm(lwage ~ educ + exper + expersq + tenure + tenuresq,data=dat)
tsls = ivreg(lwage ~ educ + exper + expersq + tenure + tenuresq 
             | sibs + exper + expersq + tenure + tenuresq,data=dat)

coeftest(ols,vcov. = vcovHC)
coeftest(tsls,vcov. = vcovHC)

robse_ols <- sqrt(vcovHC(ols)[2,2])
robse_tsls <- sqrt(vcovHC(tsls)[2,2])

CI_ols <- ols$coef[2]+1.96*robse_ols*c(-1,1)
CI_tsls <- tsls$coef[2]+1.96*robse_tsls*c(-1,1)

print(rbind(CI_ols, CI_tsls))

#               [,1]       [,2]
# CI_ols  0.06070827 0.08743524
# CI_tsls 0.08145833 0.18915900

# Run the first-stage again. Now, we have two instruments.
first_stage2 = lm(educ ~ sibs + brthord + exper + expersq + tenure + tenuresq,data=dat)
fs_res2 = coeftest(first_stage2, vcov. = vcovHC)

print(fs_res2)

coef = first_stage2$coef
V = vcovHC(first_stage2)

# Restrictions on the instruments used to compute the first-stage F-statistic
R = rbind(
  c(0,1,0,0,0,0,0),
  c(0,0,1,0,0,0,0)
)

# Compute the first-stage F-statistic
F = t( R%*%coef )%*%solve( R%*%V%*%t(R), R%*%coef )/2

# It's less than 10....
print(round(F,2))

# Run 2SLS with sibs and brthord as instruments:
tsls2 = ivreg(lwage ~ educ + exper + expersq + tenure + tenuresq 
             | sibs + brthord + exper + expersq + tenure + tenuresq,data=dat)
coeftest(tsls2,vcov. = vcovHC)
