#Jimin Oh
#PS3
rm(list=ls()) 
# Preliminaries
setwd("/Users/jimin/Desktop/TA materials/EC508/JJ/Problem Sets/ps3 IV")
library(foreign)
library(sandwich) 
library(lmtest)
library(ivreg)
library(haven)

#################### Problem 1: Labour Supply #########################

# Reading in data
  data<-read.dta('fertility.dta')
# 1(i)
  olsm1 <- lm(weeksm1~morekids+agem1+black+hispan+othrace, data=data)
  olsm1results<-coeftest(olsm1, vcov. = vcovHC)
  print(olsm1results)
  
  olsm1se<-sqrt(vcovHC(olsm1)[2,2])
  olsm1_ci<-olsm1$coefficients[2]+1.96*olsm1se*c(-1,1)
  print(olsm1_ci) 
  confint(olsm1results,"morekids",level=0.95)
  #The effect is statistically significantn at the 5% significance level as 0 is not included in the 95% CI.
  
  comparison<- cbind(abs(olsm1$coefficients[2]), mean(data$weeksm1), median(data$weeksm1))
  print(comparison)
  # The magnitude is economically significant. 
  # Having more than 2 children is expected to decrease mom's working weeks by 6 weeks on average (compared to whom?)
  # where this is about 32 % of the mean working weeks in 1979 and 120 % of the median working weeks.
  
  # Note: the mean and median values are very different. What does it imply?
  hist(data$weeksm1)
  # When interpreting the estimated coefficient for a binary regressor, 
  # one should be careful about which group you're comparing with.
  # In the above case, which two groups are we comparing with? Discuss.
  
  #1(ii)
  # Discuss: multicollinearity is not an issue in this case.
  # Recall: betahat = (X'X)^(-1)(X'Y) where X'X is not invertible if there is perfect multicollinearity. which means that beta can not be computed.
  # (a) Check pairwise correlation coefficients:
  regressors<-cbind(data$morekids, data$agem1, data$black, data$hispan, data$othrace)
  cor(regressors, use = "complete.obs")
  # Q: why am I not considering the intercept when checking pairwise correlation?
  # (b) Conceptual discussion (binary variables and dummy variable trap)
  
  # Need to be concerned about endogeneity. 
  # (a) There is a simultaneity concern with weeksm1 (Y) and morekids.(X)
  #     A mother may be unable to work more if she has more children X->Y
  #     At the same time, a mother who does not work much (for some other reasons), may
  #     find herself able and willing to raise more children Y->X
  # (b) OVB: mom's socioeconomic status, family income, etc.
  
  
  #1(iii)
  # Relevance: Some parents like to have both boys and girls. If the first two children 
  # are the same sex, then it is likely to imply (in part) whether or not the parents
  # try for a third (fourth, etc.) child. Z (same sex) -> X (morekids)
  # What are implicit assumptions?
  
  # Exogeneity is debatable:
  # Exogeneity might be violated if people can detect the gender of the infant and decide to abort based on gender.
  # Why? Discuss.
  
  #1(iv)
  ivm1 <- ivreg(weeksm1~morekids+agem1+black+hispan+othrace,~samesex+agem1+black+hispan+othrace,data=data)
  
  ivm1results <- coeftest(ivm1, vcov. = vcovHC)
  # the IV estimate for beta is still around -6, so not very different from the OLS results.
  # That is, the magnitude is very large as we compared in part (i)
  # Three possibilities:
  # 1, There is no endogenity issue and OLS works well.
  # 2, the relevance restriction failed.
  # 3, the exogeneity restriction failed.
  
  # IV-GMM estimator and optimal weighting matrix
  # Let's report the optimal standard errors:
  # Equations are in GMM's silde, page 6
  X = cbind(1,data$morekids,data$agem1,data$black,data$hispan,data$othrace) # X matrix
  Z = cbind(1,data$samesex,data$agem1,data$black,data$hispan,data$othrace) # Z matrix
  e = ivm1$residuals # residuals, u_hat
  eZ = kronecker(e,matrix(1,1,dim(Z)[2]))*Z # e*Z, used to compute standard errors
  n  = dim(Z)[1] # sample size
  
  # The formula below I used the property that Z and X are just identified.
  # Generally, the optimal variance is (Q_xz*H^(-1)*Q_zx)^(-1). When the number of columns
  # are different between X and Z, Q_zx = E(Z'X) and Q_xz =E(X'Z) are not invertible, because they are not 
  # square matrices.
  
  # However, under just identification, Z and X have the same numbers of columns. So we have
  # (Q_xz*H^(-1)*Q_zx)^(-1)=Q_zx^(-1)*H*Q_xz^(-1)
  # (A*B)^(-1)=B^(-1)*A^(-1)
  # (A*B*C)^(-1)=C^(-1)*B^(-1)*A^(-1)
  
  # A=Q_xz= t(X)%*%Z/n
  # B=H^(-1)=var(eZ)^(-1)
  # C=Q_zx^(-1)=t(Z)%*%X/n
  
  # Under Heteroskedasticity, H = E(u^2*Z'Z) where u: error term
  # Under homosk-, H = var(u) * E(Z'Z) where u: error term
  SigHeterk = solve(t(Z)%*%X/n,var(eZ))%*%solve(t(X)%*%Z/n) # formula for HC robust variance-covariance matrix
  SigHomosk = solve(t(Z)%*%X/n,var(e)*t(Z)%*%Z/n)%*%solve(t(X)%*%Z/n) # formula for homoskedastic variance-covariance matrix

  ivses = sqrt( diag(SigHeterk)/n ) # standard errors
  ivm1results2<-cbind(ivm1$coefficients, ivses)
  print(ivm1results2)
  
  ivm1_CI=ivm1$coefficients[2]+1.96*ivses[2]*c(-1,1)
  print(ivm1_CI) 
  #It is similar to the one above
  # Gmm, W matrix as the weighting matrix. It IS USEFUL UNDER the overidentified case. 
  # Under the just identified case, it is similar to the 2sls std.
  
  confint(ivm1results,"morekids",level=0.95)
  #Yes, the effect is statistically significant and also economically significant.
  #It remain similar with the one estimated by OLS.
  
  #1(v)
  # First stage regression
  fsm1<-lm(morekids~samesex+agem1+black+hispan+othrace, data=data)
  coef_fsm1 <- fsm1$coefficients
  V_fsm1 = vcovHC(fsm1)
  # Only one instrumental variable, so can just look at the t-stat.
  # But let's get F-stat to illustrate:
  R = rbind(
    c(0,1,0,0,0,0)
  )
  
  # Compute the first-stage F-statistic where F = 1/m * w and w is the Wald statistic
  F = t( R%*%coef_fsm1 )%*%solve( R%*%V_fsm1%*%t(R), R%*%coef_fsm1 )/1
  
  # F-stat is high. Weak instruments are not likely to be a concern
  
  
#################### Problem 2: Wages #########################
rm(list=ls()) 
# 2(i)
data<-read_dta('cps12.dta')
# 2(ii)
olsm2 <- lm(I(log(ahe))~age+I(age^2)+female+bachelor,data=data)
olsm2results<-coeftest(olsm2, vcov. = vcovHC)
print(olsm2results)
#Yes. The effect of age looks non-linear. The coefficient on age2 can't be rejected at 90% level.

#2 (iii)
# y= b0 + b1*age + b2*age^2 + other terms
# Marginal effect of age = dy/d(age) = b1+b2*2*age

# Point estimates suggest a nonlinear effect, although 
# b2_hat is -1.3x10^{-3}, quite small compared to b1_hat = 0.104.
# Decreasing since beta2<0

#2 (iv)
# At maximum, marginal effect of age=0. That is, the slope is 0.
# So, age=-b1/(2*b2)
optage<- -olsm2$coefficients[2]/(2*olsm2$coefficients[3])
cbind(min(data$age), mean(data$age), max(data$age), optage)
#The turning point is around 39.2 years old. 
#But it cannot be actually reached since the maximum age is 34 in the sample.
#That is, 39.2 years old is outside the range of the data.
#But can we still conceptually try by plugging in age=39.2....???
#Caution 1: We should not extrapolate outside the range of the data.
#Caution 2: When using log(y) as a dependent variable and then predicting y (i.e., y_hat) 
#based on the algebraic fact that exp(log(y)) = y, 
#we get underestimated values of y_hat.
#So we need to fix the underestimation with an adjustment factor.




