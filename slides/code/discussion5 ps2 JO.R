# EC508 PS2
# Jimin Oh

######################################################################
#             EC 508: Problem Set 2 Codes & Comments                 #
######################################################################
rm(list=ls()) # clear all
setwd("/Users/jimin/Desktop/TA materials/EC508/JJ/Problem Sets/ps2")

##################################
#        Problem 1: Suits
##################################

library(foreign)

# packages to compute standard errors
library(sandwich) 
library(lmtest)

# Reading in data
data <- read.dta('lawsch85.dta') # 'foregin' package, load data.
data <- subset(data,!is.na(salary) & !is.na(cost))
# The dataset 'data' now contains observations with non-missing salary and cost.

# (i) Compute average starting salary across law schools
print(mean(data$salary))

# Note: the salary variable records "median" starting salary of each law school in the dataset.
# Assume two schools with students' starting salary: (1,3,3,10), (3,4,5).
# The first one has a median of 3, while the second has a median of 4. Average of the two schools is (3+4)/2=3.5
# BUT the mean starting salary of all law students is (1+3+3+3+4+5+10)/7=4.14, not the same.

# What if we have a new variable called salary_new in the dataset
# which records "mean" starting salary of each law school?
# Again, we consider an example of two schools with (1,3,3,10), (3,4,5).
# The first one has a mean of 4.25, while the second has a mean of 4. Average of the two groups is (4.25+4)/2=4.125
# BUT the avg starting salary of all law students is (1+3+3+3+4+5+10)/7=4.14, not the same.
# Why this happened? Law of iterated expectations
# E(salary_new) = E(salary_new|a student is from school 1)*P(a student is from school 1)
# + E(salary_new|a student is from school 2)*P(a student is from school 2)
# = 4.25*4/7 + 4*3/7 = 4.14, the same as the average starting salary across all law students.
# Therefore, when applying the LIE, one should take into account the probability of each event.


# (ii) reg salary on ranking
reg1 <- lm(salary ~ rank, data=data)
coeftest(reg1) # 'lmtest' package

# Heteroskedasticity-robust standard errors
coeftest(reg1, vcov. = vcovHC) # vcovHC from 'sandwich' package computes a het-robust var-cov matrix.

# Note SEs change but the coefficients remain the same.
robse1 <- sqrt(vcovHC(reg1)[2,2]) # vocvHC computes a het-robust var-cov matrix. 
# Diagonal elements of the var-cov matrix show variance of each coefficients.
# [1,1] shows var(beta0_hat) and [2,2] shows var(beta1_hat).
# So we need to take a square root to get SEs.
# Compare the HET-robust SE with the non-robust one below
se1 <- coef(summary(reg1))[2,2]

CI1 <- reg1$coef[2]+1.96*robse1*c(-1,1)
print(CI1)

# (iii) Expected difference in salary between the 20th and the 40th top law schools   
# Notice that yhat_20 - yhat_40 = beta1hat*(20-40) = -20*beta1hat
diff_sal <- reg1$coefficients[2]*(20-40)
# Alternatively, to answer this question, one can use yhat40-yhat20 = beta1hat*(40-20) = 20*beta1hat

CI1_diff <- CI1*(20-40) 
print(CI1_diff)
# Now the first one is upper bound and the second one is the lower bound as we're multiplying a negative number.
# Thus, when reporting the reulsts, we should change the lower and upper bounds.
# [3716.556, 4766.999]
# We can construct the CI manually based on se(-20*beta1hat) = 20*se(beta1hat)
CI1_diff_manual <- diff_sal+1.96*abs(-20)*robse1*c(-1,1)
print(CI1_diff_manual)


# (iv) Reg cost on rank
# From now on, I will use HET-robust SEs by default.
reg2 <- lm(cost ~ rank, data=data)
coeftest(reg2, vcov.=vcovHC)

robse2 <- sqrt(vcovHC(reg2)[2,2])
CI2 <- reg2$coef[2]+1.96*robse2*c(-1,1)

print(CI2)

# (v) Expected difference in cost between the 20th and the 40th top law schools 
diff_cost <-reg2$coefficients[2]*(20-40)
CI2_diff <- CI2*(20-40)
print(CI2_diff) # again, first number = upper bound and second number = lower bound as we multiplied a negative number.
# [513.6917,1036.5364]

# (vi) Cost-Benefits?
reg_results <- rbind(reg1$coef, reg2$coef)
reg_results
# A more prestigious school by 1 rank (1st = most prestigious) is associated with 
# an increase in salary (by 212.09 on avg) from reg1
# and an increase in cost (by 38.76 on avg) from reg2

expected_diff <- rbind(diff_sal, diff_cost)
expected_diff
# Between the 20th and 40th top low school, the expected difference in median salary after graduation is 4241.778,
# while the expected diff in law school costs is 775.114. 

CIs <- rbind(CI1_diff, CI2_diff)
CIs
# Between the 20th and 40th top low school, the 95% CI for expected difference in salary: [3716.5563, 4766.999],
# while the 95% CI for the expected diff in law school costs is [513.6917, 1036.536].

# Relative benefits of attending a more prestigious program seem to be bigger than its costs.

# (vii) 
library(ggplot2)
# Plot: cost vs. rank
plot(data$rank,data$cost)
ggplot(data, aes(x=rank, y=cost)) + geom_point() +
  geom_smooth(method=lm, formula= y~x, se=F)

# A1: may not hold. OVB. Other factors may affect costs and be correlated with rank (e.g., the number/quality of faculty)
# but not included in the model (cost = beta0 + beta1*rank + u) as another regressor.
# We may need a model with multiple regressors.
# A2: may not hold. Simple random sampling? Top 156 law schools?
# A3: seems to hold. No obvious outliers.

# Plot: salary vs. rank
plot(data$rank,data$salary)
ggplot(data, aes(x=rank, y=salary)) + geom_point() +
  geom_smooth(method=lm, formula= y~x, se=F)

# A1: may not hold. OVB. Other factors may affect salary and be correlated with rank (e.g., LSAT scores)
# but not included in the model (salary = beta0 + beta1*rank + u) as another regressor.
# We may need a model with multiple regressors.
# A2: may not hold. Simple random sampling? Top 156 law schools?
# A3: may not hold. Note that the linear line doesn't fit well in the graph for salary against rank. 

# (viii)
plot(data$rank,log(data$salary)) # or you can use lsalary variable in the dataset.
ggplot(data, aes(x=rank, y=log(salary))) + geom_point() +
  geom_smooth(method=lm, formula= y~x, se=F)

# A1: may not hold. OVB. Even if we transform the dependent variable in logarithm, 
# the OVB problem is not resolved. Still, there could be other factors in the error term (e.g., LSAT scores)
# that may affect log(salary) and be correlated with rank.
# We may need a model with multiple regressors.
# A2: may not hold. Simple random sampling? Top 156 law schools?
# A3: may/may not hold. Note that the linear line seems to fit better than before but not perfect.


# (ix)  
reg3 <- lm(log(salary) ~ rank, data=data)
coeftest(reg3, vcov.=vcovHC)

# If a program is higher ranked (more prestigious, smaller absolute value for 'rank') by 1 rank, 
# then medain salary is exepcted to increase by approximately 100*0.00504=0.504% on average.

robse3=sqrt(vcovHC(reg3)[2,2])
CI3 <- reg3$coefficients[2] + 1.96*robse3*c(-1,1)
print(CI3)


##################################
#    Problem 2: Real Estate 
##################################

rm(list=ls()) # clear all 
hprice1<-read.dta('hprice1.dta') # load data

# (i) 
reg1 <- lm(price ~ sqrft + bdrms, data=hprice1)
reg1res <- coeftest(reg1, vcov. = vcovHC) # Use HET-robust SEs.

print(reg1res) # Report the estimated coefficients and standard errors

# (ii)
mean(hprice1$price)
# On avg, an additional bdrm is associated with an increase in price by $15.198*1,000=$15,198, holding all else constant. 
# The avg selling price is $293.546*1,000 = $293,546.
# The magnitude of the increase in price associated with an additional bedroom is approx. 5.2% (15198/293546) of the avg price.

# (iii)
# Recall:
# When determining statistical significance of a variable x, the proposed null hypothesis is H0: beta_x= 0.
# We can use p-value, t-stat, or CI to test the null against the alternative H1. Refer to discussion 4.

robse_bdrms <- sqrt(vcovHC(reg1)[3,3]) # Note: [1,1] for beta0_hat, [2,2] for beta1_hat, [3,3] for beta2_hat
CI_bdrms <- reg1$coefficients[3]+1.96*robse_bdrms*c(-1,1)
print(CI_bdrms) 
# bdrm is not stat. significant (H0: beta1 = 0 is not rejected in the two-sided test) at the 5% sig level
# as 0 is contained in the 95% confidence interval.

# Intuitive.
# Holding the size of a house (sqrft) unchanged, increasing the number of bedrooms might not be desirable
# as each bedroom's size will be smaller. So it may not be associated with a rise in price.

# (iv)
# There is 1 additional bedroom and the total size of the house increases by 140 sq ft.
predicted_inc <- 140*reg1$coefficients[2]+1*reg1$coefficients[3]
print(predicted_inc)
# $33.179*1,000 = $33,179 increase in price. 
# The magnitude is of this increase is approx. 11.3% (33179/293546) of the avg price.

# (v) 
# We can try three diffefent methods: p-value, t-stat, or CI
print(reg1res) # Report the estimated coefficients and standard errors
# sqrft is statistically significant (H0: beta1 = 0 is rejected in the two-sided test)
# even at the 1% significance level as t-stat >> 2.58 and p-val << 0.01.

robse_sqrft <- sqrt(vcovHC(reg1)[2,2]) # Note: [1,1] for beta0_hat, [2,2] for beta1_hat, [3,3] for beta2_hat
CI_sqrft <- reg1$coefficients[2]+1.96*robse_sqrft*c(-1,1)
print(CI_sqrft) 
# The above 95% CI shows that the variable is stat. sig. at the 5% level (as expected from the p-val and t-stat)
# as 0 is not contained in the CI.

# The statistical significance of sqrft is intuitive as people tend to prefer larger houses and thus they are more expensive.


# (vi)
head(predict(reg1), n=1) # only getting the predicted value for the first observation using head() and predict()

# Alternatively, we can create a new dataframe and plug it in predict()
new.sqrft <- data.frame(sqrft = 2438)
new.bdrms <- data.frame(bdrms = 4)
predict(reg1, newdata = cbind.data.frame(new.sqrft,new.bdrms))

# Alternatively, we can plug the numbers into the equation and manually compute the fitted value for the first obs.
reg1$coefficients[1] + 2438*reg1$coefficients[2] + 4*reg1$coefficients[3]

# Note that the predicted selling price ($354,605.2) is different from the observed/actual selling price ($300,000).
# This is usually the case: fitted values are usually different from actual data points.

# (vii)
resid <- reg1$residuals[1] # residual for first obs: uhat = y - yhat
print(resid) # underpaid by 54.60525 * $1,000 = $54,605


##################################
#  Problem 3: Omitted Variables 
##################################

# For i-iv, refer to discussion class.

rm(list=ls()) # clear all 

# (v) Numerical Experiment
set.seed(123)
n <- 1000
u <- rnorm(n) # u ~ N(0,1)
x <- rnorm(n) # x ~ N(0,1)
v <- rnorm(n) # v ~ N(0,1)
z <- x+v # cov(z,x) = cov(x+v,x) = cov(x,x) + cov(v,x) = var(x) = 1 since cov(v,x)=0 as they were independently drawn.
y <-  0 + x - z + u # true model
reg_wrong <- lm(y ~ x)    # run a reg without z.
regres_wrong <- coeftest(reg_wrong, vcov. = vcovHC)
print(regres_wrong)
# Assume a two-sided test.
# Fail to reject null H0: beta1 = 0 even at the 10% significance level (also at the 5% and 1% levels).
# t-val < 1.645, p-val > 0.1.

# Note:
# There is no perfect multicollinearity in the true model by construction:
# corr(x,z) = cov(x,z)/sqrt(var(x)*var(z)) = 1/sqrt(1*2) = 1/sqrt(2)
# since var(x) = 1 as x ~ N(0,1), var(z) = var(x+v) = var(x) + var(v) = 2 as x, v ~ iid N(0,1)
# and cov(x,z) = cov(x,x+v) = cov(x,x) + cov(x,v) = var(x) = 1 as x, v ~ iid N(0,1)

# (vi)

# By omitting z when running a regression, the partial effect of x on y is biased downward 
# True model: partial effect of z on y is -1. Also, z and x are positively correlated (cov(x,z) = 1) by construction.
# As shown in discussion class,
# plim (beta1hat - beta1) = beta2 * cov(x,z)/var(x) = -1 * 1/1 = -1 < 0. (Note: var(x)=1 as x ~ N(0,1))
# Thus, the OLS estimate, beta1hat, suffers from a downward bias when regression y on x only.
# Because of the bias, plim (beta1hat) = beta1 + bias = 1 + (-1) = 0.
# Thus, we saw from part (v) that beta1 is not statistically different from 0, as expected from the bias in the prob limit.

# Now, let's compute the bias.
# I will show you two different ways: in the probability limit and in the given sample.
# A. Bias in the probability limit: beta2 * cov(z,x)/var(x)

beta2 <- -1
cov_xz <- 1  # cov(z,x) = cov(x+v,x) = cov(x,x) + cov(v,x) = var(x) = 1
var_x <- 1   # x ~ N(0,1). So, var(x) = 1.
bias <- beta2 * cov_xz/var_x  # probability limit of (beta1hat - beta1)
print(bias)

# But this is the bias in the prob limit.
# Then, how does the R compute the bias when regressing y on x only?
# Note 1: In reality, we don't know the true value for beta2.
# Note 2: In reality, we don't know the true population cov(x,z) and population var(x).
# Note 3: We only have a sinlge sample like now (n=1000).

# B. Bias in the given sample: beta2_hat * delta1_hat

# As shown in discussion class,
# Consider a population model: z = delta0 + delta1 * x + w
# delta1 = cov(x,z)/var(x) is the population effect of x on z.
# The sample counterpart for this is:
# delta1_hat = ols estimator of delta1 when regressing z on x.
reg_zx <- lm(z ~ x) # delta1_hat = 1.02568, which is the same as sample_cov(x,z)/sample_var(x)

# As we don't know true beta2 (in reality), we use the beta2_hat from regressing y on x and z.
regtrue <- lm(y ~ x + z) # Regressing the true model
summary(regtrue)
# Note that the estimate (beta1_hat) when regressing the true model is still not equal to beta1 = 1.
# Also, the estimate (beta2_hat) when regressing the true model is still not equal to beta2 = -1.
# This is usually the case as we are using a single sample of size = 1000.

bias_R <- regtrue$coefficients[3] * reg_zx$coefficients[2] 
# In a given sample, bias = beta2_hat * delta1_hat.
print(bias_R)

# Let's check whether the bias we manually computed is the same as what R computed.
beta1_R <- regtrue$coefficients[2] + bias_R    # beta1_hat from the wrong model = beta1_hat from the true model + bias_R
print(c(beta1_R, reg_wrong$coefficients[2])) 
# Compare beta1_R (a manually computed estimate for beta1 in the wrong model) 
# with the beta1_hat which R computed when regressing the wrong model.
