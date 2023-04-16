# EC508
# Jimin Oh

library(haven)
data = read_dta('WAGE1.dta')

model = lm(lwage~educ+exper+I(exper^2),data=data)
coeftest(model,vcov = vcovHC)

plot(data$exper, data$lwage)

# exper going from 10 to 11 years
print(model$coeff[3] + 2*model$coef[4]*10)
print(model$coeff[3]*11 + model$coef[4]*11^2 - (model$coeff[3]*10 + model$coef[4]*10^2))

# turning point
print(abs(model$coeff[3]/(2*model$coeff[4])))

# appropriate specification for tenure
plot(data$tenure, data$lwage)

# cubic
model2 = lm(lwage~educ+exper+I(exper^2)+tenure+I(tenure^2)+I(tenure^3),data=data)
coeftest(model2,vcov = vcovHC)

# test for linearity
V2 = vcovHC(model2) # variance-covariance
R = rbind( 
  c(0,0,0,0,0,1,0),
  c(0,0,0,0,0,0,1)
) # Restriction matrix

beta = model2$coef # Coefficients

w = t(R%*%beta)%*%solve(R%*%V2%*%t(R),R%*%beta) # Wald statistic
print(w)
print(qchisq(0.95,2)) # critical value

# quadratic
model3 = lm(lwage~educ+exper+I(exper^2)+tenure+I(tenure^2),data=data)
coeftest(model3,vcov = vcovHC)

V3 = vcovHC(model3)

CI = model3$coeff[6] +c(-1,1)*1.645*sqrt(V3[6,6])