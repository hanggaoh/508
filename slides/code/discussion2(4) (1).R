# EC508
# Prof. JJ Forneron
# TF Jimin Oh
######################################################################################################
# for loop, user-defined functions, and interpreting results from linear reg with a single regressor #
######################################################################################################

rm(list=ls()) # clear all
# Preliminaries
# Set a working diriectory
setwd("/Users/jimin/Desktop/TA materials/EC508/JJ/Discussions") # change it to yours.

# for loop
set.seed(1)
x <- rnorm(500) # x drawn from a standard normal N(0,1) mean, sd

# Create a new variable z = sum of all elements of x
z <- 0
for (i in 1:length(x)) {
 z <- x[i] + z
}

x_sum = sum(x, na.rm = F) # sum() is a built-in function from 'base' package.

print(c(x_sum, z)) # check whether the for-loop gave us a correct number.

# How to create a user-defined function

# Create a new function called 'my_sum' which sums up all elements in a vector.
my_sum <- function(t){ # we have only one argument 't' in my_sum() function. Can change 't' with any vectors.
  L = length(t)    # define L to be the length of a vector t
  m = 0            # starting value of m = 0
  for (i in 1:L) { # loop to be run for each i = 1, ...., L
    m = t[i] + m   # m = t[1]+...+t[i] where previous m = t[1]+...+t[i-1]
  }
  return(m)        # returns the desired output of 'my_sum()' function
}

my_sum(x)
print(c(x_sum, my_sum(x)))
# we can check whether my_sum() indeed produces the same result
# as sum() from R's base package.

# How to interpret results from linear regression with a single regressor
dat <- data.frame(matrix(ncol=2, nrow=500))
colnames(dat)  <- c('x','y')

dat$x <- x
set.seed(1)
dat$y <- runif(500) # y drawn from U[0,1]
#plot(density(dat$y))

# Creaated a sample 'dat' containing y and x.
# Can run a linear regression and interpret the results as follows:
reg1 <- lm(y ~ x, data=dat)
summary(reg1)

# If x increases by 1 unit, then y is predicted to increase/decrease by .... unit on average.
# An increase in x by 1 unit is associated with an increase/decrease in y by .... unit on average.
# Be careful about the unit of a variable!
# For instance, in PS1, the varaible 'salary' is in $1000s.

