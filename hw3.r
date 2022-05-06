# Load the ak91.csv data
df <- read.csv("downloads/ak91.csv")
n <- nrow(df)

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

beta <- cov(yrs_educ, wkly_wage)/var(yrs_educ)
# this is 29.62 based on the data
alpha <- mean(wkly_wage) - mean(yrs_educ)*beta
# this is 61.19 based on the data

# economic interpretation of beta: beta is the expected approximate increase
# in weekly wage for each year of additional education

# computing BLP(Y |X = 16) we get Y = 61.19 + 16*29.62
61.19 + 16*29.62
# we get 535.11
blp_yx <- alpha + yrs_educ*beta
# Compute standard error for beta 
epsilon <- wkly_wage - blp_yx
se_numer <- sqrt(mean(epsilon^2*(yrs_educ - mean(yrs_educ))^2))
se <- (se_numer/var(yrs_educ))/sqrt(n)
# se = 0.2101

# hypothesis test

# we do beta/se(beta)

Tn <- (31-beta)/se
# Tn is 6.554693 which is extremely large

# p value is approx 0

# g, yes, very much so

# problem 7: Part A

# Define a custom function to compute the ols estimates
my_simplecoef <- function(y, x) {
  # Compute and return estimates for alpha and beta
  
  beta1 <- cov(x, y)/var(x)
  alpha1 <- mean(y) - mean(x)*beta1
  return(c(beta1, alpha1))
}#MY_SIMPLECOEF
coef <- my_simplecoef(wkly_wage , yrs_educ)



# Part B:
# Define a custom function to compute the blp estimates
my_simpleblp <- function(coef , x) {
  
  ret <- coef[1]*x + coef[2]
  return(ret)
  
}

# Test the function
mean(wkly_wage) - mean(my_simpleblp(coef, yrs_educ)) # 0

# Part C:
# Define a custom function to compute the standard error
my_simplese <- function(coef , y, x) {
  
  blp1 <- my_simpleblp(coef, x)
  # Compute standard error for beta 
  epsilon1 <- y - blp1
  se_numer1 <- sqrt(mean(epsilon1^2*(x - mean(x))^2))
  se1 <- (se_numer1/var(x))/sqrt(length(x))
  return(se1)
}#MY_SIMPLESE

# Test the function using your solution to Problem 6
se <- my_simplese(coef , wkly_wage , yrs_educ)
se

# Part D:

# Define a custom function to compute the test stat and p-value
my_simpleteststat <- function(beta , se) {
  tstat <- abs(beta)/se
  p_val <- 2*pnorm(-abs(tstat))
  return(c(tstat, p_val))
}#MY_SIMPLETESTSTAT

# Test the function using your solution to Problem 6
my_simpleteststat(coef [1] - 31, se)

# Part E:
# Define a custom function to compute and characterize ols estimates
my_simpleols <- function(y, x) {
  betaest <- my_simplecoef(y, x)
  sebeta <- my_simplese(betaest[1], y, x)
  tstat_p <- my_simpleteststat(betaest[1], sebeta)
  return(c(betaest[1], sebeta, tstat_p[1], tstat_p[2]))
}#MY_SIMPLEOLS

# Test the function using your solution to Problem 6
my_simpleols(wkly_wage , yrs_educ)