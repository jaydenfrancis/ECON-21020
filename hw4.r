
dat <- read.csv("downloads/bw06.csv")
dat <- as.matrix(dat)

y <- dat[, "birthweight"]
w <- dat[, "cigsdaily"]
x <- cbind(1, dat[, c("boy", "age", "highschool",
                      "somecollege", "college")])
x_tld <- dat[, "married"]

# Part A: 
# BLP(Y|W)-coefficients
beta <- cov(y, w)/var(w)
alpha <-mean(y) - mean(w)*beta
# The treatment W is the estimated number of cigarettes smoked daily by the mother.


# Part B:
# BLP(Y|W,X)
xNew <- cbind(1, dat[, c("boy", "age", "highschool",
                         "somecollege", "college", "cigsdaily")])
XX_inv <- solve(t(xNew) %*% xNew)
XY <- t(xNew) %*% y
betas <- XX_inv %*% XY
betaW = -11.062508
# BetaW is the approximate expected change in birth weight given an additional cigarette smoked

# part C:
# my estimate for beta is different in A and B, in part B we condition on the
# joint distribution of W and X giving us different estimates for the coefficient.

# Part D:
# My interviewer assumed that I had shown a causal relationship
# with my OLS regression, however, this is not the case. What we
# calculated is strictly observational, if we want to try to 
# come to a causal conclusion, we would need to make significant assumptions.

# Part E:
# the selection on observables assumption assumes W is independent of U|X.
# in our case this assumes that conditional on sex of the infant,
# mother's age a birth, whether or not she graduated high school, 
# whether or not she completed some college, whether or not she
# completed college, and whether or not she is married, number of cigarettes
# smoked a day is independent of all other determinants of infant birth weight.

# the common support assumption assumes that supp X|W = supp X,

# Part F:
# given the data that we have, there are certain values for W that
# do not have all possible values in X, so we can see that CS is violated
# with this dataset.

# Problem 7:
# Part A
my_coef <- function(y, X) {
  # Compute and return estimates for beta
  XX_inv <- solve (t(X) %*% X )
  XY <- t(X) %*% y
  return(XX_inv %*% XY)
}#MY_COEF

# Test the function using your solution to Problem 6
coef <- my_coef(y, xNew)
coef

# Part B:

# Define a custom function to compute the blp estimates
my_blp <- function(coef , X) {
  # Compute and return BLP estimates
  return(X %*% coef)
}#MY_BLP

# Test the function
mean(y - my_blp(coef , xNew)) # 0

# Part C:
# Define a custom function to compute the standard error
my_se <- function(coef , y, X) {
  # Compute and return the standard error
  epsilon <- c(y - my_blp(coef, X))
  XX_eps2 <- t(X * epsilon) %*% (X * epsilon)
  Sigma <- XX_inv %*% XX_eps2 %*% XX_inv/n
  return(sqrt(diag(Sigma))/sqrt(n))
}#MY_SE

# Test the function using your solution to Problem 6
se <- my_se(coef , y, xNew)
se

# Part D: