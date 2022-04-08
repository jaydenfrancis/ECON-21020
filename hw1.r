
# Install dependencies (only once)
install.packages("ggplot2")

# Load dependencies (every time)
library(ggplot2)


# Problem 8: 
# part a)
n <- 10000
mu <- 0
sigma <- 1
x <- rnorm(n, mu, sigma)
df1 <- data.frame(x)
hist(x)
ggplot(data=df1, aes(x=x)) + geom_histogram()

# part b)
n <- 10000
min_y <- -1
max_y <- 1
y <- runif(n, min_y, max_y)
df2 <- data.frame(y)
ggplot(data=df2, aes(x=y)) + geom_histogram()

# problem 9:
# part b)
my_rbernoulli <- function(n, p) {
  randuni = runif(n, min = 0, max = 1)
  x <- 1:n
  i = 1;
  for (i in 1:length(x))
  {
    if (randuni[i] < p) {
      x[i] <- 1
    } else {
      x[i] <- 0
    }
  }
  # Return draws
  return(x)
}#MY_RBERNOULLI

# Test the custom Bernoulli generator function
x <- my_rbernoulli (10000 , 0.5)
length(x) == 10000 # should return TRUE
mean(x) # should a number near 0.5

