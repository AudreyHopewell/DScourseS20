# set up a stepsize
alpha <- 0.00003

# set up a number of iterations
maxiter <- 500000

## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

## Closed-form solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))


set.seed(100)
# set up a stepsize
alpha <- 0.0003

## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*X%*%(y-t(X)%*%beta)) )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),iter)

# stochastic gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-12) {
  # Randomly re-order the data
  random <- sample(nrow(X))
  X <- X[random,]
  y <- y[random]
  # Update parameters for each row of data
  for(i in 1:dim(X)[1]){
    beta0 <- beta
    beta <- beta0 - alpha*gradient(beta0,y[i],as.matrix(X[i,]))
    beta.All[,i] <- beta
  }
  alpha <- alpha/1.0005
  if (iter%%1000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

## Closed-form solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))