install.packages("nloptr")

set.seed(100)
N <- 100000
K <- 10

X <- matrix(rnorm(N*K, mean = 0), N, K)
X[,1] <- 1

sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)
beta <- c(1.5, -1, -.25, .75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X%*%beta + eps

beta.estimate <- solve(crossprod(X))%*%t(X)%*%Y
# the estimate is pretty close to the true values of beta

# set up a stepsize
alpha <- 0.0000003

# set up a number of iterations
maxiter <- 500000

## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,Y,X) is ", beta, sep = ""))


library(nloptr)
## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8)

## Find the optimum!
res <- nloptr(x0=beta,eval_f=objfun,eval_grad_f=gradient,opts=opts)
print(res)

