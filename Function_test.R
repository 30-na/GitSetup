# This is to practice some basic function writing in R. 


# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon,
# where epsilon is normal zero with variance sigma^2 independent across samples.
# Seed should be set at the beginning of the function
#
# Input:
# X - design matrix, n by p
# beta - given parameter vector, length p
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  set.seed(seed)
  nRow = nrow(X)
  nCol = ncol(X)
  epsilon = rnorm(n=nRow, mean=0, sd=sigma^2)
  Y = X %*% beta + epsilon
  return(Y)
}

library(ggplot2)
library(gridExtra)

# Testing the Function with 4 different condition
X = cbind(rep(x=1, times=100),seq(0,10,length=100))

# B1=1, B_0=0, Sd=1
beta = c(0,1)
sigma=1
y1 = generateY(X,beta,sigma)

# B1=0, B_0=5, Sd=.5
beta = c(5,0)
sigma=0.5
y2 =  generateY(X,beta,sigma)

# B1=0, B_0=1, Sd=0
beta = c(0,1)
sigma=0
y3 = generateY(X,beta,sigma)

# B1=-1.5, B_0=1, Sd=1
beta = c(1,-1.5)
sigma=1
y4 = generateY(X,beta,sigma)

df = data.frame("x"=X[,2], "y1"=y1, "y2"=y2, "y3"=y3, "y4"=y4)

g1=ggplot(df, aes(x=x, y=y1))+
  geom_point()

g2=ggplot(df, aes(x=x, y=y2))+
  geom_point()

g3=ggplot(df, aes(x=x, y=y3))+
  geom_point()

g4=ggplot(df, aes(x=x, y=y4))+
  geom_point()

grid.arrange(g1,g2,g3,g4, ncol=2)
