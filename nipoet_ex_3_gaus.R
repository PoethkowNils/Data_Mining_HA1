# Function to generate gaussians from Practice_3

library(mvtnorm)

# function from Practice 3
poethkow_gen_gaus = function(n=200, mean=c(0,0), radiis=c(1,1), alpha=0) {
  
  alpha = alpha * pi/180
  radiis = matrix(c(radiis[1],0,0,radiis[2]),ncol=2)
  angle = matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)),  ncol=2)

  
  C = angle %*% radiis %*% solve(angle)

  ds = rmvnorm(n=n, mean=mean, sigma=C)
  ds
}