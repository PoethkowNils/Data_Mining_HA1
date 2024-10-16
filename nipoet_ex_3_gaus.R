# Function to generate gaussians from Practice_3
library(mvtnorm)

# function from Practice 3
poethkow_gen_gaus_2d = function(n=200, mean=c(0,0), radiis=c(1,1), alpha=0) {
  
  alpha = alpha * pi/180
  radiis = matrix(c(radiis[1],0,0,radiis[2]),ncol=2)
  angle = matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)),  ncol=2)

  
  C = angle %*% radiis %*% solve(angle)

  ds = rmvnorm(n=n, mean=mean, sigma=C)
  ds
}

# function for 3D dataset
poethkow_gen_gaus_3d <- function(n = 200, mean = c(0, 0, 0), radiis = c(2, 2, 2), alpha = 0) {
    alpha = alpha * pi / 180
    radiis = diag(radiis)  # Create diagonal covariance matrix

    # Rotation matrix for 3D (around z-axis as example)
    angle = matrix(c(cos(alpha), sin(alpha), 0,
                     -sin(alpha), cos(alpha), 0,
                     0, 0, 1), ncol = 3, byrow = TRUE)
    
    C = angle %*% radiis %*% solve(angle)  # Covariance matrix

    ds = rmvnorm(n = n, mean = mean, sigma = C)  # Generate Gaussian data
    return(ds)
}