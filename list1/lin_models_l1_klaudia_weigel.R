## Exercise 1
set.seed(9)
library(mvtnorm)
par(mfrow = c(1,2))
X = matrix(rnorm(100 * 2, 0, 1), ncol = 2)
plot(X, pch = 19, col = 'coral')
X_2 = rmvnorm(100, mean=c(0,0), sigma = diag(2))
plot(X_2, pch = 19, col = 'lightblue')

## Exercise 2
affine_transformation =  function(X, A, b) {
  return(A %*% X + b)
}

apply_and_plot = function(X, Sigma, b) {
  par(mfrow = c(1,2), mgp=c(3,0.5,0),mar=c(5,4,4,2)+0.1)
  # chol finds an upper traingular matrix, so we have to transpose it
  A = t(chol(Sigma)) # Sigma = AA^T
  
  # Apply an affine transformation to the original cloud of points
  Y = t(apply(X, MARGIN = 1, FUN = affine_transformation, A = A, b = b))
  plot(Y, xlab = "Y1", ylab = "Y2", pch=19, 
       cex.main = 0.5, cex.lab = 0.5, cex.axis = 0.5,
       col="coral", main = "Affine transformation")
  
  # Check the results using the rmvnorm function
  Y_check = rmvnorm(n=100, mean = b, sigma = Sigma)
  plot(Y_check, xlab = "Y1", ylab = "Y2", pch=19,  
       cex.main = 0.5, cex.lab = 0.5, cex.axis = 0.5,
       col="lightblue", main = "Rmvnorm check")
}

#### (a)

S_a = matrix(c(1, 0.9,  0.9, 1), nrow = 2, byrow = TRUE)
b = c(4, 2)
# Two plots side by side
par(mfrow = c(1,2))
apply_and_plot(X, S_a, b)

#### (b)
S_b = matrix(c(1, -0.9,  -0.9, 1), nrow = 2, byrow = TRUE)
b = c(4, 2)
par(mfrow = c(1,2))
apply_and_plot(X, S_b, b)

#### (c)
S_c = matrix(c(9, 0,  0, 1), nrow = 2, byrow = TRUE)
b = c(4, 2)
par(mfrow = c(1,2))
apply_and_plot(X, S_c, b)


## Exercise 3
n = 200 # number of random vectors
m = 100 # dimension of the distribution
X = matrix(rnorm(200 * 100, 0, 1), ncol = 100)
Sigma = diag(nrow = 100, ncol = 100)
Sigma[lower.tri(Sigma)] = 0.9
Sigma[upper.tri(Sigma)] = 0.9
# We don't transpose because the transformation is XA
A = chol(Sigma)
X_tilde = X %*% A

cov_matrix = cov(X_tilde)
par(mfrow = c(1,2))
hist(diag(cov_matrix),
     cex.main = 0.6, cex.lab = 0.5, cex.axis = 0.5,
     main = "Sample variance")
hist(cov_matrix[upper.tri(cov_matrix)],
     cex.main = 0.6, cex.lab = 0.5, cex.axis = 0.5,
     main = "Sample covariance")
mean(diag(cov_matrix))
mean(cov_matrix[upper.tri(cov_matrix)])


# check, solution that was presented during the lab
S = matrix(rep(0.9), nrow = m, ncol = m) + diag(x=0.1, nrow = m, ncol = m)
A_nm = t(chol(S))
#X = rnorm(n * m, 0, 1)
#X_nm = matrix(X_1, nrow = m)
X_nm = t(X)
Y_nm = A_nm %*% X_nm

sample_m = apply(Y_nm, 1, mean)
sample_var = apply(Y_nm, 1, var)
sample_cov = c()
for (i in 1:(m-1)) {
  for(j in (i+1):m) {
    sample_cov = append(sample_cov, cov(Y_nm[i,], Y_nm[j,]))
  }
}
hist(sample_var)
hist(sample_cov)