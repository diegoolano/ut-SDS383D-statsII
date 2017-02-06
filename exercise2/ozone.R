#Problem 4. Quantifying uncertainty: some basic frequentist ideas
#B.

# Load the library
# you might have to install this the first time
#install.packages("mlbench")
library(mlbench)

# Load the data
ozone = data(Ozone, package='mlbench')

# Look at the help file for details
?Ozone

# Scrub the missing values
# Extract the relevant columns 
ozone = na.omit(Ozone)[,4:13]

head(ozone)

y = ozone[,1]
x = as.matrix(ozone[,2:10])

# add an intercept ( vector of ones to the left of matrix x)
x = cbind(1,x)

# compute the estimator  Bhat = (X^t * X)^-1 * X^T * Y  
betahat = solve(t(x) %*% x) %*% t(x) %*% y

  #note this  t(x) %*% x   can be done more efficiently with  crossprod(x)

# Fill in the blank
y_hat = x %*% betahat
sigma_hat2 =  sum((y - (y_hat))^2) / ( (nrow(x) - ncol(x)) )   #calculate variance of y versus yhat
          
#Plug in sigma2_hat to obtain beta cov matrix estimate.
betacov =  (sigma_hat2) * solve(t(x) %*% x)         

#standard error is the square root of the diagnol of the covariance matrix
standard_error = sqrt(diag(betacov))
round(standard_error,6)
#       V5        V6        V7        V8        V9       V10       V11       V12       V13 
#38.328694  0.007251  0.174143  0.023769  0.069299  0.124714  0.000394  0.014777  0.119292  0.004896 

# Now compare to lm
# the 'minus 1' notation says not to fit an intercept (we've already hard-coded it as an extra column)
lm1 = lm(y~x-1)

summary(lm1)
betacovlm = vcov(lm1)
round(sqrt(diag(betacovlm)),6) #to six decimals
#        x       xV5       xV6       xV7       xV8       xV9      xV10      xV11      xV12      xV13 
#38.328694  0.007251  0.174143  0.023769  0.069299  0.124714  0.000394  0.014777  0.119292  0.004896


http://cns-gdc7514 ( 172.16.185.162 )
#SKIM  mac app

#Boot Strap Notes from class by Spencer Woody / James

#1st method is doing bootstrap on residuals (residual sampling)
#2nd is using samples of pairs (xi, yi) to get new y*   <-- Better APPROACH  (more intutitive) (just samples xs and ys ).. TODO.. do this  (nonparametric sampling) .. though this can kill design

#and 3rd, parametric bootstrap ( estimate parametric variance and then generate a new model and sample residuals )
# Ei ~ F
# var(Ei) = sigma^2

#for log(sigma^2(x)) = g(x)

#using just xi, yi pairs is called nonparametric sampling.
#parametric forumla or residual way will not give you good valid standard errors if not iid ( if standard deviation is not the same for all points, if there is heteroskedasity) 

#to have valid linear models (ie, standard errors)
#0) linearity
#1) independence of residuals
#2) homoskedasticity ( constant variance )
#3) normality 

#parametric sampling approach needs 0 to 2 

#* var(Bhat) = sigma^2(X^TX)-1

#REMEMBER:   XtX <- crossprod(X)
#Remember XtXinv <- solve(XtX)

#crossprod(X,y) gives =>    t(X) %*% y 


#Problem 6. Bootstrapping  A

#Let Shat denote the covariate matrix of the sampling distribution of Bhat, the least-squares estimator. 
#Write an R function that will estimate Shat via bootstrapped resampling for a given design matrix X and response vector y. 

#run B bootstraps of the beta least-squares coefficients on matrix X with response vector y to simulate covariate matrix of Bhat
#Bhat = (X^T * X)-1 * X^T * Y
#Shat = cov(B) = σ2(XT X)−1

bootstrapped_betacov = function(X,y,B){  
  n = nrow(X)		
  p = ncol(X)		  
  betahat_boot = matrix(0,nrow=B,ncol=p)   #each row is a beta bootstrap sample of beta_j's.
  
  xtx_inv_xt = solve(t(X) %*% X) %*% t(X)
  beta_hat = xtx_inv_xt %*% y
  yhat = X %*% beta_hat
  e = y - yhat
  
  #Bootstrap residuals only  X as fixed.  ****
  for (b in 1:B){
    e_boot = e[sample(1:n,n,replace=T)]	      #Choose observation residual's to sample with replacement
    y_boot = yhat + e_boot			              #calculate new y
    betahat_boot[b,] = xtx_inv_xt %*% y_boot  #Calculate bootstrapped beta coefficients.
  }
  
  #now calculate the covariance matrix of our sample
  beta_hat_cov = matrix(0,nrow=p,ncol=p)
  for (i in 1:p){
    for (j in 1:p){
      beta_hat_cov[i,j] = cov(betahat_boot[,i], betahat_boot[,j])
    } 
  } 
  
  return(beta_hat_cov)	
} 

#Use it to compute Shat for the ozone data set, and compare it to the parametric estimate based on normal theory.
bootstrapped_betacov_instance = bootstrapped_betacov(x,y,B=100000)
bootstandard_error = sqrt(diag(bootstrapped_betacov_instance))

#show boot standard error compared to parametric one from before
round(bootstandard_error,6)
#[1] 37.445669  0.007086  0.170102  0.023274  0.067518  0.121417  0.000385  0.014390  0.116354  0.004764
round(standard_error,6)
#38.328694  0.007251  0.174143  0.023769  0.069299  0.124714  0.000394  0.014777  0.119292  0.004896


#Problem 6. Bootstrapping  B1
#Create an algorithm to generate a multivariate normal random variable given mean vector mu of length n and covariance matrix S

#From the Multivarite Normal section, problem E, 
#If X ~ N(mu,Sigma) is a multivariate normal random variable, 

#we showed X can be written as an affine transformation X = Lz + mu of iid standard normal random variables z = (z_1,...,z_n)^T.

#Additionally, Since S is positive semi-definite, we can write S = LL^T .

generate_multivariate_normal_random_variable = function(mu,Sigma){
  n = length(mu)
  #1. Generate n standard normal univariate random variables and store in vector z.  #standard normal (mean = 0, variance = 1)
  z = rnorm(n,0,1)
  #2. Using Cholesky decomposition (which is faster though not stable to zero vectors compared with eigen decomposition ) 
  #   rewrite covariance matrix S as LLT.
  #   for Cholesky, t(chol(Sigma)) %*% chol(Sigma) = Sigma = (L)(L^T),  and then verify: L %*% t(L)  = Sigma
  L = t(chol(Sigma))
  
  #3. Then make the multivariate normal distribution using x = Lz + μ.
  x = L %*% z + mu
  x
}

#mu = rnorm(2,mean = 4,sd = 2)
mu = c(3,7)
Sigma = matrix(c(2, 4, 4, 12),nrow = 2,ncol = 2)
x = generate_multivariate_normal_random_variable(mu,Sigma)
x   #single generated multivariate normal 
#         [,1]
#[1,] 1.117451
#[2,] 5.886651


#Problem 6. Bootstrapping  B2
#For a given sample x1 , ..., xN from a multivariate normal distribution, 
#estimate the mean vector and covariance matrix by maximum likelihood. 
#Remember to work on a log scale. R has many possibilities for optimization; the built-in function optim usually works for me.

multivariate_normal_maximum_likelihood_estimator = function(xvec){
  #given xvec: x1...xn ~ mvn(mu,Sigma), estimate mu and Sigma using mle estimates.

  #log likelihood function is sum ( 0.5 * (xvec - theta[1])^2 / theta[2] + 0.5 * log(theta[2]) )
  fn <- function(theta) { sum ( 0.5*(xvec - theta[1])^2/theta[2] + 0.5* log(theta[2]) ) }

  #get minimum of negative of likelihood, 
  theta <- c(0,1)  #assume standard normal dist (mu = 0, and sigma = 1) as starting point
  est <- optim(theta, fn, hessian=TRUE)
  
  #note: The optim optimizer is used to find the minimum of the negative log-likelihood. 
  #      An approximate covariance matrix for the parameters is obtained by inverting the Hessian matrix at the optimum.
  mu_hat <- est$par[1]
  sigma_hat <- est$par[2]
  log_like_val <- est$value
  return(c(mu_hat,sigma_hat,log_like_val))
}



library(MASS)
library(stringr)
n = 1000
mu = c(3,7)
Sigma = matrix(c(2, 4, 4, 12),nrow = 2,ncol = 2)
x = mvrnorm(n=n,mu=mu,Sigma=Sigma)

est = multivariate_normal_maximum_likelihood_estimator(x)  #Output MLE estimates mu_hat, Sigma_hat.
print(str_c("muhat = ",round(est[1],3)," & sigmahat = ", round(est[2],3), " & loglikelihood = ", round(est[3],3) ))
#"muhat = 5.542 & sigmahat = 7.778 & loglikelihood = 3051.552"

#Problem 6. Bootstrapping  B3. 

#Bootstrap a given sample x1, . . . , xN to estimate the sampling distribution of the MLE.
#using x ~ MVN, p from last time
p = length(mu)
B = 10000  						      #num samples.
mu_boot = matrix(rep(0,p*B),p,B)		#array to hold mu vectors.. p x B 
sig_boot = array(0,c(p,p,B))	#array to hold cov matrices p x (p x B)

for (b in 1:B){
  xb = x[sample(1:n,size=n,replace=T),]	      #sample obs from x with replacement.  
  mu_boot[,b] = colMeans(xb)                    #calculate mu for sample and store
  sig_boot[,,b] = cov(xb)                       #calculate cov matrix for sample and store
}

mu_boot_mean = rowMeans(mu_boot)
sig_boot_mean = apply(sig_boot,c(1,2),mean)

mu_boot_mean
#[1] 3.005697 7.037660

sig_boot_mean
#         [,1]      [,2]
#[1,] 1.977642  3.972727
#[2,] 3.972727 12.230527

#library(xtable)
#toLatex method



#B1

#instead of Cholesky you can also do, 
#via Spectral Decomposition: A = V diag(lambda) V^(-1)
#eg = eigen(Sigma)    	 #Store spectoral value decomposition of Sigma.
#V = eg$vectors				   #Extract eigen vectors.
#lam = diag(eg$values)	 #Extract diagonal matrix of eigenvalues.
#V %*% lam %*% solve(V)	 #Check reproducing Sigma.
#L = V %*% sqrt(lam)		 #Assign L so LL^T = Sigma	
#L %*% t(L)					     #Verify LL^T = Sigma


#B2  #http://www.ms.uky.edu/~mai/sta321/MLEexample.pdf



#FOR LOOKING AT INTERNAL CODE FOR R PACKAGES
methods(class="lm")
[1] add1           alias          anova          case.names     coerce         confint        cooks.distance deviance       dfbeta         dfbetas        drop1          dummy.coef    
[13] effects        extractAIC     family         formula        hatvalues      influence      initialize     kappa          labels         logLik         model.frame    model.matrix  
[25] nobs           plot           predict        print          proj           qr             residuals      rstandard      rstudent       show           simulate       slotsFromS3   
[37] summary        variable.names vcov     

methods("vcov")
getAnywhere(vcov.lm)
'''
A single object matching ‘vcov.lm’ was found
It was found in the following places
registered S3 method for vcov from namespace stats
namespace:stats
with value

function (object, ...) 
{
  so <- summary.lm(object)
  so$sigma^2 * so$cov.unscaled
}
<bytecode: 0x7fa0d107d6a0>
  <environment: namespace:stats>
'''