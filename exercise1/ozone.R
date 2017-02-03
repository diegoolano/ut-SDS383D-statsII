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

#library(xtable)
#toLatex method

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