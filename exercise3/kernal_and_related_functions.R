#----------------------------------------------general kernel helper functions -----------------------------------#
simulate_noisy_data = function(x,v,f){
  #generate noisy normal data for function f and values x with variance v.
  #inputs: 
  #    x is a vector of n values
  #    v is the variance of desired noise
  #    f is a nonlinear function
  #output: 
  #return function f(x) + e  where e is random normal noise
  n = length(n)
  e = rnorm(n,0,sqrt(v))
  data = f(x)
  noisy_data = data + e
  return(noisy_data)
}

linear_smoother = function(x,y,x_str,h=1,K){
  #linear smoothing function for kernel regression.
  #inputs:
  #     x = a scalar or vector of regression covariates.
  #  		x_star = scalar or vector of new x values for prediction.
  #			h = a positive bandwidth.
  #			K = a kernel function.  Default is set to Gaussian kernel.
  #output:	yhat = a scalar or vector of smoothed x values.
  
  yhat=0
  for (i in 1:length(x_str)){
    w = (1/h) * K((x-x_str[i])/h) #calculate weights from weighting function (pg 3 of ex 3)
    w = w / sum(w)				     	 #normalize so they sum to 1
    yhat[i] = t(w) %*% y         #equivalent to crossprod(w,y) , which equivalent to sum_{i=1}^n{w(x_i,x_st) * y_i} 
  }
  return(yhat)	
}

K_gaus = function(x){
  #standard Gaussian kernel.
  #input:	  vector of x values.
  #output:	vector of gaussian smoothed x values.
  return(( 1 / sqrt(2*pi)) * exp(-x^2/2))
}

