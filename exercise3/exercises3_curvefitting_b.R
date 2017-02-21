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
  #			x_star = scalar or vector of new x values for prediction.
  #			h = a positive bandwidth.
  #			K = a kernel function.  Default is set to Gaussian kernel.
  #output:	yhat = a scalar or vector of smoothed x values.
  
  yhat=0
  for (i in 1:length(x_str)){
    w = (1/h) * K((x-x_st[i])/h) #calculate weights from weighting function (pg 3 of ex 3)
    w = w / sum(w)				     	 #normalize so they sum to 1
    yhat[i] = t(w) %*% y         #equivalent to crossprod(w,y) , which equivalent to sum_{i=1}^n{w(x_i,x_st) * y_i} 
  }
  return(yhat)	
}


K_unif = function(x){
  #Uniform kernel.
  #inputs:	vector of x values.
  #output:	vector of uniform smoothed x values. 
  return(.5 * ifelse(abs(x)<=1,rep(1,length(x)),rep(0,length(x))))
}

K_gaus = function(x){
  #standard Gaussian kernel.
  #input:	  vector of x values.
  #output:	vector of gaussian smoothed x values.
  return(( 1 / sqrt(2*pi)) * exp(-x^2/2))
}



#----------------------------------------------Curve fitting by linear smoothing - Part B -----------------------------------#

run_curve_fitting_B <- function(){
  #part A
  #1) simulate noisy data from some nonlinear function, y = f (x) + E; 
  x = runif(1000,0,1)	  
  f = function(x) x^5 + x^3	+ cos(x)
  v = 1 #variance of desired noise
  y = simulate_noisy_data(x,v,f)
  
  #2) subtract the sample means from the simulated x and y; 
  x = x - mean(x)
  y = y - mean(y)
  
  #look at noisy data
  #plot(x,y,main='Noisy data',xlab='x',ylab='y')
  
  #3) and use your function to fit the kernel smoother for some choice of h. 
     #bias/variance tradeoff.  
     #  high h = low variance (but high bias)
     #  low  h = low bias ( but high variance)
  
  #try linear smoother on single bandwidth and gaussian kernel
  x_str = seq(min(x),max(x),by=.001)  #estimate function value at these points
  fhat_str = linear_smoother(x,y,x_str,h=.5,K=K_gaus) 
  
  #plot(x,y,main='Noisy data',xlab='x',ylab='y')
  #lines(x_str,fhat_str,col='red',type="l")
  
  #PART B
  #Plot the estimated functions for a range of bandwidths large enough to yield 
  #noticeable changes in the qualitative behavior of the prediction functions.
  h_vec = c(.01,.1,.25,.5,1,5,10,30)     #bandwidths to choose from,
  fhat_str_mat = matrix(rep(0,length(h_vec) * length(x_str)),
                        nrow = length(h_vec), ncol = length(x_str))  #store fhat_str
  for (i in 1:length(h_vec)){
    fhat_str_mat[i,] = linear_smoother(x,y,x_str,h=h_vec[i],K=K_gaus)    
  }
  
  palette <- colorRampPalette(c("green","blue"))(length(h_vec))
  plot(x,y,main='predicting f_hat* on nonlinear noisy data with\n a gaussian smoothing kernel and various bandwidths',xlab='x',ylab='y')
  for(i in 1:length(h_vec)){
    lines(x_str,fhat_str_mat[i,],col=palette[i],lwd=2)
  }
  legend('bottomright',legend=h_vec,lwd=5,col=palette)  
  
  #saved to exercise3_fitting_curves_b.pdf
}

run_curve_fitting_B()
