#----------------------------------------------general kernel helper functions -----------------------------------#
simulate_noisy_data = function(x,v,f){
  #generate noisy normal data for function f and values x with variance v.
  #inputs: 
  #    x is a vector of n values
  #    v is the variance of desired noise
  #    f is a nonlinear function
  #output: 
  #return function f(x) + e  where e is random normal noise
  n = length(x)
  e = rnorm(n,0,sqrt(v))
  #data = f(x)
  #noisy_data = data + e
  #return(noisy_data)
  return(y = f(x)+e)
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

K_unif = function(x){
  #Uniform kernel.
  #inputs:  vector of x values.
  #output:	vector of uniform smoothed x values. 
  return(.5 * ifelse(abs(x)<=1,rep(1,length(x)),rep(0,length(x))))
}



K_gaus = function(x){
  #standard Gaussian kernel.
  #input:	  vector of x values.
  #output:	vector of gaussian smoothed x values.
  return(( 1 / sqrt(2*pi)) * exp(-x^2/2))
}

###########################
#A kernel function K(x) is a smooth function satisyfing
#1) integral of K(x) over the reals is one
#2) integral of xK(x) over the reals is zero
#3) integral of x^2 K(x) over the reals is positive

###########################
#LOOKING AT UNIFORM KERNAL
###########################
#x_input <- rnorm(n = 100, mean = 0, sd = 1)
x_input <- seq(-10,10,length.out = 100001)
plot(x_input,K_unif(x_input))

#1) Integral of K(x) = 1
integrate(K_unif,lower = -Inf, upper = Inf)  #1 with absolute error < 4.2e-11
#or
sum(.0002 * K_unif(x_input))  #1.0001  ( .0002 is size of every bin)

#2) integral of xK(x) = 0
sum(.0002 * x_input * K_unif(x_input))    #4.817246e-16

#3) integral of x^2 K(x) over the reals is positive  .33333
sum(.0002 * x_input^2 * K_unif(x_input))

###########################
#LOOKING AT GAUSSIAN KERNAL
###########################
x_input <- seq(-10,10,length.out = 100001)
plot(x_input,K_gaus(x_input))

#1) Integral of K(x) = 1
integrate(K_gaus,lower = -Inf, upper = Inf)  #1 with absolute error < 9.4e-05
#or
sum(.0002 * K_gaus(x_input))  #1

#2) integral of xK(x) = 0
sum(.0002 * x_input * K_gaus(x_input))    #7.475462e-19

#3) integral of x^2 K(x) over the reals is positive  ..1
sum(.0002 * x_input^2 * K_gaus(x_input))


fit_and_predict_with_linear_smoother <- function(training_data, testing_data, kernel_f=K_gaus, hval=0, h_vec=0){
  #Learn on TRAINING DATA
  x = training_data[,1]
  y = training_data[,2]
  
  #Predict on Test Data
  x_str = testing_data[,1]                #estimate function value at these points  
  y_str = testing_data[,2]
  
  if(hval == 0){
    #TUNE TO FIND OPTIMAL H FOR given kernel_f 
    #from candidate h values bandwidths to choose from in h_vec,
    fhat_str_mat = matrix(rep(0,length(h_vec) * length(x_str)),
                          nrow = length(h_vec), ncol = length(x_str))  #store fhat_str
    
    pred_err_test = rep(0,length(h_vec))
    
    #fit on specified choices of h
    for (i in 1:length(h_vec)){
      fhat_str_mat[i,] = linear_smoother(x,y,x_str,h=h_vec[i],K=kernel_f)
      pred_err_test[i] = sum(fhat_str_mat[i,] - y_str)^2 / (length(y_str))
    }
    optimal_h = h_vec[which.min(pred_err_test)]
    
    #return estimated functions and the realized prediction error on the testing data for each value of h.
    return( list(H=h_vec,fhat=fhat_str_mat,pred_err=pred_err_test,opt_h=optimal_h))
  }
  else{
    #Here we are given a single bandwidth and just need to evaluate for it
    #Calculate predicted points for test data set, and prediction error.
    yhat = linear_smoother(x,y,x_str,hval,kernel_f)
    pred_err_test = sum(yhat-y_str)^2 / (length(x))
    return(list(yhat=yhat, pred_err_test = pred_err_test))
  }  
}



eval_loocv = function(x,y,K=K_gauss,h){
  #Function to use leave one out cross validation to evaulate for a given bandwidth h 
  #data set x,y and kernel K (default to gaussian ) with linear smoother.
  #
  #outputs prediction error for data (loocv_pred_err)
  #  	 and predicted values for data's x vals (yhat)
  
  #Hat 'smoothing matrix'.  {H_ij} = 1/h * K((xi-xj*)/h)
  Hat = matrix(0,nrow=length(x),ncol=length(x)) 
  for (i in 1:length(x)){			                   
    for (j in 1:length(x)){	                    
      Hat[i,j] = (1/h) * K((x[j] - x[i])/h)	
    } 	    
    Hat[i,] = Hat[i,] / sum(Hat[i,])                 #Normalize weights so each row sums to one
  }
  yhat = Hat %*% y                                   #Calculate predicted values.  
  loocv_pred_err = sum(((y-yhat)/(1-diag(Hat)))^2)   #Calculate loocv prediction error. Exercise 3, Pg 4
  
  return(list(yhat=yhat,loocv_err=loocv_pred_err))	
}


local_linear_smoother = function(x,y,x_str,h,K=K_gaus,D=1){
  # Smoothing function for local linear regression on data x,y 
  # for prediction on points x_str for given bandwidth h, kernel K.  #see pg 5 exercises 3
  # outputs  yhat = a scalar or vector of smoothed x values.
  
  n = length(x)                                  #number of observations
  yhat = rep(0,length(x_str))                    #vector of yhats to return
  
  #get var for 
  Hat = matrix(0,nrow=length(x),ncol=length(x)) 
  for (i in 1:length(x)){  		                   
    for (j in 1:length(x)){	                    
      Hat[i,j] = (1/h) * K((x[j] - x[i])/h)	
    } 	    
    Hat[i,] = Hat[i,] / sum(Hat[i,])                 #Normalize weights so each row sums to one
  }
  
  
  for (b in 1:length(x_str)){
    s1 = sum( K((x_str[b]-x)/h) * (x-x_str[b]) )
    s2 = sum( K((x_str[b]-x)/h) * (x-x_str[b])^2 )
    w = K((x_str[b]-x)/h) * (s2 - (x-x_str[b])*s1)        
    yhat[b] = crossprod(w,y) / sum(w)                # sum( w_i y_i ) / sum( w_i)
  }
  
  
  
  return(list(yhat=yhat,H=Hat))
}



local_poly_smoother = function(x,y,x_str,h,K=K_gaus,D=1){
  # Smoothing function for local polynomial regression with inputs.
  #     x covariates.
  #  		x_str  x values for prediction.
  #			h positive bandwidth.
  #			K kernel function.  Default is set to Gaussian kernel.
  #			D degree of polynomial.  Default = 1.
  # outputs	yhat = a scalar or vector of smoothed x values.
  
  n = length(x)	                                    #For i indices.
  yhat = rep(0,length(x_str))	                      #To store function outputs.
  weights.mat = matrix(0,nrow=n,ncol=length(x_str)) #To hold weights for each x-str.
  ahat = matrix(0,ncol=length(x_str),nrow=D+1)
  Hat = matrix(0,length(x_str),n)	                  #Store H.. Consists of row 1 of R times each xstar intercept element.
  
  for (b in 1:length(x_str)){	           #cycle through points to predict
    xstr.i = x_str[b]                    
    W = diag( (1/h) * K((x-xstr.i)/h) )  #Calculate (nxn) weights matrix W with given kernel.
    R = matrix(0,nrow=n,ncol=D+1)        #Set up R matrix.  {R_ij} = (x_i-x)^j for j in 1...D+1.
    
    for (i in 1:n){
      for (j in 1:(D+1)){
        R[i,j] = (x[i]-xstr.i)^(j-1)
      }
    }
    
    RtW = t(R) %*% W                            #Precache t(R) %*% W.
    ahat.xstr = solve(RtW %*% R) %*% RtW %*% y  #Calculate ahat.
    Hat[b,] = (solve(RtW %*% R) %*% RtW)[1,]    #Calculate hat matrix. First row of ahat.xstr, without y.
    yhat[b] = ahat.xstr[1]                      #Estimated function value for point b
    ahat[,b] = ahat.xstr	                      #Save ahat parameters for each x_star.
  }
  return(list(yhat=yhat,weights=weights.mat,ahat=ahat,Hat = Hat))
}

eval_local_poly_loocv = function(x,y,K=K_gaus,h,D){
  #Function to use leave one out cross validation to tune for a given bandwidths h_vec 
  #for data set and kernel K (default to gaussian ) for local polynomial smoother. 
  #if D=1, you are effectively using a local linear smoother
  #
  #outputs prediction error for data (loocv_pred_err)
  #  	 and predicted values for data's x vals (yhat)
  
  x_str = x      #use existing x's as target points for LOOCV.
  
  #Call local_linear_smoother function to obtain yhat for each x point.
  output = local_poly_smoother(x,y,x_str,h,K,D)
  yhat = output$yhat
  Hat = output$Hat
  Hii = diag(Hat)

  loocv_err = sum(((y-yhat)/(1-Hii))^2) #Calculate loocv prediction error.
  
  return(list(yhat=yhat,loocv_err=loocv_err))
}



gaussian_process = function(x,mu,cov.fun,hyperparams){
  #-------------------------------------------------------------
  #Generates realizations from the Gaussian Process 
  #			with mean mu and covariance matrix.	
  #-------------------------------------------------------------
  #in	
  #     x = vector (x1,...,xn) on unit interval [0,1] (2-d for Part F)
  #			params = vector(b,tau1.sq,tau2.sq) of 3 hyperparameters
  #			mu = vector of means, length n.
  #			cov.fun = covariance matrix function.
 	#out
  #fx = vector of realizations from gaussian process.
  #-------------------------------------------------------------
  
  covmatrix = make.covmatrix(x,x,cov.fun,hyperparams)
  
  #Generate realizations f(x1)...f(xn).
  #Require the mvtnorm package for random normal generation.
  require(mvtnorm)
  fx = rmvnorm(1,mu,covmatrix,method="chol")   #use cholesky
  return(fx)
}


gp.predict = function(x,y,x.new,cov.fun,params,sig2=0){
  #	Generates predictions from the noisy Gaussian Process with specified mean and covariance matrix.
  #			Model: y = f(x) + e, with e ~ N(0,sig2*I)
  #
  #INPUTS: 	x = vector (x1,...,xn) 
  #			y = observed GP values at each x.  Can be noisy, or not.
  #			params = vector(b,tau1.sq,tau2.sq) of 3 hyperparameters,
  #				where:
  #				b = length (of period)
  #				tau1.sq = variance of function
  #				tau2.sq = noise
  #			cov.fun = covariance matrix function.
  #			sig2 = variance for noise. 0 predicts for a non-noisy GP.
  #-------------------------------------------------------------
  #OUTPUTS:	post.mean = Posterior mean E(f.new|y) 
  #			post.var = Posterior variance Var(f.new|y)
  #-------------------------------------------------------------
  if(class(x)=='matrix'){
    n = nrow(x)
    n.new = nrow(x.new)
  }else{	
    n = length(x)
    n.new = length(x.new)
  }
  
  #Set up partitioned cov matrices.   
  C  = make.covmatrix(x,x,cov.fun, hyperparams=params)                  
  Cx	= make.covmatrix(x,x.new,cov.fun,hyperparams=params)            
  CxT	= t(Cx)                                          
  Cxx	= make.covmatrix(x.new,x.new,cov.fun, hyperparams=params)        
  
  #Add noise matrix.  (Will be zeros if sig2=0.)
  noise = sig2 * diag(n)                               #n x n
  
  #Calculate posterior means and vars for each predicted value.
  invC_with_noise = solve(C + noise)                       # n x n
  
  CxT_dot_invCn = CxT %*% invC_with_noise                  # n x n %*% n x n = n x n
  post.mean = as.vector(CxT_dot_invCn %*% y )                         # (  n x n %*% n x n %*% n x 1 ) = n x 1    
  post.var = diag( Cxx - Cx %*% invC_with_noise %*% CxT )
  
  return(list(post.mean=post.mean,post.var=post.var))	
}

gp.logl.y = function(x,y,mu,cov.fun,params,sig2=0){
  # Generates values of the marginal log-likelihood p(y)  
  # from a noisy Gaussian Process  with specified mean and covariance matrix.
  #			Model: y = f(x) + e, with e ~ N(0,sig2*I)
  #			Marginal of y: multivariate N(0,sig2I + C)
  #			Logl of Marginal of y: 

  #OUTPUTS:	marg.logl = marginal loglikelihood of y (given x, params)
  require(mvtnorm)
  n = length(mu)
  
  #Build covariance matrix.
  C	= cov.fun(x,x,hyperparams=params)
  sig2I = sig2 * diag(n)
  
  #Calculate and return marginal log-likelihood for y.
  marg.logl = dmvnorm(y,mu,sig2I + C,log=T)
  return(marg.logl)
}



l2norm = function(x){
  #Calculates the Euclidean (l2) norm for a vector x
  # == square root of sum of squares of x
  return(norm=sqrt(sum(x^2)))
}

cov.se = function(x.i,x.j,hyperparams){
  #Computes the (i,j) element for squared exponential cov matrix.
  #INPUTS:	x.i, x.j = two points from two vectors in same space.
  #			      params = vector(b,tau1.sq,tau2.sq) of 3 hyperparameters,
  #OUTPUT:	cov = (i,j) element for squared exponential cov matrix.
  b = hyperparams[1]
  tau1.sq = hyperparams[2]
  tau2.sq = hyperparams[3]
  kd = function(a,b) (a==b)    #Kronecker delta function.
  ed = l2norm(x.i-x.j)         #Euclidean distance for x.i, x.j.
  cov = tau1.sq * exp(-.5 * (ed/b)^2) + tau2.sq * kd(x.i,x.j)	
  return(cov)	
}	

cov.se.2d = function(x.i,x.j,hyperparams){
  #Squared Exponential Covariance Function (non-isotropic distance)
  #			for 2-d x and y matrices.  (Each x, y is a pair of coordinates.)
  #
  #INPUTS:	x,y = two matrices containing pairs of coordinates as rows.
  #			kappa = vector(b1,b2,tau1.sq,tau2.sq) of 4 hyperparameters,
  #				b1,b2 		= length (of period) for each dimension
  #				tau1.sq = variance of function
  #				tau2.sq = noise
  #
  #OUTPUT:	The squared exponential covariance matrix.
  #-------------------------------------------------------------
  b1 = hyperparams[1]
  b2 = hyperparams[2]
  tau1.sq = hyperparams[3]
  tau2.sq = hyperparams[4]
  
  kd = function(a,b){ res = (a==b); return( res[,1] == res[,2])}    #Kronecker delta function for 2-d
  dist = ((x.i[,1] - x.j[,1])/b1)^2 + ((x.i[,2] - x.j[,2])/b2)^2 
  cov = tau1.sq * exp(-.5 * dist ) + tau2.sq * kd(x.i,x.j)
  
  return(cov)	
}	

cov.m52 = function(x.i,x.j,hyperparams){
  #return	cov = (i,j) element for Matern 5/2 cov matrix.
  b = hyperparams[1]
  tau1.sq = hyperparams[2]
  tau2.sq = hyperparams[3]
  kd = function(a,b) (a==b)  #Kronecker delta function.
  ed = l2norm(x.i-x.j)       #Euclidean distance for x.i, x.j.
  cov = tau1.sq * ( 1 + (sqrt(5)*ed / b) + (5/3 * (ed/b)^2)) * exp(-sqrt(5) * ed / b)  + tau2.sq * kd(x.i,x.j)
  return(cov)	
} 

library(stringr)
make.covmatrix = function(x,y,cov.fun,hyperparams=NA){
  #Assemble covariance matrix for a Gaussian Process w specified cov. function.
  #			params = vector(b,tau1.sq,tau2.sq) of 3 hyperparameters,
  if(ncol(x) == 2){
    n1 = nrow(x)
    n2 = nrow(y)
  }
  else{
    n1 = length(x)
    n2 = length(y)
  }
  
  covmatrix = matrix(nrow=n1,ncol=n2)
  for (i in 1:n1){
    for (j in 1:n2){ 
      if(ncol(x) == 2){
        #print(str_c("i: ",i," and j:", j))
        covmatrix[i,j] = cov.fun(matrix(x[i,1:2],ncol=2),matrix(y[j],ncol=2),hyperparams) 
      }
      else{
        covmatrix[i,j] = cov.fun(x[i],y[j],hyperparams) 
      }
    }
  }
  return(covmatrix)
}