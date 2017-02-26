setwd("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/r/")
rm(list = ls())
source("kernal_and_related_functions.R")

#----------------------------------------------Ex 3 Cross Validation -----------------------------------#

#Presumably a good choice of h would be one that led to smaller predictive errors on fresh data. 
#Write a function or script that will: 
#  (1) accept an old ("training") data set and a new ("testing") data set as inputs; 
#  (2) fit the kernel-regression estimator to the training data for specified choices of h; and 
#  (3) return the estimated functions and the realized prediction error on the testing data for each value of h. 

#This should involve a fairly straightforward "wrapper" of the function you've already written.

fit_and_predict_with_kernel <- function(training_data, testing_data, kernel_f=K_gaus, hval=0){
  
  #Learn on TRAINING DATA
  x = training_data[,1]
  y = training_data[,2]
  
  #Predict on Test Data
  x_str = testing_data[,1]                #estimate function value at these points  
  y_str = testing_data[,2]
  
  if(hval == 0){
      #GET OPTIMAL H FOR given kernel_f 
      h_vec = seq(.01,1,by=.01)             #Candidate h values bandwidths to choose from,
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






#PROBLEM A
#Data Generation Same from Curvefitting section
n = 1000
x = runif(n,0,1)    
f = function(x){ x^3  + cos(x) }
v = 1 #variance of desired noise
y = simulate_noisy_data(x,v,f)
x = x - mean(x)
y = y - mean(y)

training_ids = sample(1:n,size=n*.8,replace=F)
training_d =  cbind(x[training_ids],y[training_ids])
testing_d = cbind(x[-training_ids],y[-training_ids])

res <- fit_and_predict_with_kernel(training_d, testing_d)

#attributes(res)
#Look at error compared to h
#plot(res$H,res$pred_err,type='l')



#PROBLEM B
#########------------------------------------------------------
#Set up wiggly and smooth functions.
func_wiggly = function(x) exp(x) + cos(10*pi*x)
func_smooth = function(x) sin(2*pi*x)

#func_wiggly = function(x) sin(10*pi*x)
#func_smooth = function(x) sin(2*pi*x)

n = 500
x = runif(n,0,1)

#Generate noisy and less-noisy data for each fwiggly and fsmooth function.
highly_noisy =  2
not_so_noisy = .3
y_wiggly_noisy  = simulate_noisy_data(x,highly_noisy, func_wiggly)
y_wiggly_lil_noise	= simulate_noisy_data(x,not_so_noisy, func_wiggly)
y_wiggly_actual  = simulate_noisy_data(x,0, func_wiggly)

y_smooth_noisy	= simulate_noisy_data(x,highly_noisy, func_smooth)
y_smooth_lil_noise	= simulate_noisy_data(x,not_so_noisy, func_smooth)
y_smooth_actual  = simulate_noisy_data(x,0, func_smooth)

##show plots of each set
y = matrix(c(y_wiggly_noisy, y_wiggly_lil_noise, y_wiggly_actual, y_smooth_noisy, y_smooth_lil_noise, y_smooth_actual),ncol=6,byrow=F)
colnames(y) = c('wiggly noisy','wiggly not so noisy','wiggly actual', 'smooth_noisy','smooth not so noisy','smooth actual')
par(mfrow=c(2,1))
for(p in 1:2){
  j = p^2
  plot(x,y[,j],main=paste(colnames(y)[j]),xlab='x',ylab='y',ylim=c(-5,5),col=6)   #noisy, black
  points(x,y[,j+1],col=3)     #not so noisy, green,
  points(x,y[,j+2],col=4)     #actual, blue
}

#NOW generate training/test split
training_ids = sample(1:n,size=n*.8,replace=F)
training_d =  cbind(x[training_ids],y[training_ids,])
testing_d = cbind(x[-training_ids],y[-training_ids,])

# For each case, select a bandwidth parameter. 
optimal_h = c(0,0,0,0)	         #Vector to hold optimal h values.
names(optimal_h) = c('wiggly noisy','wiggly not so noisy','smooth_noisy','smooth not so noisy')
yhats = list() 

#for each setup.
for (i in 1:4){
  train_i = training_d[,c(1,i+1)]
  test_i = testing_d[,c(1,i+1)]
  res <- fit_and_predict_with_kernel(train_i, test_i)
  
  optimal_h[i] = res$opt_h
  yhats[[i]] = fit_and_predict_with_kernel(train_i,test_i,K_gaus,optimal_h[i])$yhat
}

#Plot output with fitted data using optimal h values.
par(mfrow=c(2,2))
ns <- names(optimal_h)
x_str <- testing_d[,1]
for (i in 1:4){
  plot(x_str,testing_d[,i+1], main=paste(ns[i],", h=",optimal_h[i],sep=''), xlab='x_train',ylab='y_train')	  #noisy data
  #Overlay fits.
  idx = sort(x_str, index.return = T)$ix
  lines(sort(x_str),yhats[[i]][idx],col='red')     #red fitted line.
  points(sort(x_str),y_wiggly_actual[idx],col=4)   #blue actual
}
