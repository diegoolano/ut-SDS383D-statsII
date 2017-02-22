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

fit_and_predict_with_kernel <- function(training_data, testing_data){
  
  #Learn on TRAINING DATA
  x = training_data[,1]
  y = training_data[,2]
  
  #Predict on Test Data
  x_str = testing_data[,1]                #estimate function value at these points  
  y_str = testing_data[,2]
  h_vec = seq(.01,1,by=.01)             #Candidate h values bandwidths to choose from,
  fhat_str_mat = matrix(rep(0,length(h_vec) * length(x_str)),
                        nrow = length(h_vec), ncol = length(x_str))  #store fhat_str
  
  pred_err_test = rep(0,length(h_vec))
  
  #fit on specified choices of h
  for (i in 1:length(h_vec)){
    fhat_str_mat[i,] = linear_smoother(x,y,x_str,h=h_vec[i],K=K_gaus)
    pred_err_test[i] = sum(fhat_str_mat[i,] - y_str)^2 / (length(y_str))
  }
  
  #return estimated functions and the realized prediction error on the testing data for each value of h.
  return( list(H=h_vec,fhat=fhat_str_mat,pred_err=pred_err_test))
  
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
testing_d = cbind(x[-test_idx],y[-test_idx])

res <- fit_and_predict_with_kernel(training_d, testing_d)

#attributes(res)
#Look at error compared to h
plot(res$H,res$pred_err,type='l')


#PROBLEM B
#HERE