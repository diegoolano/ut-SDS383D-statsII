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

res <- fit_and_predict_with_linear_smoother(training_d, testing_d,hval = 0, hvec= seq(.01,1,by=.01))  #candidate h's

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
  yhats[[i]] = fit_and_predict_with_linear_smoother(train_i,test_i,K_gaus,optimal_h[i])$yhat
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




#========================================================================
# local polyinomal regression 
#========================================================================
#Problem D
#1) Make function to fit local linear estimator using a Gaussian kernel for a specified choice of bandwidth h. 
#2) Load the data in utilities.csv, which shows the monthly gas bill (in dollars) for a single-family home in Minnesota, 
#   along with the average temperature in that month (in degrees F), and the number of billing days in that month. 
#   Let y be the average daily gas bill in a given month (i.e. dollars divided by billing days), and let x be the average temperature. 
#3) a. Choose a bandwidth by leave-one-out cross-validation.
#   b. Fit y versus x using local linear regression and some choice of kernel. 

#1) see fit_and_predict_with_kernel(training_data, testing_data, kernel_f=K_gaus, hval=0, h_vec=0)
#2) 
utilities = read.csv('/Users/dolano/htdocs/ut-james-scott/statsII/ut-SDS383D-statsII/exercise3/utilities.csv',header=T)
x = utilities$temp  							                 #average temp.
y = utilities$gasbill / utilities$billingdays	     #average daily gas bill for a given month

#Center variables so we don't need a vector of 1's for the intercept
x = x - mean(x)
y = y - mean(y)

plot(x,y)  #see data

#3a 
h_vec = seq(1,10,by=.1)           #Candidate h values. 
pred_err = rep(0,length(h_vec))

for (i in 1:length(h_vec)){
  #get pred error for each h using a local linear regression (D=1) and gaussian kernal
  res = eval_local_poly_loocv(x,y,K=K_gaus,h=h_vec[i],D=1)  
  pred_err[i] = res$loocv_err
}

#plot(h_vec,pred_err)

#Select optimal h and obtain fitted values.
h_opt = h_vec[which.min(pred_err)]            #3.9 optimal h for linear smoother, 6.9 for local linear regression
yhat = local_linear_smoother(x,y,x,h_opt,K_gaus)$yhat

#check data and fit from chosen h
plot(x,y,main=paste('Local Linear Smoothing, Gaussian Kernel, h = ',sep="",h_opt))  
lines(sort(x),yhat[order(x)],col='red',lwd=1) 


###F) LOOK AT RESIDUALS
#Inspect the residuals from the model you just fit. 
#Does the assumption of constant variance (homoscedasticity) look reasonable? If not, do you have any suggestion for fixing it?
residuals = y - yhat
plot(x,residuals)

#It appears the residuals have slightly more variance for lower values of x (temperature) and gets more accurate for higher temps
#sanity check..
l <- lm( utilities$gasbill / utilities$billingdays ~ utilities$temp)
plot( utilities$temp, resid(l))


###G
# Put everything together to construct an approximate point-wise 95% confidence interval for the local linear model 
# (using your chosen bandwidth) for the value of the function at each of the observed points xi for the utilities data. 
# Plot these confidence bands, along with the estimated function, on top of a scatter plot of the data.
# use Gaussian critical values for your confidence set.

RSS = sum(residuals^2)                  #residuals sum of squares
sigma2_hat = RSS / (length(yhat)-1)     #variance is RSS/number of observations - 1
lower = yhat - 1.96*sqrt(sigma2_hat)    #standard error below/above 
upper = yhat + 1.96*sqrt(sigma2_hat)

plot(x,y,main=paste('Local Linear Smoothing 95% Confidence Bands, h = ',sep="",h_opt))  
lines(sort(x),yhat[order(x)],col='red',lwd=1) 
lines(sort(x),lower[order(x)],col='blue',lwd=1,lty=2) 	
lines(sort(x),upper[order(x)],col='blue',lwd=1,lty=2) 
