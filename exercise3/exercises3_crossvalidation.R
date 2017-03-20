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

#3b.  Select optimal h and obtain fitted values.
h_opt = h_vec[which.min(pred_err)]            #6.9 for local linear regression
res = local_linear_smoother(x,y,x,h_opt,K_gaus)
yhat = res$yhat
resH = res$H

#check data and fit from chosen h
plot(x,y,main=paste('Local Linear Smoothing with Gaussian Kernel, h = ',sep="",h_opt))  
lines(sort(x),yhat[order(x)],col='red',lwd=1) 


###F) LOOK AT RESIDUALS
#Inspect the residuals from the model you just fit. 
#Does the assumption of constant variance (homoscedasticity) look reasonable? If not, do you have any suggestion for fixing it?
residuals = y - yhat
plot(x,residuals)

#It appears the residuals have slightly more variance for lower values of x (temperature) and gets more accurate for higher temps
#sanity check..
#l <- lm( utilities$gasbill / utilities$billingdays ~ utilities$temp)
#plot( utilities$temp, resid(l))



###G
# Put everything together to construct an approximate point-wise 95% confidence interval for the local linear model 
# (using your chosen bandwidth) for the value of the function at each of the observed points xi for the utilities data. 
# Plot these confidence bands, along with the estimated function, on top of a scatter plot of the data.
# use Gaussian critical values for your confidence set.

RSS = sum(residuals^2)                  #residuals sum of squares
sigma2_hat = RSS / (length(yhat) - 1)     #variance is RSS/number of observations - 1
Hat = local_poly_smoother(x,y,x,h=h_opt,K_gaus,D=1)$Hat 

var = rowSums(Hat^2) * sigma2_hat

lower = yhat - 1.96*sqrt(var)    #standard error below/above 
upper = yhat + 1.96*sqrt(var)

idx = sort(x, index.return = T)$ix

plot(x,y,main=paste('Local Linear Smoothing 95% Prediction Interval, h = ',sep="",h_opt))  
polygon(c(sort(x),rev(sort(x))),c(upper[idx],rev(lower[idx])),col='thistle',border=NA)
points(x,y,main=paste('Local Lin Smoothing 95% Confidence Bands, h = ',sep="",h_opt))  
lines(sort(x),yhat[idx],col='red',lwd=1) 
lines(sort(x),lower[idx],col='blue',lwd=1,lty=2) 	
lines(sort(x),upper[idx],col='blue',lwd=1,lty=2) 




#==================================
#GAUSSIAN PROCESSES
#==================================

### Generate gaussian process of 100 points in [0,1].
n = 100
x = sample(seq(0,1,.0001),n,replace=T)
mu_vec = rep(0,length(x))
b = 1
tau1.sq = .1
tau2.sq = 1e-6
hyperparams = c(b,tau1.sq,tau2.sq)

x.se  = gaussian_process(x,mu_vec,cov.fun=cov.se,hyperparams)
x.m52 = gaussian_process(x,mu_vec,cov.fun=cov.m52,hyperparams)

idx = sort(x, index.return = T)$ix
plot(x[idx],x.se[idx],col='blue',lwd=1,type='l')

# Hyperparameter Plotting for Squared Expontentila Covariance Function
### Test 1: Varying b for Sq Exponential and Matern 5/2 Cov Functions.
tau1.sq = .001
tau2.sq = 1e-6

B=c(.05,.1,.25,1,4)
colors = rainbow(length(B))

#Squared exponential plot.
for (i in 1:length(B)){
  fx = gaussian_process(x,params=c(B[i],tau1.sq,tau2.sq),mu=mu_vec,cov.fun=cov.se)
  idx = sort(x, index.return = T)$ix
  if (i==1){ 
    plot(x[idx],fx[idx],lwd=2,type='l',col=colors[i],ylim=c(-.15,.15),main='Squared Exponential Covariance (tau1.sq=.001,tau2.sq=1e-6)')
  } else{
    lines(x[idx],fx[idx],lwd=2,type='l',col=colors[i])
  }
}
legend('topleft',lty=1,lwd=2,legend=paste('b=',B),col=colors)


#Matern 5/2 plot.
for (i in 1:length(B)){
  fx = gaussian_process(x,params=c(B[i],tau1.sq,tau2.sq),mu=mu_vec,cov.fun=cov.m52)
  idx = sort(x, index.return = T)$ix
  if (i==1){
    plot(x[idx],fx[idx],lwd=2,type='l',col=colors[i],ylim=c(-.15,.15),main='Matern 5/2 Covariance (tau1.sq=.001,tau2.sq=1e-6)')
  } else{
    lines(x[idx],fx[idx],lwd=2,type='l',col=colors[i])
  }
}
legend('topleft',lty=1,lwd=2,legend=paste('b=',B),col=colors)

#Making b bigger (while holding everything else ) makes GP vary less
#Making tau1 bigger (while holding everything else ) makes GP everything vary more

#for hyper parameters of Gaussian Processes
#b is period
#tau.1 is amplitude
#tau.2 is noise ( numerical precision regularization term )


#==================================
#NON PARAMETRIC SMOOTHING
#==================================

#PART C
# For the utilities data, plot the pointwise posterior mean and 95% posterior confidence interval 
#    for the value of the function at each of the observed points x_i 
#    (again, superimposed on top of the scatter plot of the data itself). 
# Choose (tau_2)^2 to be very small, say 10^-6, and choose (b, (tau_1)^2) that give a sensible-looking answer

rm(list = ls())
source("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/r/kernal_and_related_functions.R")
utilities = read.csv('/Users/dolano/htdocs/ut-james-scott/statsII/ut-SDS383D-statsII/exercise3/utilities.csv',header=T)
x = utilities$temp    						                 #average temp.
y = utilities$gasbill / utilities$billingdays	
n = length(x)

#Set up hyperparameters.
b = 50
tau1.sq = 5
tau2.sq = 0
params = c(b,tau1.sq,tau2.sq)

#Run prediction with sigma2=1 to estimate residuals.
pred = gp.predict(x,y,x.new=x,cov.fun=cov.se,params=params,sig2=4)
sig2 = sum(y-pred$post.mean)^2/(n-1)  #0.1806997

#Rerun with new estimated sigma2.
pred = gp.predict(x,y,x.new=x,cov.fun=cov.se,params=params,sig2=sig2)

#TODO show how to select tuning parameters

#Vectors to hold posterior mean, var, and CI bounds.
post.mean = pred$post.mean
post.se = sqrt(pred$post.var)
post.ci.lb = post.mean - 1.96*post.se
post.ci.ub = post.mean + 1.96*post.se

#Plot
idx = sort(x, index.return = T)$ix
plot(x,y,col='darkgrey',xlab='x',ylab='y',main='Utilities Data Posterior Mean and 95% CI')
lines(x[idx],post.ci.lb[idx],col='red',lty=2)
lines(x[idx],post.ci.ub[idx],col='red',lty=2)

#Shade confidence bands.
polygon(c(sort(x),rev(sort(x))),c(post.ci.ub[idx],rev(post.ci.lb[idx])),col='lightgrey',border=NA)
points(x,y)
lines(x[idx],post.mean[idx],col='blue')


#=====================
# PART F: WEATHER DATA
#=====================
# In weather.csv you will find data on two variables from 147 weather stations in the American Pacific northwest.
#   pressure : the difference between the forecasted pressure and the actual pressure reading at that station (in Pascals) 
#   temperature : the difference between the forecasted temperature and the actual temperature reading at that station (in Celsius) 

# There are also latitude and longitude coordinates of each station. 
# Fit a Gaussian process model for each of the temperature and pressure variables. 
# Choose hyperparameters appropriately. 
# Visualize your fitted functions (both the posterior mean and posterior standard deviation) 
# on a regular grid using something like a contour plot or color image. 
# Read up on the image, filled.contour, or contourplot17 functions in R. 
# An important consideration: is Euclidean distance the appropriate measure to go into the covariance function? 

rm(list = ls())
source("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/r/kernal_and_related_functions.R")
weather <- read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/ut-SDS383D-statsII/exercise3/weather.csv", header = T)
cols <- rev(colorRampPalette(brewer.pal(10, "RdYlBu"))(100))  #color palette

# Plot pressure & temperature
pressureplot <- ggplot(weather, aes(x=lon, y=lat, colour = pressure)) + 
  geom_point(pch = 15, size = 2) + scale_colour_gradientn(colours=cols) + theme_bw() 

temperatureplot <- ggplot(weather, aes(x=lon, y=lat, colour = temperature)) + 
  geom_point(pch = 15, size = 2) +   scale_colour_gradientn(colours=cols) + theme_bw() 


###
n = nrow(weather)
x = as.matrix(weather[,3:4])

# Fit a GP Model for each of the temp variables and choose Optimal Parameters tau1.sq and b.                   
y = weather$temperature

#Run prediction with sigma2=1 to estimate residuals.  x is n x 2 and y is n x 1
init.params = c(5,1,1,1E-6)
pred = gp.predict(x,y,x.new=x,cov.se.2d, params = init.params,sig2=1)$post.mean
sig2 = sum(y-pred)^2/(n-1)  #3.13

#Choose optimal parameters for model using marginal loglikelihood.
tau2.sq = 1E-6
tau1.sq = .1
b1 = 10
b2= 10
params = c(b1,b2,tau1.sq,tau2.sq)

test.gp = gaussian_process(x,mu=rep(0,nrow(x)),cov.fun=cov.se.2d,hyperparams=params)
test.logl =  gp.logl.y(x,y,mu=rep(0,nrow(x)),cov.fun=cov.se.2d,params=params,sig2=1)


#----------------------------------------------------
#Weather: Grid mesh of parameters to test.
tau2.sq = 0
tau1.sq = seq(.1,15,length=20) 
b = b1 = b2 = seq(.1,2,length=20) 
triplets = expand.grid(b1,b2,tau1.sq,tau2.sq)

#Empty vector to hold marginal log-likelihoods.
ll = rep(0,nrow(triplets))

#Iterate through triplets.
for (k in 1:length(ll)){
  print(length(ll)-k)
  triplet = unlist(triplets[k,])
  ll[k] = gp.logl.y(x,y,mu=rep(0,nrow(x)),cov.fun=cov.se.2d,params=triplet,sig2)
}

#Save optimal triplet of parameters.
max.idx = which.max(ll)
opt.params.temp = unlist(triplets[max.idx,])


#NOW try for pressure.  construct grid of parameters to test.
y = weather$pressure
tau2.sq = 0
tau1.sq = seq(40000,60000,length=50)
b = b1 = b2 = seq(.01,10,length=50)
triplets = expand.grid(b1,b2,tau1.sq,tau2.sq)

#Empty vector to hold marginal log-likelihoods.
ll = rep(0,nrow(triplets))

#Iterate through triplets.
for (k in 1:length(ll)){
  print(length(ll)-k)
  triplet = unlist(triplets[k,])
  ll[k] = gp.logl.y(x,y,mu=rep(0,nrow(x)),cov.fun=cov.se.2d,params=triplet,sig2)
}

#Save optimal triplet of parameters.
max.idx = which.max(ll)
opt.params.pressure = unlist(triplets[max.idx,])


# Temperature & Pressure fitting
lon.range = range(x[,1])
lat.range = range(x[,2])

lon.new = seq(lon.range[1],lon.range[2],length=40)
lat.new = seq(lat.range[1],lat.range[2],length=40)

x.new = as.matrix(expand.grid(lon.new,lat.new))

#Predict temp values.
y = weather$temperature
temp.pred = gp.predict(x,y,x.new,cov.se.2d, params = opt.params.temp,sig2=sig2)$post.mean

#Predict pressure values.
y = weather$pressure
pressure.pred = gp.predict(x,y,x.new,mu=rep(0,nrow(x)),cov.se.2d, params = opt.params.pressure,sig2=sig2)$post.mean


# Temperature plot                                       
pred.values= cbind(x.new,temp.pred)
colnames(pred.values) = c('lon','lat','pred')

z = matrix(temp.pred,byrow=F,nrow=60)

filled.contour(lon.new,lat.new,z,
               key.title = title(main="Temp"),
               color.palette = rainbow,
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(lon.new,lat.new,z,add=T,lwd=2)             
               }
)
mtext(paste('Optimal Parameters: b1 = ',round(opt.params.temp[1],2),
            ', b2 = ',round(opt.params.temp[2],2),
            ', tau1.sq = ', round(opt.params.temp[3],2),
            ', tau2.sq = 0',sep=""),side=3)



# PRESSURE PLOTTING                                           ===
pred.values= cbind(x.new,pressure.pred)
colnames(pred.values) = c('lon','lat','pred')
z = matrix(pressure.pred,byrow=F,nrow=60)
filled.contour(lon.new,lat.new,z,
               key.title = title(main="Pressure"),
               color.palette = rainbow,
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(lon.new,lat.new,z,add=T,lwd=2)             
               }
)
mtext(paste('Optimal Parameters: b1 = ',round(opt.params.pressure[1],2),
            ' b2 = ',round(opt.params.pressure[2],2),
            ' tau1.sq = ', round(opt.params.pressure[3],2),
            'tau2.sq = 0'),side=3)	