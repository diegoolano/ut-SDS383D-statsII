library(dplyr)
library(ggplot2)
library(mosaic)
library(lme4)  # will also need to have Matrix and lattice installed

###EXERCISE 4 Hierarchical models: data-analysis problems
#Math tests
# The data set in mathtest.csv 
# shows the scores on a standardized math test from a sample of 10th-grade students at 100 different U.S. urban high schools, 
# all having enrollment of at least 400 10th-grade students. 

# Let:
#     θi be the underlying mean test score for school i, and 
#   y_ij be the score for the jth student in school i. 

# Starting with the “mathtest.R” script, 
# you’ll notice that the extreme school-level averages ȳi (both high and low) tend to be at schools where fewer students were sampled.

#1. Explain briefly why this would be.  
#   ---  These schools have less data points and thus have the potential for higher variance, and we have less confidence in their estimates generally.

#2. Fit a normal hierarchical model to these data via Gibbs sampling: 
#             y_ij ∼ N(θi, σ2) 
#               θi ∼ N( μ, τ2σ2)
#   Decide upon sensible priors for the unknown model parameters (μ, σ2, τ2).
#  SEE STEP 2 BELOW

#3. Suppose you use the posterior mean θ_hat_i from the above model to estimate each school-level mean θi. 
#   Define the shrinkage coefficient κi as
#      κ_i = ( ȳ_i − θ_hat_i )/ ȳ_i
#   which tells you how much the posterior mean shrinks the observed sample mean. 

#   Plot this shrinkage coefficient (in absolute value) for each school as a function of that school’s sample size, and comment.

#STEP Zero Look at Math Tests and model with just lm, nopooling/pooling and then hlm libray

mtests = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/mathtest.csv", header=TRUE)
lvs <- mtests %>% group_by(school) %>% summarise(n = n()) %>% arrange(-n)
mtests$school <- factor(mtests$school, levels = lvs$school)
dim(mtests)  #1993 x 2
length(unique(mtests$school)) #100 schools
summary(mtests,maxsum = 100)  #school with most tests school37 has 32 scores, and school67 has 4

#show observations per school
ggplot(lvs,aes(school,n)) + geom_point() + ggtitle("num observations per school") + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

ggplot(mtests,aes(school,mathscore)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))


#show means per school
boxplot(mathscore ~ school, data=mtests)

#get some stats per group
#xtabs( mathscore ~ school, data=mtests)  #this gives the sum per school
#mtests %>% filter(school == 37) %>% summarise(n = n(), m = mean(mathscore), s = sum(mathscore))  # for a single school

# for all schools 
group_stats <- mtests %>% group_by(school)  %>% summarise(n = n(), mu = mean(mathscore), sd = sd(mathscore), se = sd/sqrt(n))  
head(group_stats,n=4)
# school     n       mu        sd       se
#     37    32 46.08125  8.651143 1.529321
#      1    31 50.81355 11.250250 2.020604
#     98    31 52.97097 10.466526 1.879844
#     12    30 50.52700 13.404839 2.447378

# Try linear model with no pooling ( coefficients for every single school)
lm0 = lm(mathscore~school+1, data=mtests)
lm0.summary <- summary(lm0)
head(lm0.summary$coefficients,n=4)            #this treats 37 as the base case, 
#              Estimate Std. Error    t value      Pr(>|t|)
# (Intercept) 46.081250   1.625333 28.3518866 1.111337e-147
# school1      4.732298   2.317030  2.0423981  4.125035e-02
# school98     6.889718   2.317030  2.9735121  2.981327e-03
# school12     4.445750   2.336563  1.9026878  5.723290e-02

lm1 = lm(mathscore~(school-1), data=mtests)   #remove ones to remove intercept and show each group explicitly 
lm1.summary <- summary(lm1)
head(lm1.summary$coefficients,n=4)   
#          Estimate Std. Error  t value      Pr(>|t|)
# school37 46.08125   1.625333 28.35189 1.111337e-147
# school1  50.81355   1.651340 30.77111 6.008080e-169
# school98 52.97097   1.651340 32.07757 1.123050e-180
# school12 50.52700   1.678636 30.10003 5.506606e-163

just37data = mtests %>% filter(school==37)
lm.37 = lm( mathscore ~ 1 , data=just37data)   #just looking at 37
display(lm.37)
'''
lm(formula = mathscore ~ 1, data = just37data)
            coef.est  coef.se
(Intercept) 46.08     1.53  

n = 32, k = 1
residual sd = 8.65, R-Squared = 0.00
'''
#Now do with complete pooling ( treat all as one school)
tmp <- mtests
tmp$school = 1
lm2 = lm(mathscore~school, data=tmp)
summary(lm2)   #so just guess overall mean

mu_hat_overall <- mean(mtests$mathscore)  #48.07 overall average score
mu_hat_j <-  mean(mathscore ~ school, data=mtests)
sigma_hat <- sd(mathscore ~ school, data=mtests)
sample_size <- tally( ~ school, data=mtests)

# plot naive standard errors
standard_errors = sigma_hat/sqrt(sample_size)
plot(standard_errors) 

#plot ordered by schools with most observations to least to show groups with less observations have higher variation/standard error
plot(lvs$school,standard_errors,xlab = 'school', ylab = 'standard error', main = 'schools ordered from most # of test scores to least # of test scores ')
lines(lvs$school,standard_errors)


# A mixed-effects model with hierarchical structure for school
hlm1 = lmer(mathscore ~ (1 | school), data=mtests)
summary(hlm1)  
'''
Linear mixed model fit by REML ['lmerMod']
Formula: mathscore ~ (1 | school)
   Data: mtests

REML criterion at convergence: 14685

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.8433 -0.6630 -0.0216  0.6573  4.4035 

Random effects:
 Groups   Name        Variance Std.Dev.
 school   (Intercept) 23.20    4.817   
 Residual             84.73    9.205   
Number of obs: 1993, groups:  school, 100

Fixed effects:
            Estimate Std. Error t value
(Intercept)  48.1050     0.5283   91.06
'''

head(coef(lm1))   #no-pooling 
#            school37  school1 school98 school12 school26 school90 
#            46.08125 50.81355 52.97097 50.52700 46.61300 48.58667 

t(head(coef(hlm1)$school))  #hlm 
#                  37        1       98       12       26      90
#(Intercept) 46.28854 50.52812 52.45818 50.26418 46.77491 48.5344

#No pooling and HLM similar for groups with many observations


tail(coef(lm1))   #no-pooling
#            school11 school53 school94 school17 school82 school67 
#            57.94818 42.54909 46.60750 37.92714 38.76400 65.01750 
t(tail(coef(hlm1)$school))  #hlm 
#                  11       53       94      17        82      67
#(Intercept) 55.49494 43.93382 47.07684 41.4164  42.70665 56.9462

#Lookin at the groups with Less Observations though we can see the difference !!

fixef(hlm1)  #overal mu
ranef(hlm1)	 #


#FOR SCHOOL 37
fixef(hlm1) + ranef(hlm1)$school[1,]  #.. 46.28854   which is the coefficient of the HLM for school 37


####TRAINING/TESTING for nopooling, complete pooling and partial 
# Generate some predictions on the hold-out data
mtests.train.id <- sample(x= seq(1,nrow(mtests)), size = .8 * nrow(mtests), replace = F)
mtests.train <- mtests[mtests.train.id,]
mtests.test <- mtests[-(mtests.train.id),]

lm.nopool = lm(mathscore~school, data=mtests.train)

pred1 = predict(lm.nopool, mtests.test)
plot(pred1, mtests.test$mathscore)
abline(0,1)


hlm2 = lmer(mathscore ~ (1 | school), data=mtests.train)
pred2 = predict(hlm2, mtests.test)


# The simple group-wise model predicts better than just grandmean
grandmean = mean(mtests.train$mathscore)
sum( (mtests.test$mathscore - pred1)^2 )            #sum of squared errors using no pooling: 34151.31
sum( (mtests.test$mathscore - grandmean)^2 )        #sum of squared errors using no school info (complete pooling ): 40291.48
sum( (mtests.test$mathscore - pred2)^2 )            #sum of squared errors using hlm : 33674.64




####NOW GIBBS SAMPLING, Math Tests pt 2
#2. Fit a normal hierarchical model to these data via Gibbs sampling: 
#             y_ij ∼ N(θi, σ2) 
#               θi ∼ N( μ, τ2σ2)
#   Decide upon sensible priors for the unknown model parameters (μ, σ2, τ2).

#  This is a normal-normal conjugate model:
#	theta_i | ybar_i, τ2, σ2 ~ N(mu.new,var.new)
#		where 
#	precisions add, and
#	posterior mean is precision-weighted average of data and prior mean.

#	tau^2 acts as a signal-to-noise ratio.




gibbs.mathtest = function(y,x,iter=11000,burn=1000,thin=2){
  #-------------------------------------------------------------
  #FUNCTION: 	Gibbs Samler for math test hierarchical normal model.
  #			Unknown parameters: (tau.sq, sig.sq, mu)	
  #-------------------------------------------------------------
  #MODEL:		Hierarchical model:        i indexes schools and j indexes scores
  #			   y_ij ~ N(theta_i, sig.sq)
  #			theta_i ~ N(mu, tau.sq * sig.sq)
  #
  #		       mu ~ I(mu) ie indicator for real line (flat prior)
  #			 sig.sq ~ 1/sig.sq * I(sig.sq) ie jeffreys prior
  #			 tau.sq ~ I(tau.sq) ie indicator for pos real line (flat prior)
  #-------------------------------------------------------------
  #INPUTS: y = vector of y_ij math test scores
  #			   x = vector of schools
  #			iter = number of posterior samples to generate.
  #			burn = number of burn-in values to discard.
  #			thin = thinning to reduce autocorr of chain.
  #-------------------------------------------------------------
  #OUTPUTS:	
  #      theta.post = matrix of posterior theta_i samples (rows = samples)
  #		    	mu.post = vector of posterior mu samples
  #			sig.sq.post = vector of posterior sig2.sq samples
  #			tau.sq.post = vector of posterior tau2.sq samples
  #-------------------------------------------------------------
  
  n = length(y)						             	#Number of observations.
  ni = aggregate(y, list(x), length)$x	#Sample sizes for each group.
  p = length(unique(x))				        	#Number of groups.
  ybar = aggregate(y, list(x), mean)$x	#Sample group means.
  
  #Set up data structures to hold Gibbs samples from posteriors.
   theta = matrix(0,iter,p)	  #Each row is a p-length Gibbs sample.
      mu = rep(0,iter)				#Vector of posterior mu samples
  sig.sq = rep(0,iter)		  	#Vector of posterior sig2.sq samples
  tau.sq = rep(0,iter)			  #Vector of posterior tau2.sq samples
  
  #Initialize each element of chain and priors for hyperparameters: mu, sig.sq and tau..
  theta[1,] = rep(0,p)
      mu[1] = mean(y)       #empirical bayes so use overall mean
  sig.sq[1] = 1             #flat prior for variance
  tau.sq[1] = 1             
  
  #Iterate through sampler.
  for (i in 2:iter){
    
    #Update theta_i values (for theta_1...theta_p).
    theta.vars = sig.sq[i-1] * (tau.sq[i-1] / (ni * tau.sq[i-1] + 1))
    theta.means = (tau.sq[i-1] / (ni * tau.sq[i-1] + 1)) * (ni * ybar + (1/tau.sq[i-1]) * mu[i-1])
    theta[i,] = rnorm(p,theta.means,theta.vars)
    
    #Update mu.
    mu.var = sig.sq[i-1] * tau.sq[i-1] / p
    mu.mean = mean(theta[i,])
    mu[i] = rnorm(1,mu.mean,mu.var)
    
    #Update sig.sq.
    alpha = (n+p)/2
    ss1 = sum((y - theta[i,x])^2) 	         	#sum_{i=1}^{p} sum_{j=1}^{ni} (y_ij - theta_i)^2 
    ss2 = sum((theta[i,]-mu[i])^2)		        #sum_{i=1}^{p} (theta_i-mu)^2
    beta = ss1/2 + ss2/(2*tau.sq[i-1]) 
    sig.sq[i] = 1 / rgamma(1, alpha, beta)
    
    #Update tau.sq.
    alpha = p/2-1
    beta = (1 / (2*sig.sq[i])) * sum((theta[i,]-mu[i])^2)
    tau.sq[i] = 1 / rgamma(1, alpha, beta)
  }
  
  #Burn beginning observations.
  if (burn > 0){
    theta = theta[-burn,]
    mu = mu[-burn]
    sig.sq = sig.sq[-burn]
    tau.sq = tau.sq[-burn]
  }
  
  #Thin observations., ie only keep every "thin observation".. thin = 2 so obs(1,3,5,7,...)
  if (thin > 0){
    theta = theta[seq(1,nrow(theta),by=thin),]
    mu = mu[seq(1,length(mu),by=thin)]
    sig.sq = sig.sq[seq(1,length(sig.sq),by=thin)]
    tau.sq = tau.sq[seq(1,length(tau.sq),by=thin)]
  }
  
  #Return results.
  return(list(theta=theta, mu=mu, sig.sq=sig.sq, tau.sq=tau.sq))
}

###Now run GIBBS
y = mtests$mathscore
x = mtests$school
output = gibbs.mathtest(y,x,iter=21000,burn=1000,thin=2)



### MATH TESTS STEP 3
#3. Suppose you use the posterior mean θ_hat_i from the above model to estimate each school-level mean θi. 
#   Define the shrinkage coefficient κi as
#      κ_i = ( ȳ_i − θ_hat_i )/ ȳ_i
#   which tells you how much the posterior mean shrinks the observed sample mean. 

#   Plot this shrinkage coefficient (in absolute value) for each school as a function of that school’s sample size, and comment.



theta.post.mean = colMeans(output$theta)   #get posterior mean for theta_i from gibbs output. output$theta is 10500 x 100

group_stats <- mtests %>% group_by(school)  %>% summarise(n = n(), mu = mean(mathscore), sd = sd(mathscore), se = sd/sqrt(n))  
ybar = group_stats$mu                      #the sample school means.
ni = group_stats$n                         #Sample sizes for each school.
ki = (ybar - theta.post.mean) / ybar       #Calculate shrinkage coefficient for each school.  *****

#Plot shrinkage coefficient (in abs value) for each school as a function of that school's sample size.
group_stats$ki = ki
ggplot(group_stats, aes(n,abs(ki))) + geom_point() + xlab('school sample size') + ylab('shrinkage coefficient') + ggtitle('Shrinkage Coefficient as Function of School Sample Size')
