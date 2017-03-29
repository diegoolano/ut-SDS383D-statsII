library(dplyr)
library(stringr)
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

ggplot(group_stats, aes(n,mu)) + geom_point() 
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
sum( (mtests.test$mathscore - pred1)^2 )            #sum of squared errors using no pooling (each school seperately): 34151.31
sum( (mtests.test$mathscore - grandmean)^2 )        #sum of squared errors using no school info (complete pooling  ): 40291.48
sum( (mtests.test$mathscore - pred2)^2 )            #sum of squared errors using hierachical linear modeling (hlms) : 33674.64




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
  #MODEL:		Hierarchical model:         (* i indexes schools and j indexes scores )
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
    #theta var =  ( prior sigma sq * prior tau sq ) / ( num in group * prior tau sq + 1)
    #theta mean = ( prior tqu sq / ( num in group * prior tau sq + 1) ) * (num in group * avg of group + ( 1 / prior tau sq)) * prior mu
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

ki = (ybar - theta.post.mean) / ybar       #Calculate shrinkage coefficient for each school.  *****kappa_is

#Plot shrinkage coefficient (in abs value) for each school as a function of that school's sample size.
group_stats$ki = ki
ggplot(group_stats, aes(n,abs(ki))) + geom_point() + xlab('school sample size') + ylab('shrinkage coefficient') + ggtitle('Shrinkage Coefficient as Function of School Sample Size')


#Plots for Analysis
#Look at difference in Posteriors
plot(group_stats$n, theta.post.mean - group_stats$mu, xlab='n size', ylab='difference in posterior mean vs old mean')

boxplot(mathscore ~ school, data=mtests, main='new posterior means')
points(theta.post.mean,col=2)

hist(output$theta)
hist(output$mu)
hist(output$sig.sq)   #10 times bigger variable in intra school vs inter school  
hist(output$tau.sq * output$sig.sq)   #so the school has muc

plot(group_stats$mu, theta.post.mean)
abline(0,1)




#========================================================================================================================

#Lecture version of main points from Gelman paper on choosing priors
mu = grandmean
alpha_i = offsets to grandmean

y_it = mu + alpha_i + e_it 
  alpha_i ~ N(0,tau.sq_alpha)
  e_it ~ N(0, sig.sq)

  i from 1 to p  ( p, number of groups)

3 parameters to handle:   mu, sig.sq, tau.sq_alpha, 
  ( mu, sig.sq ):  with typically enough data, any "reasonable" priors will do
                  eg.  p(mu) ~ 1, p(sig.sq) ~ 1/sig.sq
       
   tau.sq_alpha is the hard parameter to give a nonbiased, always nonngegative estimate to 
            - only p "data points" alpha_i to estimate tau.sq_alpha

   The typical conditionally conjugate prior is:       
        tau.sq_alpha ~ InverseGamma( a/2, b/2 ) 
    
        (#you can consider to set a and b to 1 for tau distribution, but make sure that graph IG(1/2,1/2) is somewhat similar to likelihood)
        #normally better (according to Gelman paper)
        #      1) p( tau.sq )  propto  (1 / tau.sq)
        #         or log(sig.sq) uniform
        # - the problem with these improper priors is both of these is that they result in improper posterior distributions
        #  they put infinite mass on (0,epsilon) and we can't rule out zero so the posterior will blow up zero
          
        #      2) Uniform prior on tau_alpha   *****
        #        - this has a finite integral near tau.sq = zero
        #        - posterior is proper if p >=  3 ( james stein estimator )
        #       for small p ( small number of groups ), this call lead to heavy tails for p(tau.sq | y)  
        #        - reasonable choice ( unless p < 3 )
            
        #     3) uniform on tau.sq_alpha
        #        - proper posterior if p >= 4
        #        - even larger heavy-tail problem
        #        - works but not optimal
          
        #     4) IG( E, E)    where E is a very small number  
        #        - NEVER USE 
        #        - limit as E -> 0 : posterior becomes improper
        #        - choice of E very influential
        #        - general advice: if an improper prior would be bad (lead to an improper posterior) then a very vague proper prior is almost always worse.
        #          ie, if it works, you may as well use Jeffrey's Prior ( E = 0 )  
          
        #     5) James' preferred choice   ****
        #       tau_alpha (scale) w/ a half-t prior ( ie, a folded - t prior)
        #       - proper ( no issue of posterior propriety )     
        #       we want something that is monotonically decreasing and something that doesn't decrease too quick
        #         
        prior : should reflect genuine prior knowledge or 
                we should have our noninformative prior not affect our likelihood too much

                  p(tau.sq | y) prop to p(tau.sq) * p(y|tau.sq)
                                                     "marginal likelihood"
                   # posterior   =      prior    * likelihood
                                                                  #this is really a simplification of full expression
          vector y_i = ( y_ii, ... , y_i,Ni)

          p(y_i | tau.sq )  = integral ( p(y_i | alpha_i, tau.sq) * p(alpha_i | tau.sq) d(alpha_i) ) 
                            = prod(t = 1 to N_i) N(y_it | alpha_i, sig.sq) * N(alpha_i | mu, tau.sq_alpha) d(alpha_i)       #for one i

          p(y | tau.sq) = prod(i=1 to p) p(y_i | tau.sq)
        
        #Inverse Gamma priors tend to skew the posteriors to be larger than the original likelihood ( pulls the variance away from )
        #projectuclid.org 
        
        
#For LME4 version for cheese data  , first with mle, then with gibbs
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================


####QUESTION 2
#Price elasticity of demand
#The data in “cheese.csv” are about sales volume, price, and advertisting display activity for packages of Borden sliced “cheese.” 

#For each of 88 stores (store) in different US cities, 
#  we have repeated observations of 
#     - the weekly sales volume (vol, in terms of packages sold), 
#     - unit price (price), and 
#     - whether the product was advertised with an in-store display during that week (disp = 1 for display). 

# *** Your goal is to estimate, on a store-by-store basis, the effect of display ads on the demand curve for cheese. ***

# A standard form of a demand curve in economics is of the form 
#     Q = a * P^b, 
#     where Q is quantity demanded (i.e. sales volume), 
#           P is price, and
#           a and b are parameters to be estimated. 

# This is linear on a log-log scale,
#      log P = log a + b log Q
# which you should feel free to assume here. 
# Economists would refer to b as the price elasticity of demand (PED). 
# Notice that on a log-log scale, the errors enter multiplicatively.

#There are several things for you to consider in analyzing this data set.
#1. The demand curve might shift (different a) and also change shape (different b) depending on whether there is a display ad or not in the store.
#2. Different stores will have very different typical volumes, and your model should account for this.
#3. Do different stores have different PEDs? If so, do you really want to estimate a separate, unrelated b for each store?
#4. If there is an effect on the demand curve due to showing a display ad, 
#   does this effect differ store by store, or does it look relatively stable across stores?
#5. Once you build the best model you can using the log-log specification, do see you any evidence of major model misfit?
# Propose an appropriate hierarchical model that allows you to address these issues, and use Gibbs sampling to fit your model.

#1. so include display as a binary factor
#2. include volume as a variable
#3. Stores have different PEDs, but 
#4. 

#========================================================================================================================
cheesedata = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/cheese.csv", header=TRUE)
dim(cheesedata)                   ## 5555 obs x 4 cols
length(unique(cheesedata$store)) ## 88 stores


#add week, City and Chain variables, and make disp binary
cheesedata <- cheesedata %>% group_by(store) %>% mutate(week = row_number())
          
cities <- str_split(cheesedata$store,pattern = " - ")
cheesedata$city <- ""
for(i in 1:length(cities)){ cheesedata[i,]$city <- str_trim(cities[i][[1]][1]) }
cheesedata$city <- factor(cheesedata$city)

cheesedata$chain <- ""
for(i in 1:length(cities)){  cheesedata[i,]$chain <- str_trim(cities[i][[1]][2])}
cheesedata$chain <- factor(cheesedata$chain)

cheesedata$disp <- factor(cheesedata$disp)
summary(cheesedata)
#                              store          price            vol              disp                        city             chain     
# BALTI/WASH - SAFEWAY            :  68   Min.   :1.320   Min.   :   231   Min.   :0.0000   CHARLOTTE         : 244   WINN DIXIE: 723  
# BALTI/WASH - SUPER FRESH        :  68   1st Qu.:2.457   1st Qu.:  1990   1st Qu.:0.0000   DALLAS/FT. WORTH  : 235   KROGER CO : 638  
# BIRMINGHAM/MONTGOM - KROGER     :  68   Median :2.703   Median :  3408   Median :1.0000   BALTI/WASH        : 197   FOOD LION : 434  
# BOSTON - STAR MARKET            :  68   Mean   :2.869   Mean   :  4771   Mean   :0.6457   SYRACUSE          : 197   PUBLIX    : 244  
# BUFFALO/ROCHESTER - TOPS MARKETS:  68   3rd Qu.:3.203   3rd Qu.:  5520   3rd Qu.:1.0000   BIRMINGHAM/MONTGOM: 190   LUCKY     : 190  
# BUFFALO/ROCHESTER - WEGMANS     :  68   Max.   :4.642   Max.   :148109   Max.   :1.0000   BOSTON            : 190   RALPHS    : 136  
# (Other)                         :5147                                                     (Other)           :4302   (Other)   :3190

sort(summary(cheesedata$chain),decreasing = T)
length(levels(cheesedata$chain))  #50 Chain Stores

sort(summary(cheesedata$city),decreasing = T)
length(levels(cheesedata$city))   #46 Cities

cheese_group_stats_all <- cheesedata %>% group_by(store)  %>% summarise(n = n(), p_mu = mean(price), p_sd = sd(price), q_mu = mean(vol), q_sd = sd(vol))
head(cheese_group_stats_all)
summary(factor(cheese_group_stats_all$n))   #notice not all have same out of observations
# #obs   52 61 68 
# #groups 1 59 28

g61 = cheese_group_stats_all %>% filter(n == 61)
g61m = colMeans(g61[,c(2:6)])
g68 = cheese_group_stats_all %>% filter(n == 68)
g68m = colMeans(g68[,c(2:6)])

#outlier in number of observations
cheese_group_stats_all %>% filter(n == 52)   #DALLAS/FT. WORTH - WINN DIXIE   

#see overall means / group means
p <- ggplot(cheese_group_stats_all, aes(n, log(q_mu))) + geom_point() 
p <- p + geom_hline(yintercept = mean(log(cheesedata$vol)),col="red") 
p <- p + geom_point(data = as.data.frame(t(g61m)), colour="blue",size=3)
p <- p + geom_point(data = as.data.frame(t(g68m)), colour="blue",size=3) + annotate("text", x = 53, y = 8.2, label = "GRAND MEAN of log(vol)",size=2.5,col="red")
p + xlab("number of obs") + ylab("volume mean (log scale)") + annotate("text", x = 54, y = 7.6, label = "DALLAS/FT. WORTH - WINN DIXIE",size=2.5)

#see log(price) by log(vol) by store


#4. is there an effect on the demand curve due to showing a display ad, 
# does this effect differ store by store, or does it look relatively stable across stores?
disp0 <- cheesedata %>% filter(disp == 0) 
disp1 <- cheesedata %>% filter(disp == 1)
disp0.mu = mean(log(disp0$vol))
disp1.mu = mean(log(disp1$vol))
p <- ggplot(cheesedata, aes(disp,log(vol))) + geom_jitter() + geom_hline(yintercept = mean(log(cheesedata$vol)),col="red")
p <- p +  geom_hline(yintercept = disp0.mu ,col="green") + annotate("text", x = 1, y=12, label="Disp = 0, 1968 obs", col="green")
p <- p +  geom_hline(yintercept = disp1.mu ,col="blue") + annotate("text", x = 2, y=12, label="Disp = 1, 3587 obs", col="blue")
p

lmQgivenD = lm( log(vol) ~ disp, data=cheesedata)
summary(lmQgivenD)   #Yes there is an effect of Display on Demand Overall

lmQgivenDbyStore = lm( log(vol) ~ disp + store, data=cheesedata)
summary(lmQgivenDbyStore)  #Yes, and relatively stable


#https://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html
#xyplot(log(vol) ~ log(price) | store, data=cheesedata, type = c("p", "r"),  group = disp, auto.key = TRUE)    

#PLOT WHICH SHOWS DIFFERENCE BY DISP PER GROUP
xyplot(log(vol) ~ log(price) | store, data=cheesedata, type = c("p", "r"),  group = disp, auto.key = list(lines = TRUE), par.strip.text=list(cex=.5))

#library(hexbin)
#hexbinplot(log(vol) ~ log(price), cheesedata, aspect = 1, bins=50)
#http://lmdvr.r-forge.r-project.org/
#??stripplot
#barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",auto.key = list(adj = 1))

strip.default


#1) non HLM approaches 
#========================================================================================================================
lm0 <- lm(log(vol) ~ log(price) , data=cheesedata)                #complete pooling, not taking store into account or display ad
lm1 <- lm(log(vol) ~ log(price) + disp , data=cheesedata)         #complete pooling, not taking store into account, but considering display
lm2 <- lm(log(vol) ~ log(price) + disp + store, data=cheesedata)  #no pooling, different slopes for each store considering disp
lm3 <- lm(log(vol) ~ log(price) + store, data=cheesedata)         #no pooling, different slopes for each store
lm4 <- lm(log(vol) ~ log(price) + city, data=cheesedata)
lm5 <- lm(log(vol) ~ log(price) + chain, data=cheesedata)
lm6 <- lm(log(vol) ~ log(price) + disp + store + city + chain, data=cheesedata)   #not enough data to meaninfully do this

lm7a <- lm(log(vol) ~ log(price) + disp + store + disp:store, data=cheesedata)   #since lm2 worked well, add interactions
lm7b <- lm(log(vol) ~ log(price) + disp + store + disp:log(price), data=cheesedata)   #since lm2 worked well, add interactions
lm7c <- lm(log(vol) ~ log(price) + disp + store + store:log(price), data=cheesedata)   #since lm2 worked well, add interactions
lm7d <- lm(log(vol) ~ log(price) + disp + store + disp:store + disp:log(price) + store:log(price), data=cheesedata)   #since lm2 worked well, add interactions




summary(lm0)
summary(lm1)
summary(lm2)  #give highest R2 
summary(lm3)   
      #storeBOSTON - STAR MARKET                 0.09863    0.05330   1.850 0.064319 .  
      #storeJACKSONVILLE,FL - FOOD LION         -0.01125    0.05472  -0.206 0.837091   
summary(lm4)
summary(lm5)
summary(lm6) #kitchen sink

summary(lm7a)   #disp:store interaction
summary(lm7b)   #disp:log(price)
summary(lm7c)   #store:price   ( predictive but not super relevant)
summary(lm7d)   #same
anova(lm0)


anova(lm1,lm0)  #F-test, display ad is significant
anova(lm3,lm0)  #F-test, concludes store is significant.
anova(lm2,lm1)	#F-test, store is significant
anova(lm4,lm0)  #F-test, city is significant.
anova(lm5,lm0)  #F-test, chain is significant.

anova(lm7a,lm2)  #interaction terms appear significant
anova(lm7b,lm2)  
anova(lm7c,lm2)  
anova(lm7d,lm2)  




#2) LMER VERSION
#========================================================================================================================
#1 Hierarchical linear model; which only allows intercept among stores to change .
hlm1 = lmer(log(vol) ~ log(price) + disp + (1 | store), data=cheesedata)
summary(hlm1)

coef(hlm1)  #per store intercepts vary, but not slopes
fixef(hlm1)  
#(Intercept)  log(price)       disp1 
# 10.6266256  -2.5235375   0.1860867 
ranef(hlm1) #per store

#plot residuals
boxplot(resid(hlm1) ~ store, data=cheesedata, las=2, cex.axis=.3, main='residuals by store for hlm1') 
abline(a=2,b=0,col='red',lty=2)
abline(a=-2,b=0,col='red',lty=2)

#2 so Hierarchical linear model; which allows intercept and slopes to vary among stores to change .
hlm2 = lmer(log(vol) ~ log(price) + disp + (1 + log(price) | store), data=cheesedata)
summary(hlm2)

coef(hlm2)  #per store intercepts vary along with slopes for price, effect of display same for each group
fixef(hlm2)  
#(Intercept)  log(price)       disp1 
# 10.4843803  -2.3482953   0.1697767 
ranef(hlm2) #per store

boxplot(resid(hlm2) ~ store, data=cheesedata, las=2, cex.axis=.3, main='residuals by store for hlm2') 
abline(a=2,b=0,col='red',lty=2)
abline(a=-2,b=0,col='red',lty=2)


#3 so Hierarchical linear model; which allows intercept and slopes to vary among stores to change .
hlm3 = lmer(log(vol) ~ log(price) + disp + (1 + log(price) + disp | store), data=cheesedata)
summary(hlm3)

coef(hlm3)  #per store intercepts vary along with slopes for price and display
fixef(hlm3)  
#(Intercept)  log(price)       disp1 
# 10.4916513  -2.3497484   0.1723921  
ranef(hlm3) #per store


boxplot(resid(hlm3) ~ store, data=cheesedata, las=2, cex.axis=.3, main='residuals by store for hlm3') 
abline(a=2,b=0,col='red',lty=2)
abline(a=-2,b=0,col='red',lty=2)

ndf <- as.data.frame(cbind(cheesedata$store, resid(hlm3 )))
colnames(ndf) <- c("store","residuals")
ggplot( ndf, aes(store,residuals) ) + geom_point()   #so we can be relatively confident about our fit


#Plot conditional modes (same as means, for linear mixed models) of random effects.
resid = ranef(hlm3, condVar = T)
dotplot(resid,scales=list(cex=c(.5,.5)),layout=c(3,1),main=T, main.title='Random Effects by by Store',)


#####3   hierarchical model with Gibbs sampling to fit model
#========================================================================================================================
# log(vol) ~ log(a) + B log(p) + disp    | for each group Store t and repeated observation i
y = log(cheesedata$vol)
x = log(cheesedata$price)

cheese_group_stats_all <- cheesedata %>% group_by(store)  %>% summarise(n = n(), p_mu = mean(log(price)), p_sd = sd(log(price)), q_mu = mean(log(vol)), q_sd = sd(log(vol)))

#Adapted partially from Jennifer and Giorgio's code
#particularly for idea setup of 3 dimensional Bi and sampling of inverse wishart

#install.packages("MCMCpack")
#install.packages("LaplacesDemon")
require(MCMCpack)  						#Sample from Inverse Wishart.
require(mvtnorm)							#Sample from Multivariate Normal.
require(LaplacesDemon)						#Sample from Normal-Inverse-Wishart


p = 3  #number of paramters ( intercept, price, disp ) for each group
# Run the Gibbs Sampler
Niter <- 11000
burnin <- 100
thin <- 2

#hyperparamter priors for m,v,C,d.
m = rep(0,3)   #mu is 0 for our three paramters
v = 1          #variation is 1 
C = diag(p)    #parameters for inverse wishart
d = p+1        #""


n = length(y)  							                	#Number of observations.
nt = cheese_group_stats_all$n	                #Sample sizes for each group (# time points per store).
s = length(unique(cheesedata$store))					#Number of groups (stores).
ybar = cheese_group_stats_all$q_mu            #Sample group means.

#Sample group means for each x covariate.
X = as.data.frame(model.matrix(log(vol) ~ 1 + log(price) + disp,data=cheesedata))  #Covariates for all i,t
idx = cbind.data.frame(store=cheesedata$store,  storenum=as.numeric(cheesedata$store), week=cheesedata$week)  
Xbar = data.frame(matrix(nrow=s,ncol=p))
for (k in 1:p){ Xbar[,k] = aggregate(X[,k],list(idx$store),mean)$x }
Xbar = as.matrix(Xbar)
X = as.matrix(X)

#Set up data structures to hold Gibbs samples from posteriors.
Bi = array(0,dim=c(s,p,Niter))		# Each slice is a (88x3) matrix, with each column as a vector of covariates for each store. 
                                  # dim(Bi) 88     3 11000
                        					# Each row stores akk the results of the iterations of the Gibbs sampler (1 x 3 x Niter)

sig.sq = rep(0,Niter)	      			#Vector of posterior sig2.sq samples
mu = matrix(0,Niter,p)				    #mu ( p x 1 ) for each iteration of Gibbs Sampler
Sigma = array(0,dim=c(p,p,Niter))	#Each Sigma[,,1] is a pxp Gibbs sample of Sigma matrix.

#Initialize each element of chain.
Bi[,,1] = rep(0,s*p)
sig.sq[1] = 1
mu[1,] = rep(mean(y),p)
Sigma[,,1] = diag(p)

#Iterate through sampler.
for (k in 2:iter){
  
  #Update B_i values for each store.
  
  SigInv = solve(Sigma[,,k-1])	#Prechache Sigma inverse.
  
  for (j in 1:s){	#Loop through stores.
    Xi = X[which(j==idx$storenum),]	#Pull only Xi values for current store.
    yi = y[which(j==idx$storenum)]	#Pull out responses for current store.
    Bi.var = solve( solve(SigInv) + (nt[j]/sig.sq[k-1]) * t(Xi) %*% Xi	)
    Bi.mean = Bi.var %*% (SigInv %*% mu[k-1,] + (nt[j]/sig.sq[k-1]) * t(Xi) %*% yi )
    
    Bi[j,,k] = rmvnorm(1,Bi.mean,Bi.var)  
  }
  
  #Update sig.sq values.
  Bi.lookup = Bi[idx$store,,k]	#Finds Bi for each yi, based on store number.
  SS = sum((y - X %*% t(Bi.lookup))^2)
  sig.sq[k] = 1 / rgamma(1,n/2,.5 * SS)
  
  #Update mu and Sigma values.
  Bi.bar = colMeans(Bi[,,k])						#Precache.
  S = t(Bi[,,k] - Bi.bar)	%*% (Bi[,,k] - Bi.bar)	#Precache.
  
  mn = (v*m + s*Bi.bar) / (v + s)	#Posterior NIW mean parameter.
  vn = v + s						#Posterior cov parameter.
  dn = d + s						#Posterior df parameter.
  Cn = C + S + (v*s / (v+s)) * (Bi.bar - m) %*% t(Bi.bar - m)		#Posterior scale matrix.
  
  niv.draw = rnorminvwishart(n=1,mn,vn,Cn,dn)	#Draw from Normal-Inverse-Wishart.
  Sigma[,,k] = niv.draw$Sigma
  mu[k,] = niv.draw$mu
}

#Thin and Burn results
Bi = Bi[,,seq(1+burnin,dim(Bi)[3],by=thin)]
Sigma = Sigma[,,seq(1+burnin,dim(Sigma)[3],by=thin)]
mu = mu[seq(1+burnin,nrow(mu),by=thin),]
sig.sq = sig.sq[seq(1+burnin,length(sig.sq),by=thin)]

#Calculate posterior means.
Bi.post.mean = apply(Bi,c(1,2),mean)
Sigma.post.mean = apply(Sigma,c(1,2),mean)
mu.post.mean = colMeans(mu)
sig.sq.post.mean = mean(sig.sq)

#Look at Poster results.
Bi.post.mean
Sigma.post.mean
mu.post.mean
sig.sq.post.mean

par(mfrow=c(1,3))
plot(Bi.post.mean[,1])
plot(Bi.post.mean[,2])
plot(Bi.post.mean[,3])