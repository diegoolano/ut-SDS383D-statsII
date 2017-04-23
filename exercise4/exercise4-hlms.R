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
        
        

#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
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
rm(list = ls())
library(dplyr)
library(stringr)
library(ggplot2)
library(mosaic)
library(lme4)
cheesedata = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/cheese.csv", header=TRUE)
dim(cheesedata)                   ## 5555 obs x 4 cols
length(unique(cheesedata$store))  ## 88 stores

##EXPLORATORY PHASE
#add week 
cheesedata <- cheesedata %>% group_by(store) %>% mutate(week = row_number())
          
#add City and Chain variables  ( i didn't end up using these as factors but I wanted to get an idea of things)
cities <- str_split(cheesedata$store,pattern = " - ")
cheesedata$city <- ""
for(i in 1:length(cities)){ cheesedata[i,]$city <- str_trim(cities[i][[1]][1]) }
cheesedata$city <- factor(cheesedata$city)

cheesedata$chain <- ""
for(i in 1:length(cities)){  cheesedata[i,]$chain <- str_trim(cities[i][[1]][2])}
cheesedata$chain <- factor(cheesedata$chain)

#make disp binary
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

sort(summary(cheesedata$chain),decreasing = T)  #Most to least common chains: WINN DIXIE, KROGER CO, FOOD LION, PUBLIX , etc
length(levels(cheesedata$chain))  #50 Chain Stores

sort(summary(cheesedata$city),decreasing = T)   #Most to least common cities:  CHARLOTTE, DALLAS/FT. WORTH, BALTI/WASH, SYRACUS
length(levels(cheesedata$city))   #46 Cities

cheese_group_stats_all <- cheesedata %>% group_by(store)  %>% summarise(n = n(), p_mu = mean(price), p_sd = sd(price), q_mu = mean(vol), q_sd = sd(vol))
head(cheese_group_stats_all)
#                         store     n     p_mu       p_sd     q_mu      q_sd
# 1   ALBANY,NY - PRICE CHOPPER    61 2.758409 0.31226344 1363.246 1385.2279
# 2         ATLANTA - KROGER CO    61 2.827948 0.39100787 5077.607 1505.4671
# 3        ATLANTA - WINN DIXIE    61 2.667663 0.05380711 2956.754  463.3081
# 4 BALTI/WASH - GIANT FOOD INC    61 3.682421 0.24693757 4958.574 1973.3754
# 5        BALTI/WASH - SAFEWAY    68 3.883288 0.37420494 4458.515 1939.1340
# 6    BALTI/WASH - SUPER FRESH    68 3.691940 0.33196917 1489.147  972.4967


summary(factor(cheese_group_stats_all$n))   #notice not all have same out of observations, but relatively close 
# #obs   52 61 68                    
# #groups 1 59 28

g61 = cheese_group_stats_all %>% filter(n == 61)
g61m = colMeans(g61[,c(2:6)])
g68 = cheese_group_stats_all %>% filter(n == 68)
g68m = colMeans(g68[,c(2:6)])

#see outlier in number of observations
cheese_group_stats_all %>% filter(n == 52)   #DALLAS/FT. WORTH - WINN DIXIE   (so maybe less confidence for this one)

#IMAGE OF  overall means / group means
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

#IMAGE to show a little more detail about difference in groups by disp
p <- ggplot(cheesedata, aes(disp,log(vol))) + geom_jitter() + geom_hline(yintercept = mean(log(cheesedata$vol)),col="red")
p <- p +  geom_hline(yintercept = disp0.mu ,col="green") + annotate("text", x = 1, y=12, label="Disp = 0, 1968 obs", col="green")
p <- p +  geom_hline(yintercept = disp1.mu ,col="blue") + annotate("text", x = 2, y=12, label="Disp = 1, 3587 obs", col="blue")
p

##Confounding issue of Sales Week causing price by volume to be shifted on display
plot(log(cheesedata$price),log(cheesedata$vol))


lmQgivenD = lm( log(vol) ~ disp, data=cheesedata)
summary(lmQgivenD)   #Yes there is an effect of Display on Demand Overall

lmQgivenDbyStore = lm( log(vol) ~ disp + store, data=cheesedata)
summary(lmQgivenDbyStore)  #Yes its different per store


#https://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html
#IMAGE PLOT WHICH SHOWS DIFFERENCE BY DISP PER STORE
xyplot(log(vol) ~ log(price) | store, data=cheesedata, type = c("p", "r"),  group = disp, auto.key = list(lines = TRUE), par.strip.text=list(cex=.5))

#library(hexbin)
#hexbinplot(log(vol) ~ log(price), cheesedata, aspect = 1, bins=50)
#http://lmdvr.r-forge.r-project.org/
#??stripplot
#barchart(prop.table(postdoc, margin = 1), xlab = "Proportion",auto.key = list(adj = 1))



#1) non HLM approaches to understanding relation
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
#A. Hierarchical linear model; which only allows intercept among stores to change .
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

#B. so Hierarchical linear model; which allows intercept and slopes to vary among stores to change .
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


#C. so Hierarchical linear model; which allows intercept and slopes to vary among stores to change .
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


#D) Final Hierarchical approach using interaction of store/price as ewll  ( Professor Scott showed this in class .. see cheese_lmer.R)
hlm4 = lmer(log(vol) ~ log(price) + disp + log(price):disp | store , data=cheesedata)

summary(hlm4)  #shows restricted maximum likelihood, assumes Sigma is full rank 

#Notes from class 
#our model is 
'''
Bi = ( intercept, log price, display, log(price):display interaciton )
Bi ~ N( mu, Sigma )  

#14 parameter ( 4 on diagonl, 6 off, and 4 for mean vector mu)
#for 88 groups

#inverse wishart 
#possible priors
#- use MLE covariance matrix, as aempirical bayes 
diffuse parameter for identity matrix 
'''

coef(hlm4)  #per store intercepts vary along with slopes for price and display
fixef(hlm4)  
#(Intercept)  log(price)       disp1 
# 10.4916513  -2.3497484   0.1723921  
ranef(hlm4) #pe

#####3   hierarchical model with Gibbs sampling to fit model
#========================================================================================================================
#So our model is:  log(Q_it) = log(a_i) + log(price_it) + disp_it + disp_it*log(price_it)
#This accounts for seperate store_intercepts + seperate store price slopes + seperate slopes based on display + interactions for store disp/prices
#

rm(list = ls())
library(dplyr)
require(MCMCpack)  						#Sample from Inverse Wishart.
require(mvtnorm)							#Sample from Multivariate Normal.
require(MASS)                 #Generalized invers

#Hierarchical model:
#			y_it ~ N(X_it B_i, sig.sq)
#				where 
#				y_it = log(Q_it)    
#				X_it = [1	log(price_it) disp_it log(price_it)*disp_it] for store i at time t.
#					
#		   	       B_i ~ N(mu,Sigma)
#		  	(mu,Sigma) ~ Normal-Inv-Wishart(m,V,C,d)     #unknown hyper priors m,V,C,d:   mean, covariance, scale and df hyperpriors for N-I-W.
#			      sig.sq ~ 1/sig.sq (Jeffrey's Prior)

cheesedata = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/cheese.csv", header=TRUE)
y = log(cheesedata$vol)
X = as.data.frame(model.matrix(log(vol) ~ log(price) + disp + log(price)*disp,data=cheesedata))		#Covariates for all stores i, observations t

#use OLS to get initial overall mu estimates as per James recommendations!
ols = lm(log(vol) ~ log(price) + disp + log(price)*disp,data=cheesedata)

#Noninformative hyperpriors for Normal-Inverse-Wishart :  muB,V,C,d.
p = ncol(X)    #num predictors
muB = ols$coef 
V = diag(p)
C = diag(p)
d = p+1        #one degree of freedom more than number of predictors

n = length(y)  								                                      #Total observations.
ns = cheesedata %>% group_by(store)  %>% summarise(n = n()) %>% .$n	#Observations for each group 
s = length(unique(cheesedata$store))				        	              #Number of groups (stores).

#MCMC vars
iter=1100
burn=100
thin=2

#Set up yi, Xi, Wi matrices for each store.
yi = list()
Xi = list()
Wi = list()
for (i in 1:s){	
  yi[[i]] = y[which(as.numeric(cheesedata$store)==i)]
  Xi[[i]] = as.matrix(X[which(as.numeric(cheesedata$store)==i),])
  Wi[[i]] = Xi[[i]]
}

#Set up data structures to hold the Gibbs samples from the posteriors.
Beta = matrix(0,p,iter)			    	#Each entry is a (px1) gibbs sample.  (Constant across store.)                4 x runs
bi  = array(0,dim=c(p,s,iter))		#Each gibbs sample slice is a (sxp) matrix, with p covariates for s stores.   4 x 88 x runs
Sigma = array(0,dim=c(p,p,iter))	#Each gibbs sample slice is a (pxp) matrix.                                   4 x 4  x runs
sig.sq = rep(0,iter)			 	      #Each sig.sq value is a gibbs sample.                                         runs x 1

#Initialize first element of chain.
Beta[1] = 0
bi[,,1] = rep(0,s*p)
sig.sq[1] = 1
Sigma[,,1] = diag(p)

#Run Gibbs sampler.
for (k in 2:iter){
  ### Update bi for each store
  if(k %% 100 == 1){ print(k) }
  SigInv = solve(Sigma[,,k-1])  
  SS.Beta.1 = 0	#sum(over stores) Xi^T * Xi           for Beta update
  SS.Beta.2 = 0	#sum(over stores) Xi^T * (yi + Wi*bi) for Beta update.
  SS.sig.sq = 0	#sum(over stores) Xi^T(yi-Wi*bi)      for sig.sq update.
  SS.Sigma  = 0	#sum(over stores) bi * bi^T           for Sigma update.
  
  for (j in 1:s){
    X.j = Xi[[j]]
    y.j = yi[[j]]
    W.j = Wi[[j]]
    YmXB = y.j - X.j %*% Beta[,k-1]   #precache    
    Var = solve(SigInv + (1/sig.sq[k-1]) * crossprod(W.j) ) #prior precision
    mean = (1/sig.sq[k-1]) * Var %*% crossprod(W.j,YmXB)    #data precision 
    
    #for current iteration k, and store j
    #b ~ MVN(1/sig.sq[k-1] * solve(SigInv + (1/sig.sq[k-1]) * (Wj)T(Wj)) %*% (YmXB)T(Wj) , solve(SigInv + (1/sig.sq[k-1]) * (Wj)T(Wj)) )  
    bi[,j,k] = rmvnorm(1,mean,Var)    #update bi for store j and iteration k
    
    #update SumSqure values to be used later in Beta, sig.sq, and Sigma updates.
    SS.Beta.1 =+ crossprod(X.j)
    SS.Beta.2 =+ crossprod(X.j, y.j - W.j %*% bi[,j,k])		
    SS.sig.sq =+ crossprod(y.j - X.j %*% Beta[,k-1] - W.j %*% bi[,j,k])
    SS.Sigma  =+ tcrossprod(bi[,j,k])
  } 
  
  VInv = solve(V)  
  b_var = solve(VInv + (1/sig.sq[k-1]) * SS.Beta.1)
  b_mean = b_var %*% ( VInv %*% muB + (1/sig.sq[k-1]) * SS.Beta.2)
  Beta[,k] = rmvnorm(1,b_mean,b_var)  ### Update Beta.
  
  sig.sq[k] = 1 / rgamma(1,n/2, SS.sig.sq/2) ### Update sig.sq.
  
  dn = d + s
  Cn = C + SS.Sigma
  Sigma[,,k] = riwish(dn,Cn)   ### Update Sigma.
} 

#Burn beginning observations and thin observations to just keep every other "thin" observation.
bt_seq = seq(burn,iter,by=thin)
bi = bi[,,bt_seq]
Sigma = Sigma[,,bt_seq]
sig.sq = sig.sq[bt_seq]
Beta = Beta[,bt_seq]

#Calculate posterior means.
bi.pm = apply(bi,c(1,2),mean)
Beta.pm = rowMeans(Beta)   
sig.sq.pm = mean(sig.sq)
Sigma.pm = apply(Sigma,c(1,2),mean)


#Now show demand curves for each store.
# Add ad and non-ad demand curves per store
#
#  	AD = NO group:    exp(log(Intercept)) * x ^ (log(Price)))
#		AD = YES group: 	exp(log(Intercept + disp) * x ^ (log(Price) - log(Price):disp))
#                             log(price)     disp log(price):disp (Intercept)
# Under model formulation:   AD = NO:	exp(log(B + Intercept_i))


#Beta is the overall effect on each variable where as bi.pm is store specific 
intrcpt 	= Beta.pm[1] + bi.pm[1,]   
logp 	= Beta.pm[2] + bi.pm[2,]
dsp 	= Beta.pm[3] + bi.pm[3,]
logp.d 	= Beta.pm[4] + bi.pm[4,]

#from hml
overall_int_hlm = mean(intrcpt)
overall_logp_hlm = mean(logp)
overall_dsp_hlm = mean(dsp)
overall_logp.d_hlm = mean(logp.d)

#from ols
overall_int = muB[[1]]
overall_logp = muB[[2]] 
overall_dsp = muB[[3]]
overall_logp.d = muB[[4]]  



#FIRST plot demand curve for a single store 
par(mfrow=c(1,1))
i = 1
curstore =  cheesedata %>% filter( as.numeric(store) == i)
curstore_wo_disp = curstore %>% filter( disp == 0)
plot(vol ~ price, data=curstore, xlab=paste('Store',i),ylab='',col='red',xaxt='n',yaxt='n',main=paste('Store',i))  #Color all points red
points(vol ~ price, col='blue',  data=curstore_wo_disp)    #Color 'disp = 0' points in blue.

curve(exp(intrcpt[i])*x^(logp[i]), add=TRUE, col='blue')   #Fitted demand curve for disp=0 group at store i.
curve(exp(intrcpt[i] + dsp[i])*x^(logp[i] + logp.d[i]), add=TRUE, col='red') #Fitted demand curve for disp=1 group at store i.

curve(exp(overall_int)*x^(overall_logp), add=TRUE, col='purple', lty=2)
curve(exp(overall_int + overall_dsp)*x^(overall_logp + overall_logp.d), add=TRUE, col='orange', lty=2)

curve(exp(overall_int_hlm)*x^(overall_logp_hlm), add=TRUE, col='black', lty=2)
curve(exp(overall_int_hlm + overall_dsp_hlm)*x^(overall_logp_hlm + overall_logp.d_hlm), add=TRUE, col='green', lty=2)

legend('top','groups',c("Store Disp=0","Store Disp=1","Overall Disp=0 (OLS)", "Overall Disp=1 (OLS)", "Overall Disp=0 (HLM)", "Overall Disp=1 (HLM)"), lty = c(1,1,2,2,2,2),
       col=c('blue','red','purple','orange','black','green'),ncol=1,bty ="n")

#NOW Plot demand curve for all stores sorted by price
sort.avg.price = as.numeric(cheesedata %>% group_by(store)  %>% summarise(avg_price = mean(price)) %>% arrange(desc(avg_price)) %>% .$store)
par(mfrow=c(8,11))
for (i in sort.avg.price){ 
  curstore =  cheesedata %>% filter( as.numeric(store) == i)
  curstore_wo_disp = curstore %>% filter( disp == 0)
  plot(vol ~ price, data=curstore, xlab=paste('Store',i),ylab='',col='red',xaxt='n',yaxt='n',main=paste('Store',i))
  points(vol ~ price, col='blue',  data=curstore_wo_disp)   #Add the 'AD = 0' points in blue.
  
  curve(exp(intrcpt[i])*x^(logp[i]), add=TRUE, col='blue')   #Fitted demand curve for disp=0 group at store i.
  curve(exp(intrcpt[i] + dsp[i])*x^(logp[i] + logp.d[i]), add=TRUE, col='red') #Fitted demand curve for disp=1 group at store i.
}






#==========================
'''
Demand Curves
 Q = K * P ^ B                        ( which can be rewritten logQ = logK + B * logP )
    model :   K  and B change w/ 
        - store
        - disp
        - intraction bt store/disp

      - Beta ( would allow change between Beta and Price)

Net Profit = Quantity ( Price - UnitCost )
      N(P) = Q(P-C)
           = KP^B ( P - c)
 
to optimize N(P):
     set N\'(P) = 0
     P* = g(K,B)        ( g gives optimal price for given K and B which you get from posteriors ) 
                          this allows you to give a posterior distribution of optimal price !!
  

Calculate posterior probabilty of a model ( likelihood )
* Bayesian Model Selection.  
* propogation of uncertainity 
'''
###############
#04/03 class
'''
halft.R
a = 3
NMC = 10000

10000 normals time 10000 inverse gamma  ( t3  via redundant t)

shows via monte carlo that the redundant parameterization corresponds to the initial one

schools_halfcauchy.R    #LOOK AT THIS FOR HIS FULL BAYESIAN SOLUTION TO CHEESE

Half Cauchy is better than Inverse Gamma cause it doesnt force tau.sq to zero!! <--- number one reason why half cauchy is better than inverse gamma 
#half cauchy better than normal because normal decays too slow
'''


#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================

####QUESTION 3
#A hierarchical probit model via data augmentation  (polls data)
#  - a Bayesian treatment of probit regression (similar to logistic regression) 
#  - using the trick of data augmentation — 
#  -       that is, introducing "latent variables" that turn a hard problem into a much easier one

# "polls.csv" has the results of several political polls from the 1988 U.S. presidential election. 
#  The outcome of interest is whether someone plans to vote for George Bush (senior, not junior). 
#  There are several potentially relevant demographic predictors here, including the respondent’s state of residence. 
#  ** The goal is to understand how these relate to the probability that someone will support Bush in the election. 

#  You can imagine this information would help a great deal in poll reweighting and aggregation (ala Nate Silver).

# Use Gibbs sampling, together with the Albert and Chib data augmentation trick, to fit a hierarchical probit model of the following form:
#   Pr(y_ij = 1) = F(z_ij)
#     z_ij = μ_i + (x_ij)Tb.

#   where 
#      y_ij is the response (Bush=1, other=0) for respondent j in state i; 
#      F(·) is the probit link function, i.e. the CDF of the standard normal distribution; 
#      μi is a state-level intercept term; 
#      xij is a vector of respondent- level demographic predictors; and b is a vector of state-invariant regression coefficients.

#Notes:
#1. There are severe imbalances among the states in terms of numbers of survey respondents! 
#         Following the last problem, the key is to impose a hierarchical prior on the state-level intercepts.
#2. The data-augmentation trick from the Albert and Chib paper above is explained in many standard references on Bayesian analysis. 
#         If you want to get a quick introduction to the idea, you can consult one of these. 
#         A good presentation is in Section 8.1.1 of "Bayesian analysis for the social sciences" by Simon Jackman, available as an ebook through lib.utexas.edu.
          #related-ish presentation: http://www.lawschool.cornell.edu/SELS/upload/Jackman-Part2.pdf
          #http://jackman.stanford.edu 

#Related Books: Gelman, Andrew. Bayesian data analysis.   (2014)   and Simon Jackman. Bayesian analysis for the social sciences ( 2009 )
#3. You are welcome to use the logit model instead of the probit model. 
#         If you do this, you’ll need to read the following paper, rather than Albert and Chib: 
#         Polson, N.G., Scott, J.G. and Win- dle, J. (2013). Bayesian inference for logistic models using Polya-Gamma latent variables. J. Amer. Statist. Assoc. 108 1339–1349.

pollsdata = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/polls.csv", header=TRUE)
#dim(pollsdata) 2193 x 10

# head(pollsdata)
#      org year survey bush state      edu    age female black weight
# 1 cbsnyt    7   9158   NA    CT SomeColl 18to29      1     0    923
# 2 cbsnyt    7   9158    1    PA     Bacc 30to44      1     0    558
# 3 cbsnyt    7   9158    0    NJ       HS 65plus      1     0    448
# 4 cbsnyt    7   9158    0    CT SomeColl 18to29      1     0    923

#remove na's
pollsdata <- pollsdata %>% filter( bush != "NA") %>% select(bush,state,edu,age,female,black,weight)
#pollsdata$bush <- factor(pollsdata$bush)

#> summary(pollsdata)
#  bush         state            edu          age          female           black             weight    
# 0: 891   CA     : 190   Bacc    :559   18to29:491   Min.   :0.0000   Min.   :0.00000   Min.   : 149  
# 1:1124   NY     : 163   HS      :773   30to44:762   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.: 568  
#          TX     : 129   NoHS    :216   45to64:480   Median :1.0000   Median :0.00000   Median : 841  
#          FL     : 126   SomeColl:467   65plus:282   Mean   :0.5876   Mean   :0.07643   Mean   :1000  
#          OH     : 101                               3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1137  
#          PA     :  95                               Max.   :1.0000   Max.   :1.00000   Max.   :8562                                                              

#Massive Difference in Number of Observations Per State!
sort(summary(pollsdata$state),decreasing = T)
#  CA  NY  TX  FL  OH  PA  IL  MI  NJ  VA    ....    DE  ND SD  MT  ID  NV  DC  NH  VT  WY 
# 190 163 129 126 101  95  85  83  67  58             7   7  6   5   4   4   3   3   2   2 

summary(sort(summary(pollsdata$state),decreasing = T))      
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   12.00   30.00   41.12   50.00  190.00 


#what is weight?  Is it weights for the Betas?
summary(pollsdata$weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 149     568     841    1000    1137    8562

#order state by number of observations
par(mfrow = c(1,2))
pollsdata$state <- factor(pollsdata$state, levels = names(sort(summary(pollsdata$state),decreasing = T)))
pollsdata$edu <- factor(pollsdata$edu, levels = c("NoHS","HS","SomeColl","Bacc"))

plot(pollsdata$state, pollsdata$weight, main='weights per state')
sts <- names(sort(summary(pollsdata$state),decreasing = T))
plot(factor(sts,levels=sts), sort(summary(pollsdata$state),decreasing = T), col='red', lty=2, type="p",main='observations per state')
lines(factor(sts,levels=sts), sort(summary(pollsdata$state),decreasing = T), col='red', lty=2)



#FOR NOW DON'T INCLUDE WEIGHTS so use summary
xtabs( ~ bush + state, data=pollsdata)


#LOOK AT OVERALL DATA
desc_stats_by_state <- pollsdata %>% group_by(state,bush) %>% summarise(n=n()) %>% mutate(pct = n/sum(n))
head(desc_stats_by_state)

bush_states <- desc_stats_by_state %>% filter(pct >= .5, bush==1) %>%  .$state          #BUSH WON 34 states
#bush_states     #CA TX FL OH PA MI NJ VA WA GA TN NC MO IN AZ LA AL KY SC MS WV KS CT CO OK NE AR UT ND SD NV NH VT WY

dukakis_states <- desc_stats_by_state %>% filter(pct >= .5, bush==0) %>%  .$state       #BUSH LOST 17 States
#dukakis_states  #NY IL WI MA MN MD NM IA OR ME AR RI DE SD MT ID DC

intersect(bush_states,dukakis_states)    #TIED IN TWO AR, SD
desc_stats_by_state %>% filter(state %in% c("AR","SD"))
#    state   bush     n   pct
# 1     AR      0     6   0.5
# 2     AR      1     6   0.5
# 3     SD      0     3   0.5
# 4     SD      1     3   0.5

desc_stats_by_state$overall = 0
desc_stats_by_state[desc_stats_by_state$state %in% bush_states,]$overall = 100

#view BUSH by STATE
ndf <- data.frame(desc_stats_by_state$state,desc_stats_by_state$overall)
colnames(ndf) <- c("state","n")
ggplot(desc_stats_by_state,aes(x=state,y=n,color=as.factor(bush))) + geom_point()  + geom_point(data=ndf,colour="red")   + ggtitle("bush ~ state")

#view BUSH by AGE
desc_stats_by_age <- pollsdata %>% group_by(age,bush) %>% summarise(n=n()) %>% mutate(pct = n/sum(n))
pa <- ggplot(desc_stats_by_age,aes(x=age,y=n,color=bush)) + geom_line() + geom_point()   
pa + ggtitle("bush ~ age  | across age groups more people say they'll vote for bush") 

#view BUSH by black
desc_stats_by_black <- pollsdata %>% group_by(black,bush) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) %>% arrange(black,desc(n))
pb <-ggplot(desc_stats_by_black,aes(x=black,y=n,color=as.factor(bush))) + geom_point(shape = 21, size = 10, stroke = .1)   + geom_text(aes(label=str_c(pct,"%"))) 
pb + ggtitle("bush ~ black  |  21.4% of blacks say they'd vote for bush.  58.6 of nonblacks say they would")

#view BUSH by edu
desc_stats_by_edu <- pollsdata %>% group_by(edu,bush) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) %>% arrange(edu,desc(n))
pe <- ggplot(desc_stats_by_edu,aes(x=edu,y=n,color=as.factor(bush))) + geom_point(shape = 21, size = 10, stroke = .1) + geom_text(aes(label=str_c(pct,"%"))) 
pe + ggtitle("bush ~ education | outside of No Highschool, people moreso say they'll vote for bush")

#view BUSH by female
desc_stats_by_female <- pollsdata %>% group_by(female,bush) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) %>% arrange(female,desc(n))
pf <- ggplot(desc_stats_by_female,aes(x=female,y=n,color=as.factor(bush))) + geom_point(shape = 21, size = 10, stroke = .1) + geom_text(aes(label=str_c(pct,"%"))) 
pf + ggtitle("bush ~ female | more women in sample.  54% would vote for bush.  For men its 57%")  


#FACET VIEW TO SEE STATES by AGE / BUSH
desc_stats_ab <- pollsdata %>% group_by(state,age,bush) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) 
p <- ggplot(desc_stats_ab, aes(x=age,y=n) )  + geom_point(aes(color=as.factor(bush)))  #+ geom_text(aes(label=str_c(pct,"%")))
p1a <- p + theme(axis.text.x = element_text(angle=90,size = 6)) + xlab("age") +ylab("Bush/number of obs") 
p1 <- p1a + facet_wrap( ~ state, ncol=9)
p1

#FACET VIEW TO SEE STATES by AGE / EDUCATION
desc_stats_ae <- pollsdata %>% group_by(state,age,edu) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) 
p2 <- ggplot(desc_stats_ae, aes(x=age,y=n) )  + geom_point(aes(color=as.factor(edu)))  #+ geom_text(aes(label=str_c(pct,"%")))
p2a <- p2 + theme(axis.text.x = element_text(angle=90,size = 6)) + xlab("age") +ylab("Education number of obs") 
p2 <- p2a + facet_wrap( ~ state, ncol=9, scales = "free_y")
p2

#FACET VIEW TO SEE STATES by AGE / FEMALE
desc_stats_af <- pollsdata %>% group_by(state,age,female) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) 
p3 <- ggplot(desc_stats_af, aes(x=age,y=n) )  + geom_point(aes(color=as.factor(female)))  #+ geom_text(aes(label=str_c(pct,"%")))
p3a <- p3 + theme(axis.text.x = element_text(angle=90,size = 6)) + xlab("age") +ylab("Female number of obs") 
p3 <- p3a + facet_wrap( ~ state, ncol=9, scales = "free_y")
p3

#FACET VIEW TO SEE STATES by AGE / BLACK
desc_stats_abl <- pollsdata %>% group_by(state,age,black) %>% summarise(n=n()) %>% mutate(pct = round(n/sum(n),3)) 
p4 <- ggplot(desc_stats_abl, aes(x=age,y=n) )  + geom_point(aes(color=as.factor(black)))  #+ geom_text(aes(label=str_c(pct,"%")))
p4a <- p4 + theme(axis.text.x = element_text(angle=90,size = 6)) + xlab("age") +ylab("Black number of obs") 
p4 <- p4a + facet_wrap( ~ state, ncol=9, scales = "free_y")
p4

#multiplot(p1,p2,p3,p4,cols=2)   #TOO HARD TO READ AND NOT TERRIBYLY CONVINCING

#numeric
#scatter/boxplot

#binary
#contingency table


xyplot(as.factor(bush) ~ age | state, data=pollsdata, type = c("p", "r"),  group = female, auto.key = list(lines = TRUE), par.strip.text=list(cex=.5), superpose = F)

#install.packages("GGally")
library(GGally)
forp <- pollsdata[,c(3,4,5,6)]
sapply(forp,class)
#forp$bush = factor(forp$bush)
forp$age = factor(forp$age)
forp$female = factor(forp$female)
forp$black = factor(forp$black)

ggpairs(forp)  #state has too many levels ( only allows for 15)  #ehhh



#BETTER TO VISUALIZE LOG ODDs between MEN/WOMEN and voting Republican


logit.m1 <- glm(bush ~ state, data=pollsdata, family = binomial(link='logit'))
summary(logit.m1)    #only NY, OH, VA, TN, AL, MD are significant,  AIC: 2747.4
anova(logit.m1, test="Chisq")


logit.m2 <- glm(bush ~ black, data=pollsdata, family = binomial(link='logit'))
summary(logit.m2)    #black significant, AIC: 2688.3

logit.m3 <- glm(bush ~ edu, data=pollsdata, family = binomial(link='logit'))
summary(logit.m3)    #eduSomeColl significant, AIC: 2763.6

logit.m4 <- glm(bush ~ female, data=pollsdata, family = binomial(link='logit'))
summary(logit.m4)    #female intercept significant, AIC: 2769.1

logit.m5 <- glm(bush ~ age, data=pollsdata, family = binomial(link='logit'))
summary(logit.m5)    #age intercept significant, AIC: 2768.9


logit.m6 <- glm(bush ~ black + edu + female + age, data=pollsdata, family = binomial(link='logit'))
summary(logit.m6)    #black, edu, age, but not female  significant, AIC: 2768.9

logit.m7 <- glm(bush ~ state + black + edu + female + age, data=pollsdata, family = binomial(link='logit'))
summary(logit.m7)    #state, black, edu, age, but not female  significant, AIC: 2768.9


#glmer : http://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/ 
#        http://stats.idre.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/   ****
#========================================================================================================================

m1 <- glm(bush ~ state + black + edu + age + female, data = pollsdata, family = binomial(link="probit"))
#


#A. Hierarchical linear model; which only allows intercept among states to change .
hlm1 <- glmer(bush ~ black + edu + age + female + (1 | state), data = pollsdata, family = "binomial") #(link="probit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(hlm1)   #black, some college, age30-44


# table of estimates with 95% CI
se <- sqrt(diag(vcov(hlm1)))
tab <- cbind(Est = fixef(hlm1), LL = fixef(hlm1) - 1.96 * se, UL = fixef(hlm1) + 1.96 * se)
tab

coef(hlm1)  #per state intercepts vary, but not coefficients for betas
fixef(hlm1)  
#(Intercept)       black       eduHS eduSomeColl     eduBacc   age30to44   age45to64   age65plus      female 
#0.31198997 -1.74427022  0.23296729  0.51612594  0.31601970 -0.29221165 -0.06757393 -0.22514508 -0.09681328 
ranef(hlm1) #random per state

#sort by 
ord <- order(summary(pollsdata$state,decreasing = T))
sts <- names(sort(summary(pollsdata$state),decreasing = T))  #ORDER BY NUMBER OF SAMPLES
plot(factor(rownames(ranef(hlm1)$state), levels=sts), ranef(hlm1)$state[[1]][ord])

#PLOT CONFIDENCE INTERVALS AROUND 
CI = confint(hlm1)[-c(1:2),]
CI = cbind(CI,'50%'=rowMeans(CI))

plotCI(x=CI[,3],li=CI[,1],ui=CI[,2],xlab='Coefficients',ylab='',main='Coefficient Estimates from LMER',xaxt='n')
axis(1,at=1:length(colnames(X)[-1]),labels=colnames(X)[-1],cex.axis=.75)
abline(h=0,lty=2,col='red')


tf <- data.frame(tab)
#ggplot(tf, aes(x = rownames(tf), y = Est)) + geom_linerange(aes(ymin = LL, ymax = UL)) + geom_line(size = 2) + ylim(c(0, 1))

dotplot(ranef(hlm1,which = "state", condVar = TRUE), scales = list(y = list(alternating = 0)),   as.table=TRUE, auto.key=list(space="top", column=4, cex=.8, title="Sample size",   cex.title=1, lines=TRUE, points=FALSE))




###CHECK JAMES VERSION OF POLLS DATA ON SITE ####  .. try albert and chib skills introducing Z

#JENNIFER's version ( Read albert paper)  .. effective sample size ( autocorrelation )  to understand how to choose number of iterations

#BECAREFUL sometimes if you just plot the intercept (mu_is) they are RELATIVE TO BASE LINE ( which is nonblack, no high school, young, etc)


#READ UP ON MCMCPack
vignette("MCMCpack")




#=======================Notes on Albert and Chib trick ==============
'''
IDEAL POINT MODEL ( used in Political Science to understand correlation structure for how legislative bodies vote! )
SPATIAL MODEL OF VOTING (not in terms of geography but a socio-economic 2 dimensional space)
axises f1 (x) and f2 (y)   ( liberal / conservative economic <--> on f1 ,  liberal / conservative social ^|| on f2)


y = matrix  
legislator i  as row
bill j  as column ( lots of bills / less legislators  and how each voted in every cell)

y_ij = 1 if legislator i voted yes on bill j

bernouli probability model      ------------Z_ij--------------
p(y_ij | alpha, beta, f) = Phi( alpha_j + beta_j1*fi1 + Bj2fi2)                        

       where 
        Phi = probit link function =  cdf of Normal Random Variable 
        (fi1,fi2)  = ideal point for legislator i = factors
(beta_j1, beta_j2) = loadins on each factor for bill j
           alpha_j = measures overall popularity of bill j  ( super popular bills are less informative )

This is a factor-probit model ( a factor model is just a regression model with unobserved covariates 
                                  since you dont know the ideal points f1,f2 for every legislator or the bills b1/b2 )

zj = (z_1j,z_2j ... z_nj)^T        ... legislator 1 to legislator n on bill j

zj = alpha_j * 1vec + FBj
   = alpha_j * 1vect + Fstar * Bjstar         ( infinite number of solutions because of rotation problem)

where F = ( f_11, f_12 )
             .. ... ..
          ( f_n1, f_n2 )

      Fstar = FA^T
     Bjstar = ABj
 and (A^T)A = I                Where A and A^T are orthogonal ( so their product is the identity matrix )

F((A^T)A)Bj = FBj

#FOR WEDNESDAY, BAYES VERSION OF POLLS ( Steven Jesse <---  polisci/stats at UT)


#DO BAYES , DO LIN, DO CODE REVIEW.

### CLASS 4/17

## LOOK AT GENOMIC STUFF BY NEXT CLASS ( Data exploration , etc.. )

#FACTOR ANALYSIS MODELS

y_ij


look at simplex plots ( to show three dimensions in two dimensions via a 2d triangle plot)
you always want to try and find correlation in your residuals ( once there isnt any left , your residuals are just noise)

'''



#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================

####QUESTION 4
#In droslong.csv, you will find a small subset of a time-course DNA microarray experiment. 
# The gene-expression profiles of 2000 different genes in the fruit fly (Drosophila) genome are tracked over time during embryogenesis; 

#  you are getting data on 14 of these genes, organized in three groups (think of these as marking which cellular pathway that gene influences). 

# For each gene at each time point, there are 3 "technical replicates", 
#     ie, three copies of the same biological material from the same fly, run through the same process to measure gene expression.

# The question of interest is: how does each gene’s expression profile change over time, as the process of embryogenesis unfolds? 

# Propose a hierarchical model for this data that properly reflects its structure. Fit this model using Gibbs sampling.

#DATA EXPLORATION
library("lattice")
library("dplyr")
library("ggplot2")
droslong <- read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/droslong.csv", header=TRUE)

dim(droslong)    #504  6
head(droslong)
#        gene  group    label   log2exp time replicate
# 1 142798_at group1 A_01.cel 15.448250    1         A
# 2 142472_at group1 A_01.cel 15.343945    1         A
# 3 143074_at group1 A_01.cel 15.412123    1         A

summary(droslong)
#         gene        group          label        log2exp            time       replicate
# 141251_at: 36   group1:180   A_01.cel: 14   Min.   : 6.273   Min.   : 1.00   A:168    
# 141311_at: 36   group2:180   A_02.cel: 14   1st Qu.:10.299   1st Qu.: 3.75   B:168    
# 141404_at: 36   group3:144   A_03.cel: 14   Median :12.878   Median : 6.50   C:168    
# 142200_at: 36                A_04.cel: 14   Mean   :12.483   Mean   : 6.50            
# 142342_at: 36                A_05.cel: 14   3rd Qu.:15.318   3rd Qu.: 9.25            
# 142356_at: 36                A_06.cel: 14   Max.   :15.468   Max.   :12.00            
# (Other)  :288                (Other) :420                                     

droslong %>% filter( gene == "142798_at")
#        gene  group    label  log2exp time replicate
# 1  142798_at group1 A_01.cel 15.44825    1         A
# 2  142798_at group1 A_02.cel 15.46265    2         A
# 3  142798_at group1 A_03.cel 15.46792    3         A
# ...
# 34 142798_at group1 C_10.cel 15.46801   10         C
# 35 142798_at group1 C_11.cel 15.46456   11         C
# 36 142798_at group1 C_12.cel 15.46561   12         C

#1. do I need to reorder any ordinal variables?     ie, levels(polls$edu) = c("NoHS", "HS", "SomeColl", "Bacc")

levels(droslong$gene)     #Is there an ordinal or spatial structure here?
# [1] "141251_at"   "141311_at"   "141404_at"   "142200_at"   "142342_at"   "142356_at"   "142472_at"   "142661_at"   "142709_s_at" "142791_at"   "142798_at"   "143051_at"  
#[13] "143074_at"   "143143_at"  

#order by group they are in
ord <- droslong %>% group_by(group, gene) %>% summarise(n=n()) %>% .$gene
levels(droslong$gene) <- ord

#levels(droslong$group)   Others ordered fine.
#levels(droslong$label)

#2. any groups with small number of  ; xtabs(~state, data=polls) , xtabs(~state+bush, data=polls)
#Nope, all genes have 12 time observations for each replicate A, B, C
#There are 5 genes in group1, 5 in group2, and 4 in group3 ( so 3 is slightly underrepresented.)

groups <- droslong %>% group_by(group, gene) %>% summarise(n = n())
#    group        gene     n
# 1  group1   142472_at    36
# 2  group1   142791_at    36
# 3  group1   142798_at    36
# 4  group1   143074_at    36
# 5  group1   143143_at    36
# 6  group2   141311_at    36
# 7  group2   141404_at    36
# 8  group2   142200_at    36
# 9  group2   142356_at    36
# 10 group2   143051_at    36
# 11 group3   141251_at    36
# 12 group3   142342_at    36
# 13 group3   142661_at    36
# 14 group3 142709_s_at    36

#3. any cofounders 

#SHOW INDIVIDUAL GENES SEPERATELY COLORED BY GROUP
xyplot(log2exp~time | gene, data=droslong, type = c("p", "r"),  group = group, auto.key = list(lines = TRUE)) #, par.strip.text=list(cex=.5))    #going over 14 genes, these all have variation

#SHOW INDIVIDUAL GROUPS COLORED BY GENE / REPLICATE
xyplot(log2exp~time | group + replicate, data=droslong, type = c("p", "r"),  group = gene, auto.key = list(lines = TRUE, space = "right"),superpose = F)   #the "gene" variation over time seems to be expressed by the group they are from though ( there are three groups)

# SO GROUP (3) AND GENE (14) BOTH VARY OVER TIME.

#more than 8 colors needed (ggplot defaults to 8 and we have 14 genes)
library("RColorBrewer")
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))
#colorRampPalette(brewer.pal(name="Dark2", n = 8))(14)

#+ geom_line(aes(colour=group)) 

#JUST REPLICATE A
ggplot( droslong %>% filter(replicate == "A"), aes(time,log2exp,colour=gene)) + geom_point() + geom_line() + scale_color_manual(values = colorRampPalette(brewer.pal(name="Dark2", n = 8))(17)) #+ geom_line(aes(colour=group))   ##GOOD

#ALL REPLICATES FACETED
ggplot( droslong, aes(time,log2exp,colour=gene)) + geom_point() + geom_line() + facet_wrap(~replicate ) + scale_color_manual(values = colorRampPalette(brewer.pal(name="Dark2", n = 8))(17)) #+ geom_line(aes(colour=group))   ##GOOD


##########NOW LMER



#4. what does glm give out of the box , glm1 = glm(bush~black + female + state, family=binomial(link="logit"), data=polls)
hlm1 = lm(log2exp ~ time + gene + group + replicate, data=droslong)
summary(hlm1)  #singularities in group*

#account for different slopes per gene and group
hlm2 = lmer(log2exp ~ time + replicate + ( 1 + gene | group), data=droslong)   #WARNINGS !! Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
summary(hlm2)  


#
hlm3 = lmer(log2exp ~ time * group + ( time | gene), data = droslong)
summary(hlm3)
coef(hlm3)

r = ranef(hlm3, condVar=TRUE)
dotplot(r)
dotplot(r, scales=list(relation='free'))

#http://rpsychologist.com/r-guide-longitudinal-lme-lmer  | #GOOD guide to longitudinal analysis
#Three Level model - group , gene, replicate , time  
hlm4 = lmer(log2exp ~ time*group + ( time | gene/replicate), data = droslong)
summary(hlm4)
coef(hlm4)

dros_fitted = droslong
dros_fitted$log2exp = fitted(hlm4)

p1 <- ggplot( droslong, aes(time,log2exp,colour=group)) + geom_point() 
p1 + geom_line(data=dros_fitted,colour="red")  + facet_wrap(~gene)  


#REPLICATE DOESN'T REALLY MATTER, BUT TIME IS CLEARLY NOT LINEAR
data_avg = droslong %>% group_by(gene,time) %>% summarise(log2exp = mean(log2exp), group = first(group)) %>% select(gene,time,log2exp,group)

hlm5 = lmer(log2exp ~ (time + I(time^2))*group + ( (time + I(time^2)) | gene ), data = data_avg)
summary(hlm5)
coef(hlm5)

#gene1pts = which(droslong$gene == "142472_at" & droslong$replicate =="A")
#gene1 = droslong[gene1pts,]
gene1pts = which(data_avg$gene == "142472_at")
gene1 = data_avg[gene1pts,]
plot(gene1$time, gene1$log2exp, col="blue",ylim=c(10,14))
lines(gene1$time,fitted(hlm5)[gene1pts],col="red")


#FOR NO REPLICATE DATA MODEL, SHOW EACH GENE OVER TIME AND THEIR FITTED LINES
data_fitted = data_avg
data_fitted$log2exp = fitted(hlm5)

#get GRAND MEANS
grandMeans = rep(data_avg %>% group_by(time) %>% summarise(log2exp = mean(log2exp)) %>% .$log2exp,14)   #TO SHOW HOW BAD THAT IDEA IS
data_grand = data_avg
data_grand$log2exp = grandMeans

#show GROUP LEVEL MEANS for data avg
getGroupMeans = function(groupname,groupMeans){ groupMeans %>% filter(group == groupname) %>% .$log2exp }
groupMeans <- data_avg %>% group_by(group,time) %>% summarise(log2exp = mean(log2exp)) 
data_group = data_avg
gg <- data_avg %>% distinct(gene,group)
gg = gg[,2]
data_group$log2exp  = as.vector(sapply(gg$group, function(x) getGroupMeans(as.character(x),groupMeans)))


#MAKE PLOT FOR EACH GENE USING NO REPLICATE/QUADRATIC TIME MODEL DATA
p1 <- ggplot( data_avg, aes(time,log2exp,colour=group)) + geom_point() + geom_line() 
p2 <- p1 + geom_line(data=data_fitted,colour="red") 
p2 + geom_line(data=data_group,aes(colour=group),linetype="dashed") + facet_wrap(~gene)  

#FOR NOW FINAL RESULTS




#HLM5's model (which does not take replicate into account) is the following
#lmer(log2exp ~ (time + I(time^2))*group + ( (time + I(time^2)) | gene ), data = data_avg)

# response Y is log2exp of the average of the replicates for a gene ( so now there is only 168 obs)
# i is the gene
# j is the group 
# B_0 diffent intercepts and slopes per group independent of time and allows for correlation between them
# B_1 accounts for linear time
# B_2 accounts for quadratic time

Y_ij = β_0j + β_1j*t_1ij + β_2j*(t_1ij)^2 + R_ij 

where 
  β_0j = gamma_00 + gamma_01 * GROUP_j + U_0j
  β_1j = gamma_10 + gamma_11 * GROUP_j + U_1j
  β_2j = gamma_20 + gamma_21 * GROUP_j + U_2j      

  U_0j = N(0, (tau_00)^2 tau_01    tau_02     )
  U_1j = N(0,  tau_01   (tau_11)^2 tau_12     )
  U_2j = N(0,  tau_02    tau_21    (tau_22)^2 )

  R_ij ~ N(0, sigma^2)

#TODO ASSIGN PRIORS FOR GAMMA/TAU AND DO MCMC

##4/19
##### BAYESIAN MODEL SELECTION

#not contriversial when you have scientifically driven priors ( and if you have prior scientific studies use their results as priors over this! )
#and more data can sometimes amplify the effect of a bad prior!  ( priors on model parameters)

#4 overall schools of thought for model selection ( philosophical positions .. most objective to least )
#-------------------------------------------------------------
#1. Complete Coherent approach to all model selection problems
#    ( Harold Jeffries (cambridge), E.T. James )
#    -- most people judge these efforts to be a failure

#2. Find the "best" method in some context 
#3. "Objective" methods are just conventions for when some appearance of "objectivity" is required
#4. Methods are ad-hoc and useful

#Jame's view is between 3 and 4
#Jose Banardo (spanish statistician)

#Data model
#Suppose our data is X
#There are q candidate data models of the form: 
#   M_i :  X has density f_i( X | theta_i)      ... theta_i is specific to model f_i ..
#     i element_of  1 ... q
#  theta_i element_of Theta_i


#A Bayesian model is completed by having a prior, pi_i(theta_i)

#Define the marginal likelihood ("evidence") as 
#   M_i(x) = integral ( f_i( x |theta_i) * pi_i(theta_i))  d(theta_i)

#If each model has prior probability a_i, then
# the poster probability that "model i is correct"

# is P( gamma = i | X) = [ a_i * M_i(x) ]  / sum(j=1 to q)a_j * M_j(x)

# where gamma is the model indicator

# Bayes factor: B_ji = M_j(x) / M_i(x)           !!!!
# which is like the usual, Likelihood ratio which is ratio of two models mles
# also like a prior odds ratio.

# in Bayes analysis, M_i(x) is what you use to assess something is correct!    All you can compare is marginals.
# whereas in frequentist its [ f_i(theta_i), pi_i(theta_i) ]


###############EXAMPLE        ( bayes just gives us a probability distribution over data to use for prediction )
# Model M_1 :  y element_of ( -Epsilon, Epsilon )       
# Model M_2 :  y element_of ( -10Epsilon, 10Epsilon)

If y = 3 Eps , then p( M_1 | y ) = 0
If y = 0,
  under M_1: p(y | M_1) = 1 / 2Epsilon
  under M_2: p(y | M_2) = 1 / 20Epsilon
  
so P(M_1 | y) =  (1 / 2Epsilon) / ( 1/2Epsilon + 1/20Epsilon )  =  10/11 in favor of M_1  
                                                                  ( because M_1 makes a much stronger, concentrated prediciton around the interval -E to E)
Bayes model selection penalizes "vagueness"  (ie, Model 2)
"Vagueness" comes from the prior

#########################EXAMPLE 2

M_1 :  y ~ N(0, Sigma^2)
M_2 :  (y | theta) ~ N(theta, Sigma^2)
      where Sigma_2 = 1
      needs a prior
      eg.  theta ~ N(0, tau^2)

marginal of M_1:  M_1(y) = N(y| 0,1)   |  nothing to marginalize out since there are no hyper parameters
marginal of M_2:  M_2(y) = N(y | 0, 1 + tau^2)

Bayes Factor
BF_2,1 = ( 1 / sqrt(tau^2 + 1))  * exp(-y^2/ ( 2(tau^2) + 1)) 
            ------------------------------------------------
              exp( -y^2 / 2)
    
       =     1 / sqrt(tau^2 + 1) * exp(y^2/2 * tau^2/ tau^2 + 1)

so here you have to choose something remotely to scale for tau^2  because it influences your Bayes Factor 

Lindley paradox

###SENSIBLE IDEAS
# 1. Never Ever Use Vague Priors for Model Selection ( so never use a Normal Distribution with use Variance )
# 2. Focus on the Predictive Distribution M_i(x) 

###WHY DO IT
#1. Posterior Model probabilities are easier to understand than p-values
#2. Bayesian Model Selection is consistent ( subject to getting the priors right and some other regularity conditions .. Berk 1966 paper)
#3. Bayesian Model Selection gives an automatic occam's razor effect. ( reward models that are simple, and sharp (ie, not vague.. ie, spread thin over wide range))
#4. Approach is conceptually the same in all circumstances.
#5. Approach can account for model uncertainity !
#      Recall posterior predictive distribution of new data given old data
#         r_i(y_star | y ) = integral( f_i(y* | theta_i)) pi_i(theta_i | y ) d(theta_i)
#                                        sampling model    model i posterior    
#
#      Model Average predictions:
#        say we have models 1 ... q  
#        with posterior probabilities c_hat_i , given y.                      | p( gamma = i | y)
#         r_tilda(y_star | y) = sum(i=1 to q)[ c_hat_i * r_i(y_star | y) ]    #Model Average predictions.. integrates out which model is true ( average over undercertainty )
#         
#6. Bayesian approach can yield optimal frequentist procedures. ( review paper by Berger and Perichi 2001 on website)


###DIFFICULTIES/CHALLENGES
#1. Computation of marginal likelihoods is difficult ( integration is harder than derivation and mle)
#    ( Nested sampling is the best approach but alot harder to set up in Monte Carlo Markov Chains )
#      --> Energy-level sampling methods in physics ( see James review paper!!  great technique that should be used more possibly )
#
#2. Improper priors yield indeterminant answers
#    example.  y ~ N(theta, 1)
#              pi(theta) properto 1
#              M(y) = integral( N(y(Theta,1) * 1 d(theta))       <-- doesn' integrate to anything so that 1 is your likelihood 
#
#3. Use of vague proper priors give terrible answers
#4. The meaning of "common" parameters can change from model to model
#   example.   y: mpg ,   x_1: Horse Power, x_2: Weight
#           M_1:  y_i = alpha + B_1*X_1i + E_i                      // whats the overall relations between MPG and horsepower for B1
#           M_2:  y_i = alpha + B_1*X_i  + B_2*X_i2 + E_i           // here B1 is what the overall relation between MPG and horsepower keeping Weight constant

#classically you could do an f-test

#3 Approaches to Actually Specifying Priors in Bayesian Computation:   #Default "Objective" Approaches
#------------------------------------------------------------------
#1. Convention prior approach ( case by case basis )
#    example: Data = X = (X1 ... Xn)
#   M_1: ( X_i | sigma_1^2) ~ N(0, sigma_1^2)
#   M_2: ( X_i | theta, sigma_2^2) ~ N(theta, sigma_2^2)

#Most famous answer:  Jeffries ( 1961 )  
# For sigma
#   say that sigma_1^2 = sigma_2^2 = sigma_2 ("make sigma parameter the same")
#           and use pi_(sigma^2) = 1/sigma_2 ( only time its ok to use a vague prior is when all models use that)
# 
# For pi_(theta) it "should" 
#   - be centered at 0
#   - be symmetric
#   - be scaled by sigma ( standard deviation of the error distribution, ie sampling varaince)
#   - be heavy tailed  ( to allow it to be consistent)  .. ( no integer moments)
# Candiate: Cauchy
# pi_(theta | sigma) = Cauchy( 0, sigma)
#                    = 1 / ( 3.14*sigma*( 1 + theta^2/sigma^2))

################################################################################
#Rob Tibshirani Lecture ( department of biomedical data sciences and statistics )
#Some Progress and Challenges in Biomedical Data Science
#google index

#Livia Eberlin presented Rob 
#deeplearning, sparse modelling
#hal varian, google chief economist
#stochastic gradient descent
#mentioned Bengio Deep Learning new book ( from last 6)
#DL good for data with spatial organization 

#3 Personalized medicine | 
#   --> find patients like our current one ( who have similar diseases ) and then see how they have done with certain drugs related to a disease the initial person has
#   --> have to be careful because things arent randomized ( maybe all super sick patients got one pill, while others got anohter... need to infer how drugs where given!)

#a sparse set of features. Lasso **** "glmnet"   ***** (convex optimization problem)
#| idea of having a budget of penalty  for your weights ( otherwise you could overfit easy).. Least squares with a budget
#absolute value of penalty is what gives you sparsity...

#for continous problems "relaxed lasso" can be good

#cancer detection is not a symmetric loss situation (its way worse to classify cancer as not cancer than vice versa so your loss function)
#challenge ob absentations ( when should the prediction ssy I dont know.)
#look into isolation forests and causal random forests ( paper by: ahey, imbens, wager )

#thoughts on Robs lecture 
#desi-mass spectometry
#how to create a classifier that says "i dont know"
#critiques of a persons presentation can be a good way to come up with new ideas for research questions/suggesting improvements!

#spatial smoothers for sharp discontinuity between neighbors
#using neighboring pixels classes as potential features
#fundamentally particle spaces are discrete states
#you can exploit correlation along an axis if it is an "ordered" axis ( time, etc)

#possibly using lasso to get potential features and then doing ols on that ( as opposed to doing classification and doing lasso )
#winners curse problem

#LOOKUP ** "adaptive lasso" (state of the art),  better than just standard lasso
#
#1) run ols, 
#2) take absolute value of Betas, and 
#3) then create penalities that are inverse to size of Beta
#4) and now run lasso with these penalities.



#supervised learning via the lasso
###############################################

#0405  Normal Presentation For Bayes 
# 1) explatory analysis
# 2) simple model to show need for random effects
# 3) full model with mcmc steps
# 4) findings 


####
#Continued Cheese by Spencer (mira al codigo y al parte escrito !!!)

#"Matrix"  package to handle sparse matrices 
#Monday MLE version (lmer / glmer for polls)
#Wednesday Polls Problem (fully bayes ) and Peer Review ( for cheese problem )



# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}