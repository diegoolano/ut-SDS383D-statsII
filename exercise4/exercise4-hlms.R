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

#SO BOOKS TO GET ARE: Gelman, Andrew. Bayesian data analysis.   (2014)   and Simon Jackman. Bayesian analysis for the social sciences ( 2009 )
#3. You are welcome to use the logit model instead of the probit model. 
#         If you do this, you’ll need to read the following paper, rather than Albert and Chib: 
#         Polson, N.G., Scott, J.G. and Win- dle, J. (2013). Bayesian inference for logistic models using Polya-Gamma latent variables. J. Amer. Statist. Assoc. 108 1339–1349.

pollsdata = read.csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/exercise4/polls.csv", header=TRUE)



###############
#04/03 class
'''
halft.R
a = 3
NMC = 10000

10000 normals time 10000 inverse gamma  ( t3  via redundant t)

shows via monte carlo that the redundant parameterization corresponds to the initial one

schools_halfcauchy.R    #LOOK AT THIS FOR HIS FULL BAYESIAN SOLUTION TO CHEESE

Half Cauchy is better than Inverse Gamma cause it doesn't force tau.sq to zero!! <--- number one reason why half cauchy is better than inverse gamma 
#half cauchy better than normal because normal decays too slow
'''






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




