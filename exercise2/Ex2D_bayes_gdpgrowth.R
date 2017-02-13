#Exercises 2.  Problem D
#Bayes Linear Model

#Take a look at the data in gdpgrowth.csv from the class website, which has macroeconomic variables for several dozen countries. 
#consider a linear model (with intercept) for a country's GDP growth rate (GR6096) versus its level of defense spending as a fraction of its GDP (DEF60).

#Inspect the fitted line (graphically). Are you happy with the fit?  Why or why not?

library(dplyr)
library(readr)
library(mvtnorm)
gdp_data <- read_csv("/Users/dolano/htdocs/ut-james-scott/statsII/spring2017/r/gdpgrowth.csv")

#Fit the Bayesian linear model to this data set, choosing L = I and 
#something diagonal and pretty vague for the prior precision matrix K = diag(k1, k2). 

#       ( y | B, s2 ) ~ N(XB, (wL)^-1)
#               (B|w) ~ N( m, (wK)^-1)
#   	              w ~ Gamma(d/2,eta/2)

# things to keep in mind
# - posterior (y | B, s2) = (B,w | y) * p(y)
# - joint (B,w) = (B|w) * (w)
   
#  From Conjugate Gaussian Linear Model: Basics , problem (A)
#  joint posterior = sampling model * joint prior 
#       p(B,w | y) =     p(y | B,w) * p(B, w)            //proportional to, via bayes rule
#                  =     p(y | B,w) * [ p(w) * p(B|w) ]
#       
#       this then integrates to give us, something of the form
#                  =     p(B | y,w) * p(w)              // so basically you are integrating out omega based on prior omega
#                            Normal * Gamma
            
#  From problem (B),
#  marginal posterior  p(w | y) = integral of p(B,w | y) with respect to B
#       this gives us  p(w | y) ~ Gamma(d*/2, eta*/2)

#  From problem (C),
#  marginal posterior  p(B | y) = integral of p(B,w | y) with respect to w
#       this gives us  p(B | y) ~ multivariate centered, scaled T distribution = t(m*, d*, (eta*/d*)(K*)^-1 )
#            thus the posterior mean for B = m* .

#   y is an n vector of responses, 
#   X is an n x p matrix of data features, and 
#   w = 1/s2 is the error precision (omega ), and 
#   L is some known matrix (here, Identity matrix)

#   XB is a prior guess for y.
#   m is the vector of means (ie, prior guess for B)
#   K is a p x p precision matrix in the multivariate normal prior for B, which we assume to be known.
#   d is like a "prior sample size" for the error variance sigma2.
#   eta  is like a "prior sum of squares" for the error variance sigma2.

n = nrow(gdp_data)             
p = ncol(X)                           
X = cbind(rep(1,n),gdp_data$DEF60)    #feature:  defense spending  ( X is n x p) , where p = 2 ( vector of 1's and defense spending )
y = gdp_data$GR6096                   #response: growth rate

L = diag(n)	            # Lambda = I , which is n x n identity matrix (ones along diagnol... 79 x 79 here)
m <- rep(0, p)          # vector of prior means on B
d = .01                 # prior sample size
eta = .01               # prior sum of squares
K = diag(c(.01,.01))    # precision matrix on B which is p x p ( 2 x 2 here)

#Update posterior parameters from B.
d_star = d+n
K_star = (t(X) %*% L %*% X) + K
m_star = solve(K_star) %*% (t(X) %*% L %*% y + K %*% m)
eta_star = eta + t(y) %*% y + t(m) %*% K %*% m - t(m_star) %*% K_star %*% m_star

beta_hat_post = m_star     # from C posterior beta_hat estimate is posterior mean of beta.

#Bayesian lm plot
plot(X[,2],y,col='black', xlab='Defense Spending',ylab='GDP Growth Rate')
abline(a=beta_hat_post[1],b=beta_hat_post[2],col='blue')


#Compare with Frequentist Linear Model
flm = lm(gdp_data$GR6096 ~ gdp_data$DEF60)
beta_hat = flm$coefficients

plot(X[,2],y,col='black', xlab='Defense Spending',ylab='GDP Growth Rate')
abline(a=beta_hat[1], b=beta_hat[2],col='green')
abline(a=beta_hat_post[1],b=beta_hat_post[2],col='blue')
legend('topright',legend=c("Frequentist LM","Bayesian LM"),lwd=2,lty=1,col=c('green','blue'))

summary(freq_lm)
'''
Residuals:
      Min        1Q    Median        3Q       Max 
-0.048524 -0.011655 -0.000705  0.008314  0.056712 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    0.011768   0.003073   3.830  0.00026 ***
gdp_data$DEF60 0.206506   0.084492   2.444  0.01681 *  

Residual standard error: 0.01885 on 77 degrees of freedom
Multiple R-squared:  0.07199,  Adjusted R-squared:  0.05994 
F-statistic: 5.974 on 1 and 77 DF,  p-value: 0.01681
'''

#The frequentist and bayesian linear fits are pretty close.  
#They are both pretty bad ( R-square at .07) and influenced by outliers (far right point specifically)
