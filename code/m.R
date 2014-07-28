#Nested logit coding assignment:
#Simulate the nested logit, described in section 4.2 of Train (2009).
#Use the lambda values given in the lambda_values.R file of the variables folder. 
#Use the baseline choice values given in the choice_values.R file of the variables folder. These values correspond to the Ynk values of equation (4.3) of Train (2009) (the corresponding Wnk values are zero). 
#Using the rgev() function of the library('fExtremes') package, simulate 1,000 choices using:
#   (1) the two-stage choice framework outlined in section 4.2.3 of Train (2009).
#   (2) a single stage choice framework, where the value of choice i of bucket k is Vi+epsilon_k+lambda_k*eta_i, where epsilon_k and eta_i are independent Gumbel distributions.
#For each case, estimate the choice probabilities with your simulated choices
#Conduct a Wald test with the null hypothesis that the estimated probabilities equal the theoretical probabilities given in equation (4.2) of Train (2009).

source('header.R')

#makeData()
val=easy.load('choice_values.R')
lambda=easy.load('lambda_values.R')

