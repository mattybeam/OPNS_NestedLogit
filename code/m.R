#Nested logit coding assignment
source('header.R')

#Load data
val<-readRDS(paste0(varSave, 'choice_values.rds'))
lambda<-readRDS(paste0(varSave, 'lambda_values.rds'))

# Simulate 10,000 choices corresponding to V_ik and lambda_k values: 
N <- 10^5
epsilon <- rgev(n = nrow(lambda)*N, xi = 0, mu = 0, beta = 1)

eta <- rgev(n = length(unique(val$choice))*N, xi = 0, mu = 0, beta = 1)

# 1 under the traditional specification where epsilon_k resolves before eta_i
IV <- left_join(val, lambda, by = "bucket") %>%
      group_by(bucket) %>%
      summarise(lambda = unique(lambda), iv = log(sum(exp(val/lambda)))) %>%
      mutate(iv = lambda * iv)
nest_choice <- data.frame(
  "bucket" = apply(IV$iv + matrix(epsilon,nrow = 3,byrow = TRUE),2,which.max)
)

# nest_choice <- ldply(seq(N), function(x){
#   util = IV$iv + rgev(n = nrow(IV), xi = 0, mu = 0, beta = 1)
#   which.max(util)
# }) 

item_choice1 <- data.frame(
  "bucket" = nest_choice$bucket,
  "choice" = LETTERS[
    apply(
      matrix(
        left_join(nest_choice,val,by="bucket")$val,
        nrow = length(unique(val$choice)),
        ncol = N) + 
        matrix(eta,nrow = 3,byrow = TRUE),
      2,which.max)
    ]
)

# 2 under the single-shot specification where epsilon_k and eta_i both resolve up front  
a <- matrix(eta,nrow = 3,byrow = TRUE)
U2 <- apply(
  val$val + 
  matrix(rep(epsilon,3),nrow = 9, byrow = TRUE) + 
  rep(lambda$lambda,3) * 
  a[rep(seq(nrow(a)),each = 3),],2,which.max
)
item_choice2 <- data.frame(
  "bucket" = val$bucket[U2],
  "choice" = val$choice[U2]
)


# Estimate choice probabilities and calculate covariance matrix using bootstrap

# 1  under the traditional specification where epsilon_k resolves before eta_i
Prob <- function(pick){
  data.frame(
    ProbA1 = with(pick,sum(bucket == 1 & choice == "A")/nrow(pick)),
    ProbB1 = with(pick,sum(bucket == 1 & choice == "B")/nrow(pick)),
    ProbC1 = with(pick,sum(bucket == 1 & choice == "C")/nrow(pick)),
    ProbA2 = with(pick,sum(bucket == 2 & choice == "A")/nrow(pick)),
    ProbB2 = with(pick,sum(bucket == 2 & choice == "B")/nrow(pick)),
    ProbC2 = with(pick,sum(bucket == 2 & choice == "C")/nrow(pick)),
    ProbA3 = with(pick,sum(bucket == 3 & choice == "A")/nrow(pick)),
    ProbB3 = with(pick,sum(bucket == 3 & choice == "B")/nrow(pick)),
    ProbC3 = with(pick,sum(bucket == 3 & choice == "C")/nrow(pick))
  )
}
Prob1 <- Prob(item_choice1)
   
boot <- function(pick){
  new_choice <- pick[sample(1:nrow(pick),100,replace = TRUE),]
  Prob_new <- Prob(new_choice)
  Prob_new
}

# 2 under the single-shot specification where epsilon_k and eta_i both resolve up front
Prob2 <- Prob(item_choice2)




# True probability
Prob_bucket <- function (bucket){
  Pbucket = exp(IV$iv[bucket])/sum(exp(IV$iv))
  return (data.frame("bucket" = 1:3,Pbucket))
}


Prob_choice_bucket <- function (choice,bucket){
  Pchoice <- exp(val$val[val$choice == choice & val$bucket == bucket])/sum(exp(val$val[val$bucket==bucket]))
  return (Pchoice)
}

trueProb <- left_join(
  ddply(val,.(bucket,choice),summarise,PchoiceBucket = Prob_choice_bucket(choice,bucket)),
  Prob_bucket(1:3),
  by = "bucket"
) %>% 
  mutate(Pchoice = PchoiceBucket * Pbucket) %>%
  select(bucket,choice,Pchoice)




















