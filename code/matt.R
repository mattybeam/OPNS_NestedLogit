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
Prob1 <- data.frame(
  ProbA1 = with(item_choice1,sum(bucket == 1 & choice == "A")/nrow(item_choice1)),
  ProbB1 = with(item_choice1,sum(bucket == 1 & choice == "B")/nrow(item_choice1)),
  ProbC1 = with(item_choice1,sum(bucket == 1 & choice == "C")/nrow(item_choice1)),
  ProbA2 = with(item_choice1,sum(bucket == 2 & choice == "A")/nrow(item_choice1)),
  ProbB2 = with(item_choice1,sum(bucket == 2 & choice == "B")/nrow(item_choice1)),
  ProbC2 = with(item_choice1,sum(bucket == 2 & choice == "C")/nrow(item_choice1)),
  ProbA3 = with(item_choice1,sum(bucket == 3 & choice == "A")/nrow(item_choice1)),
  ProbB3 = with(item_choice1,sum(bucket == 3 & choice == "B")/nrow(item_choice1)),
  ProbC3 = with(item_choice1,sum(bucket == 3 & choice == "C")/nrow(item_choice1))
)
                    


# 2 under the single-shot specification where epsilon_k and eta_i both resolve up front
Prob2 <- data.frame(
  ProbA1 = with(item_choice2,sum(bucket == 1 & choice == "A")/nrow(item_choice2)),
  ProbB1 = with(item_choice2,sum(bucket == 1 & choice == "B")/nrow(item_choice2)),
  ProbC1 = with(item_choice2,sum(bucket == 1 & choice == "C")/nrow(item_choice2)),
  ProbA2 = with(item_choice2,sum(bucket == 2 & choice == "A")/nrow(item_choice2)),
  ProbB2 = with(item_choice2,sum(bucket == 2 & choice == "B")/nrow(item_choice2)),
  ProbC2 = with(item_choice2,sum(bucket == 2 & choice == "C")/nrow(item_choice2)),
  ProbA3 = with(item_choice2,sum(bucket == 3 & choice == "A")/nrow(item_choice2)),
  ProbB3 = with(item_choice2,sum(bucket == 3 & choice == "B")/nrow(item_choice2)),
  ProbC3 = with(item_choice2,sum(bucket == 3 & choice == "C")/nrow(item_choice2))
)











