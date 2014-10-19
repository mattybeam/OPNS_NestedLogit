#Nested logit coding assignment
source('header.R')

#Load data
val<-readRDS(paste0(varSave, 'choice_values.rds'))
lambda<-readRDS(paste0(varSave, 'lambda_values.rds'))

#Generate random draws
size = 10000
eps<- 
  rdply(3,rgev(size, xi = 0, mu = 0, beta = 1)) %>% 
  select(-1) %>% 
  t %>% 
  as.data.frame
colnames(eps)<- c(1,2,3)
eta<- 
  rdply(3,rgev(size, xi = 0, mu = 0, beta = 1)) %>% 
  select(-1) %>% 
  t %>% 
  as.data.frame
colnames(eta)<- c("A","B","C")

#Method 2
types<- data.frame(val[,1:2])
val<- dcast(val, bucket ~ choice, mean)

U2<-
  mdply(types,
        function(bucket, choice) 
          val[bucket, toString(choice)] + eps[,bucket] + lambda[bucket,"lambda"]*eta[,toString(choice)])


