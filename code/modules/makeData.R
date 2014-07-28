makeData<-function(){
  bucket=1:3
  choice=c('A', 'B', 'C')
  expand.grid(bucket=bucket, choice=choice) %>%
    mutate(val=rnorm(n())) %>%
    easy.save('choice_values.R')
  
  cbind(bucket=bucket, data.frame(lambda=runif(length(bucket)))) %>%
    easy.save('lambda_values.R')
}
