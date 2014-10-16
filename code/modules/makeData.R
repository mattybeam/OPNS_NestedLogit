makeData<-function(){
  bucket=1:3
  choice=c('A', 'B', 'C')
  expand.grid(bucket=bucket, choice=choice) %>%
    mutate(val=rnorm(n())) %>%
    saveRDS(paste0(varSave, 'choice_values.rds'))
  
  cbind(bucket=bucket, data.frame(lambda=runif(length(bucket)))) %>%
    saveRDS(paste0(varSave, 'lambda_values.rds'))
}
