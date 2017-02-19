replicatio_function <- function(){
  source("split_merge.R")
  set.seed(123)
  X<-matrix(rnorm(50000),ncol = 100,nrow = 500,byrow = T)
  usual.svd<-svd(X)
  new.ago<-split_merge(X)
  l<-lineprof(split_merge(X))
  return(list("usvd"<-usual.svd,"nago"<-new.ago,"l1"<-l))
}
