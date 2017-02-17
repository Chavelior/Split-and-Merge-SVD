#R code for split-and-merge approach for Single Value Decomposition(SVD)
# X is our data variable


split_merge <- function(X){
  
  x <- as.matrix(X)
  m <- nrow(x)
  Y <- NULL               #Initializing the end Y matrix
  U1 <- NULL              #Initializing the U bar matrix form X
  xi <- NULL
  
  #Forming sub-matrices from X by row slicing
  #Number of rows in each sub-matrirx has to be approximately same, so, I'm considering the root of intial rows in data
  i <- 1                    #Keeping track of rows in the original data
  part_len <- as.integer(sqrt(m))
  
  #loop through the data
  while ( i <= m ){
    
    if( i+part_len-1 < m ){
      xi <- as.matrix(x[i:(i+part_len-1), ])       
    }
    else if(i != m) {
      xi <- as.matrix(x[i:m, ])
    }
    else{
      xi <-matrix((x[i,]), nrow = 1)      
    }
    fin<-NULL
    xi.svd <- svd(xi)                     #Performing svd on individual submatrices
    if(is.null(U1)){                      #When first submatrix is being used
      fin<-xi.svd$u
    }
    else                                 #We need to add U matrices diagonally
    {
    coly<-ncol(xi.svd$u)                 #extending rows and columns by row binding and column binding
    subyup<-matrix(0,nrow(U1),coly)
    x1<-cbind(U1,subyup)
    subydown<-matrix(0,nrow(xi.svd$u),ncol(U1))
    subydowncom<-cbind(subydown,xi.svd$u)
    fin<-rbind(x1,subydowncom)
    }
    U1 <- fin        #Updating U bar matrix
    d<-diag(xi.svd$d, nrow = length(xi.svd$d))
    yi <- d %*% t(xi.svd$v)    #Creating y=v*d
    Y <- rbind(Y,yi)                      #Updating the y matrix
    i <- i+part_len
  }
  
  y.svd <- svd(as.matrix(Y))
    #
  svd(X)$u <- U1 %*% y.svd$u
  svd(X)$v <- y.svd$v
  svd(X)$d <- diag(y.svd$d, nrow = length(y.svd$d))
  return(X)
  
    }
