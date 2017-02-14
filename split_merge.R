#R code for split-and-merge approach for Single Value Decomposition(SVD)
# X is our data variable
split_merge <- function(X){
  m <- nrow(as.matrix(X))
  x <- as.matrix(X)
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
      xi <- x[i:(i+part_len-1), ]
      }
    else{
      xi <- x[i:m,]
      }
      
    xi.svd <- svd(xi)               #Performing svd on individual submatices
    U1 <- rbind(U1,xi.svd$u)        #Updating U bar matrix
    yi <- xi.svd$v %*% diag(xi.svd$d)     #Creating y=v*d
    Y <- rbind(Y,yi)                      #Updating the y matrix
    }
    
    y.svd <- svd(Y)
    X$u <- U1 %*% y.svd$u
    X$v <- y.svd$v
    X$d <- diag(y.svd$d)
    return(X)
    }
