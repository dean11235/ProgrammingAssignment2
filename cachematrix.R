## 
## the two functions are supposed to create a matrix that can cache its inverse
##and compute the inverse. if already computed, the inverse would be
##retrieved from cache. They look really similar to the samples (makeVector and cachemean)


## the makeCacheMatrix is written to create a list of functions that:
# 1 - set the matrix
# 2 - get the matrix
# 3- set the matrix inverse
# 4- get the matrix inverse

makeCacheMatrix <- function(x = matrix()) { #x must be a matrix
  inv <- NULL # will hold the value to inverse of the matrix
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  set_Inverse <- function(inverse){inv <<- inverse}
  get_Inverse <- function(){inv}
  
  list(set = set, get = get, 
       set_Inverse = set_Inverse, get_Inverse=get_Inverse)
}

## calculate the inverse of the above matrix, but first check the cache
##if inverse's not calculated, it'd calculate it and put in cache, 
##if so, ttake the data from cache

cacheSolve <- function(x, ...) {
  inv <- x$get_Inverse()
  
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  
  m <- x$get() 
  inv <- solve(m,...) #use solve function to compute inverse
  x$set_Inverse(inv)
  
  inv
}



