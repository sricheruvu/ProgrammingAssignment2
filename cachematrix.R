## Put comments here that give an overall description of what your
## functions do

## The following function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## first init the inverse property
  inv <- NULL
  
  ## next is the method to 'set' the Matrix
  set <- function( matrix ) {
    x <- matrix
    inv <<- NULL
  }
  
  ## method to 'get' the Matrix
  get <- function () {
    ## return the matrix
    x
  }
  
  ## method to 'set' the inverse of the Matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## method to 'get' the inverse of the Matrix
  getInverse <- function(){
    ## return the Inverse property
    inv
  }
  
  ## lastly return the list of methods in this cache function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
} ## end of makeCacheMatrix function


## Here we compute the inverse of the special matrix returned by the above
## makeCacheMatrix function. Assuming that the matrix has not changed AND the 
## inverse has already been calculated, the cacheSolve function below should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## check if it is already set, if yes, return the inverse
        if(!is.null(m)) {
          message("Inverse already in cache. Getting the cached inverse")
          return(m)
        }
        
        ## now retrieve the matrix from the object
        m_data <- x$get()
        
        ## next calculate the inverse
        m <- solve(m_data) %*% m_data
        
        ## set the inverse now
        x$setInverse(m)
        
        ## done with all the work, return the matrix
        m
        
} ## end of cacheSolve function
