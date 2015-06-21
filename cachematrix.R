## Creating a cache inverse matrix and calculating inverse matrix if not present


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_mat = NULL
  set <- function(y){
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_mat <<- inverse
  getinverse <- function() inverse_mat 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse_mat <- x$getinverse()
  if(!is.null(inverse_mat)){
    message("getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data, ...)
  
  x$setinverse(inverse_mat)
  inverse_mat
  
}
