## This makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    tmp <- NULL
    set <- function(y) {
        x <<- y
        tmp <<- NULL
    }
    get <- function() x
    setinv <- function(inv) tmp <<- inv
    getinv <- function() tmp
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()      
    inv <- solve(data)
    x$setinv(inv)
    inv
  ## Return a matrix that is the inverse of 'x'
}
