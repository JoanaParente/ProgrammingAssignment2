## A pair of functions to catch the inverse of a matrix

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
## define the behaviors or functions for objetcs of type makeCacheMatrix ()
    set <- function(matrix) {
    x <<- matrix
    m <<- NULL
}
get <- function() {
    x
}
setInverse <- function(inverse) {
    m<<- inverse
    }
getInverse <- function() {
    m
    }
## create a new object by returning a list()
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been  
## calculated (and the matrix has not changed),then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    m <- x$getInverse()
    ##check if the result is null
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ##calculate the inverse using matrix multiplication
    m <- solve(data)%*%data
    ##set the inverse to the object
    x$setInverse(m)
    ## Return a matrix that is the inverse of 'x'
        m
}
