## Coursera - R Programming - Spring 2020
## Programming Assignment 2
## Author: Di Pei
## 
## makeCacheMatrix: the function to create a special 
## "matrix" object that can cache its inverse.
##
## cacheSolve: the function to compute the inverse of 
## the special "matrix" returned by makeCacheMatrix. If 
## the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    ## Return a list containing 4 functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    ## Return inversed matrix
    inv
}

## A test for a 2x2 matrix
cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
