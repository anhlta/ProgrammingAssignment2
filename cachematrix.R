## Programming assignment 2 - R Programing course
## This file contains functions implementing special matrix which is able to 
## cache its inversed value

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Variable to store inverse value of matrix
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(solve) s <<- solve
    
    getSolve <- function() s
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    # Try to get inverse value from cache first
    s <- x$getSolve()
    
    # If found in cache
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # if not found
    data <- x$get()
    
    # Calculate inverse value and cache it
    s <- solve(data, ...)
    x$setSolve(s)
    
    s
}
