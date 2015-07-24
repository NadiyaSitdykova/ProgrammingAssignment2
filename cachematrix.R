## This module contain two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## This special "matrix" object is really a list containing four functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinv <- function(lolo) inv <<- lolo
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## First, in checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the calculation. 
## Otherwise, it computes the inverse of the data and sets the value of the inverse in the cache
## via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cashed data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
