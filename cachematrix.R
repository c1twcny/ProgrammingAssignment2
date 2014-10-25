##############################################################################
## Two functions are used to calculate the inverse of a matrix and cache 
## the result.
## 
## makeCacheMatrix()
##     Input:  numerical vector or matrix
##     Output: a list containing 4 functions [set,get,setinverse,getinverse]
##     Functions: 
##          (1) set(): input new numerical vector or matrix
##          (2) get(): get original vector or matrix
##          (3) setinverse(): store inverse
##          (4) getinverse(): retrive inverse
##
## cacheSolve()
##     Input:  matrix derived from makeCacheMatrix()
##     Output: cached result of inverse matrix
##     Functions:
##          (1) Check whether inverse has been calculated (getinverse())
##          (2) Otherwise, caculate the inverse and store the result
##              in the cache (setinverse())  
##############################################################################
##
## Created by: TWC
## Date:       Oct. 2014
## Version:    0.1.0
## License:    GNU GPL-3.0 

##
## makeCacheMatrix()
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set       <- function(y) {
        x <<- y
        m <<- NULL
    }
    get        <- function() {
        if (!is.matrix(x)) x <- matrix(x) # convert x to a matrix
        if (sqrt(length(x))%%1 == 0) { # test whether x can be a square matrix 
            x <- matrix(x, sqrt(length(x))) # convert x to a square matrix
            x
        } else {
            print("Unable to converted input to a square matrix")
        } 
    } 
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m

    list(set        = set, 
         get        = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## cacheSolve()
##
cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached inverse matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
