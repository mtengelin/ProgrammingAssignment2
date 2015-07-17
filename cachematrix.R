## These functions cache the inverse of a matrix. They create and use a special
## matrix object that's represented as a list

## makeCacheMatrix() creates the special "matrix" object from the vector argument
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Create special matrix object from matrix 'x'
    m <- NULL
    
    ## Define the set function for the special matrix object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Define the other 3 functions for the special matrix object
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempt to get the inverse from cache of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) { ## inverse already calculated, return cached value
        message("getting cached data")
        return(m)
    }
    
    ## inverse not yet calculated, calculate it and store it in the cache of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
