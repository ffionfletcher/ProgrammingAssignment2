## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. The below two
## functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    mx <- NULL
    set <- function (y) {
        x <<- y
        mx <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) mx <<- inverse
    getinverse <- function() mx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    mx <- x$getinverse()
    if(!is.null(mx)){
        message("getting cached data")
        return(mx)
    }
    
    dat <- x$get()
    mx <- solve(data, ...)
    x$setinverse(mx)
    mx
    
}
