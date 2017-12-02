## Caching the INVERSE of a MATRIX:
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrse <- NULL
        set <- function(y) {
                x <<- y
                invrse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrse <<- inverse
        getInverse <- function() invrse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the mean of the special "vector" created with 
## the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets 
## the value of the mean in the cache via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrse <- x$getInverse()
        if (!is.null(invrse)) {
                message("getting the cached data...")
                return(invrse)
        }
        mat <- x$get()
        invrse <- solve(mat, ...)
        x$setInverse(invrse)
        invrse
}
