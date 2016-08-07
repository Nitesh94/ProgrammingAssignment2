## Program to evaluate inverse of a matrix and 
## cache the value for faster computation:

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function cacheSolve evaluates the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverse()
    if (!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)             ## Returns the inverse if present in cache
    }
    mat <- x$get()
    invMatrix <- solve(mat, ...)
    x$setInverse(invMatrix)
    invMatrix                         ## Calculates inverse if not present in cache
}