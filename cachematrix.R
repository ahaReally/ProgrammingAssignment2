## Calculating the Inverse of a Matrix using Caching

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatively


##  `makeCacheMatrix`: This function creates a special "matrix" object
##    that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        INV <- NULL
        set <- function(y) {
                x <<- y
                INV <<- NULL
        }
        get <- function() x
        setINV <- function(invCal) INV <<- invCal
        getINV <- function() INV
        list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        INV <- x$getINV()
        if(!is.null(INV)) {
                message("getting cached inverse of a matrix")
                return(INV)
        }
        data <- x$get()
        INV <- solve(data, ...)
        x$setINV(INV)
        INV
}
