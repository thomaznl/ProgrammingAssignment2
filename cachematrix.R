## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        d <- NULL
        set <- function(y) {
                x <<- y
                d <<- NULL
        }
        get <- function() x
        setCache <- function(solve) d <<- solve
        getCache <- function() d
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        d <- x$getInverse()
        if(!is.null(d)) {
                message("getting inversed data")
                return(d)
        }
        data <- x$get()
        d <- solve(data, ...)
        x$setCache(d)
        d
}
        ## Return a matrix that is the inverse of 'x'
