## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(solve) m <<- solve
        getCache <- function() m
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting inversed data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCache(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
