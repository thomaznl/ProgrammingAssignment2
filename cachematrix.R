## Write a short comment describing this function
##  #The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##      - set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        d <- NULL
        set <- function(y) { #check nll values
        #check if it is inverse
                x <<- y
                d <<- NULL
        }
        get <- function() x
        setCache <- function(solve) d <<- solve #get inverse
        getCache <- function() d
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

## Write a short comment describing this function
##      The following function calculates the inverse of the special "matrix" created with the above function. 
##      However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
##      the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
##      of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
#return inverse of x
        d <- x$getInverse()
        if(!is.null(d)) { #check null values
                message("getting inversed data")
                return(d) #get inversed
        }
        data <- x$get()
        d <- solve(data, ...)
        x$setCache(d)
        d
}
        ## Return a matrix that is the inverse of 'x'
