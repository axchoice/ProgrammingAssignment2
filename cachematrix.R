## The makeCacheMatrix and cacheSolve functions follow closely the guidelines provided in Assignment 2,
## minor modifications made. The code is tested with test cases suggested by Gregory D. Horne's hints.  

## makeCacheMatrix follows the makeVector function in setting up the get, set, getinverse and setinverse functions.
## Since we are getting the inverse of a matrix, the solve function is used in place of mean in makeVector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve follows the cachemean function closely.  
## Similarly, solve is used here instead of mean in cachemean

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
