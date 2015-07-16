## This functions provide object and API for caching inverse matrix calculation results

## This function returns vector of 4 API functions 
## for maintaining matrix and corresponding inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function checks if inverse matrix is available in cache
## If no, calculates an inverse matrix and puts in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
