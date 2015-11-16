## cachematrix provides functions to cache the inverse matrix of the input matrix
## And look it up in the cache rather than recompute it.

## This function returns a list that caches a matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'
## It first tries to retrieve the inverse matrix from cache. 
## If that does not exist. It calculates the inverse matrix of the input matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
