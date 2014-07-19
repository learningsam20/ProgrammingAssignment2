## The following two functions try to demonstrate the usage of Caching in R
## These work with get, set strategy of storing the first calculation in memory and then
## returning the data from memory if it exists and do calculations only if it does not
## It makes use of solve function and takes matrix to be inverted as input

## The function is responsible for storing the matrix inverse in global environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## This is global assignment for storing in cache
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse by first checking if it already exists in the cache using above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## This is to check if it was calculated and return if returning from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else{
        message("getting freshly calculated data")
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
