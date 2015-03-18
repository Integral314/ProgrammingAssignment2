## These function invert a matrix and cache the inverse then
## check the cache for the matrix if called to invert later

## makeCacheMatrix will invert a matrix and cache the solution

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the vector
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the vector
        get <- function()  x
        ## set the value of the mean
        setinv <- function(solve) inv <<- solve
        ## get the value of the mean
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks the matrix for a cached solution and reports it if found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
