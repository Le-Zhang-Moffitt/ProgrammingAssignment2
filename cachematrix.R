## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } ## caches new elements
        get <- function() x ##gets matrix value
        setinverse <- function(solve) m <<- solve ##sets inverse value
        getinverse <- function() m ##gets inverse value
        list (set = set, get = get, 
                setinverse = setinvers, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$getinverse(m)
        m
}
