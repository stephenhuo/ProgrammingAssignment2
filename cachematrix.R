## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize m with Null value
        m <- NULL 
        ## set value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get value
        get <- function() x
        ## set value as its inverse
        setsolve <- function(solve) m <<- solve
        ## returns its inverse
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve<- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
