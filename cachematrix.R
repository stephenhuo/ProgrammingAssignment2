## 2 functions are included in here, makeCacheMatrix and cacheSolve
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
## How to run: 
## source("cachematrix.R")
## t<-matrix(c(11, 21, 12, 22), 2, 2)
## datalist <- makeCacheMatrix(t)
## cacheSolve(datalist)



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize m with Null value
        m <- NULL 
        ## set value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## returns the value of the original matrix
        get <- function() x
        
        
        ## this is called by cacheSolve() during the first cacheSolve() access     
        setsolve <- function(solve) m <<- solve
        
      
        ## this will return the cached value to cacheSolve() on subsequent accesses
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve<- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        m <- x$getsolve()
        if(!is.null(m)) {       # if solved matrix was already cached (not NULL) ...
                message("getting cached data") # ... send this message to the console
                return(m)       # return the matrix ... "return" ends
        }
        data <- x$get() # pass the matrix to data
        m <- solve(data, ...) # if m was NULL then we have to inverse the data
        x$setsolve(m) # store the inverse matrix in x
        m # return m
}
