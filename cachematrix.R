

##  This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function computes the inverse of the matrix
## and retrieves the inverse from the cache, if the inverse has already been calculated

cacheSolve <- function(x, ...) {
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

## Testing the code by creating and inverting a matrix

m<- makeCacheMatrix( )
m$set( matrix( c(3, 2, 4, 1 ), 2, 2))
m$get()
cacheSolve(m)
cacheSolve(m)
