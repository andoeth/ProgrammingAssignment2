## These two functions do the following: 
## 1. Make a matrix that caches the inverse of itself.
## 2. Solves for the inverse but looks for a cached answer.

## Makes list to set & get value of matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Caches inverse of matrix, but looks to see if answer is cached.

cacheSolve <- function(x, ...) {
     m <- x$getInverse()
     if(!is.null(m)) {
          message("Getting cached matrix inverse!")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
