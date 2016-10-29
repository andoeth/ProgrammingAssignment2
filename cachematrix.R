## These two functions do the following: 
## 1. Make a matrix that caches the inverse of itself.
## 2. Solves for the inverse but looks for a cached answer.

## Makes list to set & get value of matrix and inverse
## Start with null cache, then set matrix input. Next get matrix, set inverse, and get inverse.
## Returns a list with the outputs of the set, get, setInverse, and getInverse functions for later use.

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
## If the getInverse function returns a non NULL value, returns that value because it is cached and will save time.
## Else: gets the matrix from makeCacheMatrix function, solves for the inverse, and sets it to the cache for later use.
## Returns the value of the inverse matrix, whether from the cache or via solving for the inverse.

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
          
## Testing out the function to see if the code works as intended
cacheSolve(makeCacheMatrix(x <- rbind(c(5, 1.5), c(2, 4))))
## The result is below
##                     [,1]        [,2]
##                     [1,]  0.2352941 -0.08823529
##                     [2,] -0.1176471  0.29411765
## Solving regularly shows that the answer is the same, so the function is correct.
## x <- rbind(c(5, 1.5), c(2, 4))
## solve(x)
##            [,1]        [,2]
## [1,]  0.2352941 -0.08823529
## [2,] -0.1176471  0.29411765
