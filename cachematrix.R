## Creates a matrix and sets the value of the matrix and its inverse and
## gets the values of the matrix and its inverse. Calculates the inverse of the matrix 
## and checks if it has already been calculated and retrieves it from the cache

## Creates matrix and sets and gets values of matrix and
## sets and gets value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}