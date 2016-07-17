## These two functions, when used in conjunction with each other, will
## solve for the inverse from a matrix and then cache it.

#This function creates a special variable in 'matrix' form so that the
#inverse can be calculated and cached.

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

#This function solves for the inverse of the matrix created by
#makeCacheMatrix and caches it.  So long as the matrix has not
#changed, the function will retrieve the inverse of a matrix that
#has already been solved.

cacheSolve <- function(x, ...) {
     s <- x$getsolve()
     if (!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     s
}