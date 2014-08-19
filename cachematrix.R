## Matrix inversion is usually a costly computation. It might be better to cache
## inverse of matrix instead of computing it repeatedly. Below functions will do
## the job.

## Cache matrix and its inversion.
## param x: original matrix.

makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setSolved <- function(inverse) solved <<- inverse
  getSolved <- function() solved
  list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## Call makeCacheMatrix to get the inverse of a matrix
## param x: original matrix.

cacheSolve <- function(x, ...) {
  solved = x$getSolved()
  if(!is.null(solved)) {
    message("Get cached inverse.")
    return(solved)
  }
  data = x$get()
  solved = solve(data, ...)
  x$setSolved(solved)
  ## Return a matrix that is the inverse of 'x'
  solved
}
