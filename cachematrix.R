## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatinv <- function(inverse) inv <<- inverse
  getmatinv <- function() inv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getmatinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatinv(inv)
  return(inv)
}
