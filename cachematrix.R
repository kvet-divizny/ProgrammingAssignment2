## makeCacheMatrix creates a list of four functions for setting and getting the original imput matrix (x)
## and its inverse (inv)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverted) inv <<- inverted
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes the output of makeCacheMatrix and checks, if value is already computed. If so, it returns
## the value of inv variable from makeCacheMatrix. If the value of inv was not already set, it returns the inverse
## of ofiginal matrix (stored by makeCacheMatrix as x) and stores it for later use.

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
