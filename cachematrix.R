## Two functions are used here: makeCacheMatrix function and cacheSolve function
## This pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<-NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function() {inv}
  list(set =set, get = get, setInverse = setInverse,getInverse=getInverse)
  
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  dat <-x$get()
  n <- solve(dat, ...)
  x$setInverse(n)
  n                           ## Returns a matrix that is the inverse of 'x'
}
