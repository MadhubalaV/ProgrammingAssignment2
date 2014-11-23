## makeCacheMatrix and cacheSolve work together to avoid repeated computations for inverse matrix with same input.

## makeCacheMatrix uses <<- to save the inverse matrix; provides get and set methods for the cacheSolve function.

makeCacheMatrix <- function(x=matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  } 
  setInverse <- function(inverse) 
  {
    i <<- inverse 
  }

  getInverse <- function() 
  { 
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve first checks if the inverse of the matrix is available in enviroment - if so, it returns the existing value. else, it calculates the inverse and saves it for future retrieval.

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()

  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
