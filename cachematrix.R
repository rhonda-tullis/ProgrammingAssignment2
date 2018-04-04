## A pair of functions below work together, making a matrix and its inverse then 
## caching both. If there is a new matrix, a new inverse is calculated and the 
## matrix and inverse are cached.  Otherwise, the cached one is returned.


## Sets functions to cache the matrix and its inverse that will populated by
## cacheSolve.  Also makes functions retrievable using R object/$ method.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks whether inverted matrix is in cache and whether the matrix changed 
## from before.  If so, it makes new inverted matrix and calls function to 
## cache matrix and its inverse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
          if(!is.null(m)) {
          message("getting cached data")
          return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}

