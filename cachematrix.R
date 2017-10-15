## This function allows the user to calculate the inverse of a matrix which is stored to cache.
## example code is: m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## mymatrix <- makeCacheMatrix(m1)
## cacheSolve(m1)
## Will return the following (the inverse of m1):
## [1,]    6    8
## [2,]    2    4

## the makeCacheMatrix function creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the cacheSolve function retrieves the mean from the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
