

# The makeMatrixCache function creates a list of four functions with a matrix as the argument 
# The cacheSolve function uses the list of functions to check whether the inverse has been cached
# and if it hasn't it computes it. It takes makeMatrixCache as an argument.

makeMatrixCache <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(solve) {
    m <<- solve
  }
  getinverse <- function() {
    m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
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

a <- makeMatrixCache(matrix(1:4,2,2))


cacheSolve(a)






