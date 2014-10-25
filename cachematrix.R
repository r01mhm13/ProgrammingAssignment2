

# The makeMatrixCache function creates a list of four functions with a matrix as the argument 
# The cacheSolve function uses the list of functions to check whether the inverse has been cached
# and if it hasn't it computes it. It takes the output of the makeMatrixCache function as an argument.

makeMatrixCache <- function(x = matrix()) { 
  m <- NULL # Assigns NULL to m as default
  set <- function(y) { # set function uses <<- operator to assign y to x and NULL to m in all environments (because x and m will have values from previous runs)    x <<- y
    m <<- NULL
  }
  get <- function() { # creates function that gets matrix
    x
  }
  setinverse <- function(solve) { # creates function that assigns function solve to m in parent environment
    m <<- solve
  }
  getinverse <- function() { # creates function that gets the inverse of matrix set by the setinverse function
    m
  }
  
  list(set = set, get = get,  #Creates a list of the four functions for use in cacheSolve function
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  m <- x$getinverse() # Within the cacheSolve environment variable m is assigned to output of the getinverse function from makeMatrixCache
  if(!is.null(m)) { # If m is not NULL then the inverse matrix stored in m is returned
    message("getting cached data")
    return(m)
  }

  data <- x$get() # If the inverse is not cached in m (m is NULL) then the inverse matrix is:
  m <- solve(data, ...) #solved
  x$setinverse(m)       #set
  m                     #returned
}

a <- makeMatrixCache(matrix(1:4,2,2))


cacheSolve(a)






