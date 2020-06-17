
  

## This function sets the parameters to create an inversible matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##sets the parameter of the null vector
  set <- function(y) { ##sets the "set" function 
    x <<- y
    m <<- NULL
  }
  get <- function() x ## sets the "get" function
  setinv <- function(inv) m <<- inv ##set the parameter of the inversible matrix
  getinv <- function() m ##plots the parameter of the inversible matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse cached function above. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { ##calculates if the inversible matrix exists. 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ##calculates the inversible matrix
  x$setinv(m)
  m
}
