
#This function caches the parts of an inversion matrix procedure..

makeCacheMatrix <- function(x = matrix()) {
  # 1. Set the value of the matrix:
  invnul = NULL
  setfunc = function(y) {    
    x <<- y
    invnul <<- NULL
  
  }
  # 2. get the value of the matrix:
  getval = function() x
  # 3. set the value of inverse of the matrix:
  setinverse = function(inverse) invnul <<- inverse
  # 4. get the value of inverse of the matrix:
  getinverse = function() invnul
  list( setfunc= setfunc, getval=getval, setinverse=setinverse, getinverse=getinverse)
}

# This function gets the inverse matrix based on the cache-based function. 

cacheSolve <- function(x, ...) {
  #1. Return the inverse matrix of x
  invnul = x$getinverse()
  
  #2.  Get the matrix if calculated.
  if (!is.null(invnul)){
    message("getting cached data")
        return(invnul)  }
  
  #3. If not, then calculates the inverse.
  matinverse = x$getval()
  invnul = solve(matinverse, ...)
  
  #4. Finally, sets the inverse 
  x$setinverse(invnul)
  
  return(invnul)
}

