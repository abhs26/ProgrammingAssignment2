
# makeCacheMatrix 
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(mat) {
    x <<- mat
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
# In first step we calculate if the inverse has been already calculated. 
# If calculated gets the result and return
# else compute the inverse
# set the value in the cache using setinverse function.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  return(inver)
}



