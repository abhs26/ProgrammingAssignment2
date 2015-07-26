
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


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



