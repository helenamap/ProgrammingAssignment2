##The two functions below -- makeCacheMatrix and cacheSolve --
##are used to create a matrix, store it, and cache its inverse
##for later retrieval.

#The makeCacheMatrix function stores a list of 4 functions:
#set(), get(), setinverse(), and getinverse(). Running a
#square matrix in this function stores it and its inverse.

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

#The function cacheSolve solves for the inverse
#of the matrix -- if it isn't already cached. If
#the inverse matrix is cached, using the cacheSolve
#function will produce a message "getting cached data"
#before returning the cached inverse matrix.

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
