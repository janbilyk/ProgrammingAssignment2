#load invertible matrix B to run tests on
B <- matrix( 
 c(2, 4, 3, 1, 5, 7, 9, 9, 12), 
  nrow=3, 
   ncol=3) 

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL #creating empty matrix
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
  

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
 
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    message("nothing found in cache")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
