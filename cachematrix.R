## makeCacheMatrix creates a list of functions 
## which help to cut the memory use of calculating the inverses of large matrices.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns a matrix which is the inverse of the input, but only calculates if there's no inverse stored in memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
