## Short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    ## initialize inv as NULL
  set <- function(y) {           ## defines function set 
    x <<- y                      ## allocate x as the value of y
    inv <<- NULL                 ## allocate inv as NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve  ## apply the function solve to get the inverse
  getinverse <- function() inv                 ## set "inv" as the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Short comment describing this function

cacheSolve <- function(x, ...){  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
