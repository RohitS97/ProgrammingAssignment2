makeCacheMatrix <- function(x = matrix()) {
  
  ## This function creates a special matrix object which can cache its inverse
  
  m <- NULL
  set <- function(y) {
    x <<- y          ##Settng the value of matrix
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
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  
  m <- x$getinverse()  ## If the inverse has already been calculated the inverse is retrieved from the cache
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()  ##Otherwise, it calculates the inverse of matrix
  m <- solve(data, ...) 
  x$setinverse(m)
  m
}
