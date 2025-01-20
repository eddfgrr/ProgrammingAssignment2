## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # variable for inverse matrix is set NULL 
  set <- function(y){
    x <<- y
    m <<- NULL
  } # matrix x is updated and m is initialized 
  get <- function() x # return currently saved matrix 'x'
  setinverse <- function(inverse) m <<- inverse # save calculated inverse matrix 
  getinverse <- function() m # return m in cache 
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
# this function calculates inverse matrix or returns the cached data 


cacheSolve <- function(x, ...) {
  m <- x$getinverse() # cached inverse matrix is loaded 
  # if the inverse matrix has been cached, it is returned
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # if there is no cached matrix, inverse matrix is calculated
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}      ### Return a matrix that is the inverse of 'x'

