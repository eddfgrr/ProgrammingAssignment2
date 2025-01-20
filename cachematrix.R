## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function caches the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
# computes the inverse of the matrix returned by makeCacheMatrix
# if the inverse has already been calculated, it retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}      ### Return a matrix that is the inverse of 'x'

# making matrix
x <- rbind(c(2,0), c(0,2))

makeCacheMatrix(x)

# return the inverse of 'x'
cacheSolve(makeCacheMatrix(x))
