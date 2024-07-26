## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){     
    x <<- y 
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

##The cacheSolve function calculates the inverse of the special "matrix" from the makeCacheMatrix function.
##This function checks to see if the inverse has been calculated and that the matrix has not changed
##If this is true, then the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached date")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

