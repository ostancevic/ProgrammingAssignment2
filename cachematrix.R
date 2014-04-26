## Put comments here that give an overall description of what your
## functions do

## Creates a Cacheable Matrix object
## The object can store the matrix and its inverse
## for quick computation by caching

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  x <<- x
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check if inverse already exists and return if so
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  ## otherwise compute the inverse and store it
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}

