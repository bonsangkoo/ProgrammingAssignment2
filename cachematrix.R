## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function definese the set, get functions that enables other functions to set and get the loaded matrix
## The funcion also defines the setinverse, getinverse functions that allows other functions to 
## set and get the inverse values in the cache
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #cached value
  }
  get <- function() {x}
  setinverse <- function(inverse) {m <<- inverse}
  getinverse <- function() { m }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function
# this function gets the inverse of the matrix x if it is saved in cache, otherwise 
# it solves for the inverse of the matrix x and sets it to m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
