## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 
## returns a list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #initialize m to be 0
  set <- function(y) { #allows you to assign new matrix 
    x <<- y
    m <<- NULL
  }
  get <- function() x #allows you to call the matrix stored
  setinverse <- function(inverse) m <<- inverse #this allows you to cache a matrix with the cache function below
  getinverse <- function() m  #allows you to call the inverse matrix stored
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #get the cache matrix out, if any, 
  #if there is one stored, then simply call the matrix from the cache data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if not, get the matrix and solve for the inverse matrix
  m <-solve(data, ...)
  x$setinverse(m) #after solving for the matrix, store it to the cache in the orginial makeCacheMatrix function
  m
}
