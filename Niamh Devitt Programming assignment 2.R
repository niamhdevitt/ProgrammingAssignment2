## Put comments here that give an overall description of what your
## functions do

## create a special matrix which is a list containing functions to
## set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Take cached matrix and checks if inverse has already been computed and returns cached inverse if so,
## otherwise calculates inverse of matrix 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
