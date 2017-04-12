## Put comments here that give an overall description of what your
## functions do






## makeCacheMatrix creates a list object containing a matrix
## that can cache its inverse
## it is inspired by makeVector in the assignment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## cacheSolve takes any matrix (we assume the matrix
## is square and invertable).
## it calls the makeCacheMatrix function to create the
## object 

cacheSolve <- function(my_mat, ...) {
  x <- makeCacheMatrix(my_mat)
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
