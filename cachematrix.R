## Put comments here that give an overall description of what your
## functions do
## These two functions take in a matrix, x, and solve for the inverse of x.
## 

## MakeCacheMatrix takes in a matrix, sets an obect to NULL where the matrix
## can be placed in the parent frame, and is a list of functions that can be 
## used to act on that matrix.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(Inverse) m <<- Inverse
  
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve takes the matrix argument x and gives the inverse.  
## If the matrix is already in the cache, it will not redo the calculation
## If the matrix is null, this function gets the matrix from the above function
## solves it, and sets that as the inverse in the parent frame, 
##where it is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
  
}
