## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a CacheMatrix from a matrix
## Assumption: Matrix is assumed to be invertible.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Setter function for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Getter function for the matrix
  get <- function() x 
  ## Setter for the inverse.
  setinverse <- function(inverse) inv <<- inverse
  ## Getter for the inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## Write a short comment describing this function
## Solve for inverse of a matrix using a CacheMatrix as input.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if(!is.null(inv)) {
     ## Inverse saved, returning it.
     message("Getting cached inverse matrix")
     return(inv)
   }
   # Inverse not saved, calculate and return.
   matrix <- x$get()
   inv <- solve(matrix,...)
   x$setinverse(inv)
   inv
}
