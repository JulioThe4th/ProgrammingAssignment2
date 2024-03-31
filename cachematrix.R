## These functions enable caching of matrix inverses, improving efficiency by 
## storing and retrieving previously computed results.

## This function creates a special "matrix" object that can cache its inverse.
## It contains methods to set and get the matrix, 
## as well as to set and get the cached inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  
  Set <- function(matrix){
    mat <<- matrix
    inv <<- NULL
  }
  
  Get <- function(){
    mat
  }
  
  Set_Inverse <- function(inverse) {
    inv <<- inverse
  }
  
  Get_Inverse <- function() {
    inv
  }
  
  list(Set = Set, Get = Get,
       Set_Inverse = Set_Inverse,
       Get_Inverse = Get_Inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mat <- x$Get_Inverse()
  
  if( !is.null(mat) ) {
    message("getting cached data")
    return(mat)
  }
  
  obs <- x$Get()
  mat <- solve(obs) %*% obs
  x$Set_Inverse(mat)
  
  mat
}