## Functions for cache the matrix inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  newMatrixSet <- TRUE
  
  
  set <- function(y) ## Stores the matrix to cache
  {
    x <<- y
    my_inverse <<- NULL
    newMatrixSet <<- TRUE
  }
  get <- function() x ## Returns the actual matrix inside
  setinverse <- function(matInverse){
    my_inverse <<- matInverse ## Stores the inverse in cache
    newMatrixSet <<- FALSE
  }
  isUpdated <- function() newMatrixSet
  getinverse <- function() my_inverse ## Reads the inverse from cache
  #Special matrix is actually a list with relevant functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse, isUpdated = isUpdated)
}


## Finds if the passed special matrix matches the pne in cache

cacheSolve <- function(mat, ...) {
  
  
  if(!mat$isUpdated()){
    ## Passed data is same as in cache
    
    matInverse <- mat$getinverse()
    if(!is.null(matInverse)) {
      message("getting cached inverse")
      return(matInverse)
    }
  }
  ## Passed data is different from cache
  message("Matrix is changed")
  data <- mat$get()
  makeCacheMatrix(data)
  mat$set(data)
  matInverse <- solve(data)
  mat$setinverse(matInverse)
  matInverse    ## Return a matrix that is the inverse of 'x'
}
