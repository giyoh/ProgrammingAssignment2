## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {

  InverseMatrix <- NULL
  
  
  # Assigns the Matrix
  set <- function(Y){
    
    X <<- Y
    InverseMatrix <<- NULL
    
  }
  
  # Retrives the Matrix
  get <- function()X
  
  # Set and Get the Inverse Matrix
  setInverse <- function(Inv) InverseMatrix <<- Inv
  getInverse <- function() InverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  xInv <- x$getInverse()
  
  if (!is.null(xInv)){
    message("Getting Cached Inverse")  # Inverse exists in cache
    return(xInv)
  }
  
  # compute Inverse Matrix
  
  matx <- x$get()
  xInv <- solve(matx, ...)
  x$setInverse(xInv)
  xInv
}
