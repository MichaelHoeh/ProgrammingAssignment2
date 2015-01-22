# Summary: a special matrix is created - actually list containing function to
# 1. set value of matrix
# 2. get value of matrix
# 3. set value of inverse of matrix
# 4. get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  
  setMat <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  getMat <- function() x
  setInvMat <- function(xinvatrix) xinv <<- xinvatrix
  getInvMat <- function() xinv
  list(setMat = setMat, getMat = getMat,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}

## cacheSolve: Return matrix that is the inverse of 'x'.
## When inverse of 'x' has already been computed and x did not change then it is returned from cache
cacheSolve <- function(x, ...) {
  xinv <- x$getInvMat()
  # if inverse is found in cache then use it
  if(!is.null(xinv)) {
    message("getting cached matrix")
    #return inverse matrix from cache
    return(xinv)
  }
  #else compute inverse and set it in cache
  else {
    mtrix <- x$getMat()
    xinv <- solve(mtrix, ...)
    x$setInvMat(xinv)
    #return the inverse matrix
    return(xinv)
  }
}