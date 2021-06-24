
  ##makeCacheMatrix takes in a matrix returns a
  ## cached object with functions to get and set 
  ## values from the initial matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y     #sets new matrix from the parent environment
    m <<- NULL    #inverse matrix is null when a new matrix is set
  }
  get <- function() x     #return matrix values
  setInv <- function(inv) m <<- inv     #sets inverse matrix computed from matrix in parent environment
  getInv <- function() m    #returns inverse matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)     #allows function calls with $
}



  ##takes a cached matrix object of type makeCacheMatrix
  ## and computes its inverse. before printing
  ## the inverse matrix, it sets the computed
  ## matrix value in the makeCacheMatrix object.
  ## if the inverse has already been computed, 
  ## returns the cached inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getInv()     #if inverse matrix has been cached, returns it to m, otherwise m is empty
  if(!is.null(m))
  {
    message("getting cached inverse")
    return(m)     #returns cached inverse matrix
  }
  data <- x$get()     #holds cached matrix
  m <- solve(data, ...)     #computes the inverse matrix of the cached matrix
  x$setInv(m)     #sets the computed inverse matrix
  m
        ## Return a matrix that is the inverse of 'x'
}
