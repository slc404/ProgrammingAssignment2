## These functions work together to prevent the calculation of an inverse if it has already been done before.
## This save computation time. 


## This function does four things. 1) set the matrix, 2) get the matrix, 3) set the value of inverse, 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function returns the argument makeVector(). It retrieve the inverse for the cache value if it is already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getmatrix()
  if(!is.null(m)) {
    #checks to see whether the result is NUL
    message("getting cached data")
    return(m)
  }
  #If the result of !is.null(m) is FALSE, cachemean() gets the vector from the input object, 
  #calculates a mean(), uses the setmean() function on the input object to set the mean in the input object, 
  #and then returns the value of the mean to the parent environment by printing the mean object.
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
