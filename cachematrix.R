#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #myMatrix <- makeCacheMatrix(a)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #setmatrix <- function(solve) m <<- solve(x)
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
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

#Demonstrates how above code works
x <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
z <- makeCacheMatrix(x)
cacheSolve(z)
cacheSolve(z)
