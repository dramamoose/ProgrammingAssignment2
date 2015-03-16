# Create a list of functions which allows a Matrix and the inverse of that matrix
# to be cached in the operating (non-local) environment

##makeCacheMatrix: Creates a number of functions for a matrix which can be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Set M to null
  set <- function(y) { #Set the value of our matrix and reset m in the parent environment
    x <<- y 
    m <<- NULL
  }
  get <- function() x #Returns the value of our matrix 
  setsolve <- function(solve) m <<- solve #Sets the inverse of our matrix
  getsolve <- function() m #Returns the inverse of our matrix
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) #Create a list of all four of these functions
}  



##cacheSolve: Returns the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve() 
  if(!is.null(m)) { #If we already have cached data for M
    message("getting cached data") #Let us know we aren't recalculating
    return(m) #And provide us with the inverted matrix
  }
  data <- x$get()  #Otherwise, pull the matrix
  m <- solve(data, ...) #Then reverse it
  x$setsolve(m) #And cache it in x
  m #Then display the inverted matrix
}
