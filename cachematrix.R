## cachematrix.R

# Function to cache potentially time-consuming matrix inverse computations. 
# Rationale:
#   Taking the inverse of a matrix vector can be time consuming. 
#   If the contents of a vector are not changing, it may make sense to cache the return value.

##############################################################################
## Function makeCacheMatrix
##
## Function used in conjunction with cacheSolve to return matrix inverses.  
## Return: List of functions ( set, get, setmatrix, getmatrix )

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve)  m <<- solve
  getmatrix <- function()       m
  list( set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix
  )
}
##############################################################################
## Function cacheSolve
##
## Return: A matrix that is the inverse of the matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if (is.null(m)) {
    message("No cached data found")
  } else
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

##############################################################################
## Coursera example: Caching the Mean of a Vector
##############################################################################
## Function makeVector
##
##
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(
    set = set, get = get,
    setmean = setmean,
    getmean = getmean
  )
}

##############################################################################
## Function cachemean
##
##
cachemean <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

####################################################################################
## Original Coursera Template for Prog Ass2
####################################################################################

# ## Put comments here that give an overall description of what your
# ## functions do
# 
# ## Write a short comment describing this function
# 
# makeCacheMatrix <- function(x = matrix()) {
#   
# }
# 
# 
# ## Write a short comment describing this function
# 
# cacheSolve <- function(x, ...) {
#   ## Return a matrix that is the inverse of 'x'
# }
# 
