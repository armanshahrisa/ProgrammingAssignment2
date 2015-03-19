# makeCacheMatrix: This function creates a special
# "matrix" object that can cache its inverse.

# cacheSolve: This function computes the inverse of
# the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.


# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of a square matrix
# get the value of inverse of a square matrix
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of a square
# matrix created with the above function. However, it first
# checks to see if the inverse matrix has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the matrix
# and sets the value of the inverse in the cache via the setinverse
# function.
cacheSolve <- function(x = matrix(x), ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}

# Explanations were partly from following link and some by modifying sentences.
# https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/submissions
