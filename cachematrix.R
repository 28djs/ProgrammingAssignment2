## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly
## Below are two functions that are used to create a special object that stores a matrix
## and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse. 
## Input to this function is always a square invertible matrix.
## function makeCacheMatrix creates a special "vector", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


############################## Testing the functions  ###############################

## test_matrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2,byrow = T))
## cacheSolve(test_matrix)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

## cacheSolve(test_matrix)
# getting cached data
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5


## Using $set to reset the input matrix below
## test_matrix$set(matrix(c(4,7,2,6),nrow = 2, byrow = T))
## cacheSolve(test_matrix)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

## cacheSolve(test_matrix)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
