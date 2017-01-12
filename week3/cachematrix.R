# Chuan Lee
# R Programming course by Johns Hopkins University
# Coursera https://www.coursera.org/learn/r-programming/
# January 2017 Session
# Programming assignment 2 : Lexical scoping
# Week 3
# https://github.com/rdpeng/ProgrammingAssignment2

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there are
# also alternatives to matrix inversion that we will not discuss here). Your assignment
# is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## TEST

# > source('cachematrix.R')
# > mat<-makeCacheMatrix(matrix)
# > mat$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# > mat$getInverse()
# NULL  ## the inverse has not been calculated yet
#
# > cacheSolve(mat)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > mat$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# Second run, the inverse has already been calculated once and cached
#
# > cacheSolve(mat)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
 
