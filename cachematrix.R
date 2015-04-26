## cachematrix.R contains 2 main functions:  makeCacheMatrix and cacheSolve
## the main function makeCacheMatrix stores 4 subfunctions (set, get, setinv, getinv)
## set = stores the matrix stored in the main function
## get = returns the matrix stored by set in the main function
## setinv = stores the value of the inverse in the function 
## getinv = returns the value of the inverse computed in the function
##cacheSolve computes the inverse using the input of the matrix stored in makeCacheMatrix
 
## The makeCacheMatrix function creates and stores a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
         x <<- y
         inv <<- NULL
 }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
 
 ## the list command is needed to ensure when the matrix object is assigned, the object has 
 ## all 4 functions available
 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  data2 <- det(data)
  
  ## Not all square matrices entered are invertible this next code will give
  ## an error message if the matrix stored in makeCacheMatrix is not invertible
  
  
  if(data2==0) {
    message("Matrix Not Invertible - Determinant = 0 - Try again")
    stop 
  }
  
  ## If matrix is invertible this code will calculate the inverse of the matrix
  ## and store it in the makeCacheMatrix object above
  
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
} 
