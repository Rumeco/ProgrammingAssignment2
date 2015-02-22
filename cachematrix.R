##The following pair of functions are used to cache the inverse of 
## a matrix

## makeCacheMatrix creates a special "vector", or list containing 
## a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The following function calculates the inverse of the 
## special "vector" created with the above function. 
## However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the 
## cache via the setinverse function.
## The matrix here must be square otherwise the solve function 
## will not work

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("Retreiving cached data.")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}
