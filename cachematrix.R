## Put comments here that give an overall description of what your
## functions do
## I am going to create two main functions which are makeCacheMatrix and cacheSolve. They will cache 
## the inverse of the special matrix and use the cache data rather than the time-consuming computation. 


## Write a short comment describing this function
## makeCacheMatrix function will create a special matrix object which will cache the inverse
makeCacheMatrix <- function(x = matrix()) {
invmat <- NULL
setmat <- function(y) {
  x <<- y
  invmat <<- NULL 
}
getmat <- function() x
setinv <- function(inverse) invmat <<- inverse
getinv <- function() invmat
list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve function will compute the inverse of the special matrix using using makeCasheMatrix.If the value 
## is already calculated, it will use its data from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$getmat()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}

## test 
testmat <- matrix(1:4, 2,2)
cachemat <- makeCacheMatrix(testmat)


cacheSolve(cachemat)

## > cacheSolve(cachemat)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
