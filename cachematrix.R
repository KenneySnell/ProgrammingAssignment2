## Put comments here that give an overall description of what your
## functions do

## Assignment # 2


#This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

## 1.set the data of the matrix
## 2.get the data of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}







## The second function (CacheSolve)  computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
## otherwise it create the inverse of the matrix and caches it.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
