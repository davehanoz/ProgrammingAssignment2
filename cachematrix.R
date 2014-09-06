## File name: cachematrix.R

## Martix inversion is usually computation expensively. A possible improvement is to chache a matrix inversion instead of 
## compute it repeatly. This program is to provide a pair of functions which cache the inverse of a matrix. 

## The two functions are:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##               inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##               the inverse from the cache


## makeCacheMatrix() function creates a special list which contains the functions to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the matrix inversion
## 4.get the value of the matrix inversion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve function calculates the inversion of the matrix which was created with the above function. 
## Firstly, it checks to see if the inversion is existed with getsolve function. 
## If so, it gets the inversion from the cache and skips the computation. 
## Otherwise, it calculates the inversion of the matrix and sets the value of the inversion in the cache 
## via the setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  x$setsolve(m)
  
  m
}
