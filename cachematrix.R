## The mackCacheMatrix returns the following list of functions: 
## 1. set the matrix
## 2. Get the matrix
## 3. set the inverse
## 4. get the inverse
##The list is used as input in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  set <- function(y) {
    x <<- y
    matrixinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrixinv <<- inverse
  getinverse <- function() matrixinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of the original matrix input from makeCacheMatrix
## It then checks to see if the inverse has been calculated, if so, it grabs the inverse from the original comp, if not, it calculates the ivnerse. It then resets the inverse in the cache. 

cacheSolve <- function(x, ...) {
  matrixinv <- x$getinverse()
  if(!is.null(matrixinv)) {
    return(matrixinv)
  }
  data <- x$get()
  matrixinv <- solve(data)
  x$setinverse(matrixinv)
  matrixinv
}
## Return a matrix that is the inverse of 'x'