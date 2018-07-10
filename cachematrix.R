## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL       ##initialize inv as NULL 
  set <- function(y){   ##define set func to assign new val of matrix in parent env
    x <<- y 
    inv <<- NULL   ##if there is new matrix reset to null
  }
  getfunction <- function() x  ##returns matrix arguments
  
  setinverse <- function(inverse) inv <<- inverse  ##assign value of inv in parent env
  getinverse <- function() inv          ##gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##refer to the func using $ operation

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
