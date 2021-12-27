## Programs of this assignment are supposed to use cache for storing and 
## retrieving important and frequently used data 

## The makeCacheMatrix function, gets an invertible matrix and creates a list
## containing four funtions (get, set, getinverse and setinverse). The description
## of these functions are as follows:
## set: gets an invertible matrix and cache in a variable named x, and assigns 
## NULL to variable mat_inverse
## get: retrieve the matrix cached in the x variable
## setinverse: cache the inverse of a matrix in a variale named mat_inverse 
## getinverse: retrieves the value cached in mat_inverse variable

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat_inverse <<- inverse
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getinverse()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data)
  x$setinverse(mat_inverse)
  mat_inverse
}
