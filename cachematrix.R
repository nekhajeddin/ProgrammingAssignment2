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
  set <- function(y) { # store the matrix to cache
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x # retrieve the matrix from cache
  setinverse <- function(inverse) mat_inverse <<- inverse # store the inverse matrix to cache
  getinverse <- function() mat_inverse # retrieve the inverse matrix from cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of the matrix is calculated yet. if not,
## it calculate the matrix inverse and send it to the setinverse function 
## in order to save the inverse matrix in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getinverse()
  if(!is.null(mat_inverse)) { # check if inverse is calculated yet
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data) # calculate the inverse
  x$setinverse(mat_inverse) # store the inverse matrix in cache
  mat_inverse # return the inverse matrix
}
