## function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse 
## functions "cacheSolve" computes the inverse of the special "matri"

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invM) inv <<- invM
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, y=x$get(), ...) {  
  #x is the special matrix created by makeCacheMatrix.
  #y is the original matrix  that was used to make x; allow user to check if matrix changed; default as x$get()
  idcheck <- identical(x$get(), y)  #Compare to see if the matrix cached has changed compared to original matrix
  inv <- x$getinv()
  
  if(idcheck & !is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
