## The makeCacheMatrix function creates an R object that stores a matrix and its inverse.
## The R object contains four functions: set(), get(), setinverse() and getinverse() as well as two data objects x and m.

## Firstly the two data objects x and m are initialised. Then the behaviours for the four functions of are defined. Lastly
## each of the functions are assigned as elements of a list and returned to the parent environment.

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes an argument that is returned by makeCacheMatrix in order to retrieve the inverse from the cached value 
## that is stored in makeCacheMatrix object's environment.
##cacheSolve starts with an argument x, and an ellpsis. Next the function attempts to retrieve an inverse from the object passed as an
## argument. cacheSolve uses the setinverse() function on the input object to set the inverse in the iput object and then returns the
## inverse of the matrix to the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
