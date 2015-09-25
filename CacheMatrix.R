## These functions find the inverse of the function and stores it in a special matrix.

## The makecache function creates a matrix(x) that is the inverse of the variables. 
##It then sets the variables within the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)inv<<- inverse
  getInverse <- function()inv
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse
  )
}


## This function gets the inverse and places it in the matrix created above.
## If there is already an inverse from the matrix above it will return that value

cacheinv <- function(x, ...) {
  z <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}

        ## Return a matrix that is the inverse of 'x'

