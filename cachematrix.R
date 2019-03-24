## Getting the inverse of a matrix works similarly with the example of 'Caching the Mean of a Vector",
## the only difference is the type of data to run the program.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Below function prints out the answer derived from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}

##Sample
a=matrix(c(1,3,0,1),2,2)
cacheSolve(makeCacheMatrix(a))
  [,1] [,2]
[1,]    1    0
[2,]   -3    1
