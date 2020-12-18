## Put comments here that give an overall description of what your
## functions do

## the main ideas of the two functions below is to save the overall
## computing efforts by caching the computed data using "<<-"
## to assign a value to an object in an environment

## in this function, we get/set the inverse using "<<-"
## to assign those values into an obj in the environment
## (cache the data)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## in this function, we first check if we need to do the calculation
## if so, calculate the inverse then cache it
## if the data is already cached in the current environment
## we do not have to compute again, hence, the efficiency is enhanced


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
