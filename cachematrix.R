## Put comments here that give an overall description of what your
## functions do

## the main ideas of the two functions below is to save the overall
## computing efforts by caching the computed data using "<<-"
## to assign a value to an object in an environment

## Write a short comment describing this function
## in this function, we get/set the inverse using "<<-"
## to assign those values into an obj in the environment
## (cach the data)

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


## Write a short comment describing this function
## in this function, we calculate the inverse once then cache it
## so we do not have to calculate it again when we need that value 
## in the current environment, hence, the efficiency is enhanced

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
