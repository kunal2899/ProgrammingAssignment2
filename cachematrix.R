## Put comments here that give an overall description of what your
## functions do

##it creates a matrix and define various functions which will be used in 'cacheSolve' function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               ##firstly we declare variable named 'inv' and initially set it to be NULL
  set <- function(y) {      ##then create a function that set the value of 'x' according to value passed through argument 'y'
    x <<- y
    inv <<- NULL
  }
  get <- function() x       ##a function to get the value of 'x' whenever it's called
  
  setinverse <- function(inverse) inv <<- inverse    ##it sets the value of inverse
  getinverse <- function() inv                       ##it gets the value of inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##this matrix takes matrix and calculate it's inverse if that calculation is happened already then it returns that value rather that calculating it again
cacheSolve <- function(x, ...) {     ##Return matrix that is inverse of 'x'
  c <- x$getinverse()   ##first we get value of inverse in 'c' if available
  
  if(!is.null(c)){     ##it checks the availabilty, if available then it direct return the value rather than calculating it again
    message("getting cached data")
    return(c)
  }
  
  data <- x$get()     ##then get the value of matrix 
  c <- solve(data,...) ##and process it for inverse
  x$setinverse(c) ##finally set the value of inverse present in 'c'
  c                 ##last line indicates value which is returned
}
