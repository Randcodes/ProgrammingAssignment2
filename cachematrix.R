## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  #
  #makeCacheMatrix function creates a special
  #"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}





## Write a short comment describing this function
  #
  #cacheSolve function computes the inverse of the special
  #"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

###### Solution ######

#The makeCacheMatrix function returns a list with four functions:
#set and get allow to set and retrieve the matrix stored in the object;
#setInverse and getInverse allow to set and retrieve the cached inverse.
#When the matrix is set or modified, the cached inverse is invalidated
#(set to NULL).

#The cacheSolve function first checks if the cached inverse is available;
#if it is, it returns it and prints a message. Otherwise, it retrieves
#the matrix, computes its inverse with solve, caches it, and returns it.
#The ... argument is used to pass additional arguments to solve, if needed.

#To use these functions, we can create a matrix object with makeCacheMatrix,
#set the matrix with set, and retrieve the inverse with cacheSolve.
#for example:



# create matrix object
m <- makeCacheMatrix(matrix(1:4, 2, 2))

# calculate inverse and cache result
cacheSolve(m)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# retrieve cached inverse
cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# update matrix and invalidate cache
m$set(matrix(c(1, 2, 3, 4), 2, 2))
cacheSolve(m)
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5


##the second call to cacheSolve retrieves the cached value
#rather than recalculating the inverse, and that the third call
#invalidates the cache by changing the matrix data.


####################### End ######################