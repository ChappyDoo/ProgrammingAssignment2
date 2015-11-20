## These functions will take a matrix and put it to the cache memory
## (makeCacheMatrix) and then calculate the inverse of that matrix and commit
## it to cache memory as well (cacheSolve).

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse and test whether the matrix provided is new to the
## environment.

makeCacheMatrix <- function(z = matrix()) {
  #Creates a function to cache matrix and NULL inverse
  set <- function(y) {          
    x <<- y                     #assigns value passed to function to cached variable
    inv <<- NULL                #inv will be inverse matrix
  }
  
  #Function to return matrices value
  get <- function() z
  
  #Function to cache inverse value
  setinv <- function(inverse) inv <<- inverse
  
  #Function to return inverse matrix value
  getinv <- function() inv                     
  
  #Test for cached matrices and if 'new' matrices value was used previously
  if (is.null(r <- get0('x', envir = pos.to.env(-1L)))){   
    #assigns NULL to inv and z to x in the case that no matrices has been run
    set(z)      
  }else{
    #NULLs inv if not identical and sets z to x
    if (!(identical(x,z))) set(z) 
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  #Get current value of inv variable
  inv <- x$getinv()
  
  #Test whether inverse has been calculated previously
  if(!is.null(inv)) {  
    message("getting cached inverse")
    return(inv)
  }
  
  #Assigns matrix from makeCacheMatrix to data
  data <- x$get()
  
  #Invert matrix assigned to data
  inv <- solve(data)   
  
  #Set new value to inv and caches it
  x$setinv(inv)
  
  #Print inverse matrix
  inv                  
}