## The following functions create an object whih contains a matrix and has the ability to cache the
## solve or inverse matrix of the object's matrix in order to reduce repeat expensive computations

## makeCacheMatrix creates the object, with x as a matrix, s as the solve of the matrix
## and four functions which allow the object to manipulated: set, get, setsolve, getsolve.
## set/get allow the setting and getting of the basic matrix
## setsolve/getsolve allow the setting and getting of the inverse matrix, s.
## setsolve/getsolve allow the cacheSolve function to interact with the object in order to
## successfully cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ##The s variable will hold the solve/inverse matrix
  s <- NULL ##NULL as it has not been calculated yet
  set <- function(y) { ##This function allows the matrix to be set in the object
    x <<- y ##setting the variable in the object scope
    s <<- NULL ##setting the variable in the object scope
  }
  get <- function() x ##return the matrix as is
  setsolve <- function(solve) s <<- solve ##sets s to the inverse of x 
  getsolve <- function() s ##return the solve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve returns the solve of the cacheMatrix.
## It checks to see if the cacheMatrix object has the solve of its matrix computed. 
## If the matrix does not have the solve computed, it is computed and stored in the cacheMatrix
## object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve() ##get the solve from the object x
    if(!is.null(s)) { ##check to see that the solve is not NULL
      message("getting cached data") ##print that we are using the cached value of the solve
      return(s) ## return the cached value of the solve
    }
    data <- x$get() ##get the matrix from the object and store in data
    s <- solve(data) ##solve the matrix and store in s
    x$setsolve(s) ##set object x's solve to the calculated s
    s ##return the computed solve
  }
  
