## function makeCacheMatrix will set  matrix and make a list of functions 
## available to other functions and clear the setsolve value if the matrix
## is changed. cacheSolve will check if getsolve has been calculated, if
## it has, it will be retreived, if not it will calculate it.
## The purpose is to only calculate the solve function when neccessary, as
## it reduces the load on the computer from continuously calculating the same
## values

## makeCacheMatrix is a function to check if a matrix is passed,
## define the functions of set, get, setsolve, getsolve and set the lexical
## values of x and m(NULL). Create a list of the functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve retrieves the value of getsolve, checks if it has a value, 
## If it has a value it returns the cached value, if not it runs the solve
## function and runs the setsolve function to cache it.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, data)
    x$setsolve(m)
    m
}
