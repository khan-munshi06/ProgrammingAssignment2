## my functions helps in caching inverse matrix so we do not have to calculate
## inverse matrix again. It first set all four function to retrieve and set values
##for functions and default value of x and matrix_inv

## this function sets initially matrix_inv value to null and we are setting 
##matrix_inv value to null in set() function to clear the memory when a different 
##value is passed on and assigns four function to use in cachesolve
##without defining them in cachesolve function

makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y) {
      x <<- y
      matrix_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) matrix_inv <<- inverse
    getinv <- function() matrix_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## this function try to retrieve inverse matrix for the object passed on from makecachematrix
##if retrieved value is not null then it returns inverse matrix value.
##if !is.null(matrix_inv) is false then it calculates inverse matrix by solve()
##next time we call the function it displays result "getting cached data" and just return
##inverse matrix value

cacheSolve <- function(x, ...) {
  matrix_inv <- x$getinv()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data, ...)
  x$setinv(matrix_inv)
  matrix_inv
}
## Return a matrix that is the inverse of 'x'
