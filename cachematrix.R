## The `makeCacheMatrix` function essentially uses lexical scoping to
## create a matrix "object" with a cached inverse.  The `cacheSolve`
## function (named so as to be similar to the R function `solve` used
## to invert matrices) is used to make the necessary updates to the
## cache when the underlying matrix changes.


## As described above, the makeCacheMatrix (optionally) takes a matrix
## x as argument, and attempts to store a cached inverse.  It returns
## a list of functions which can be used to modify the underlying
## matrix, as well as the cached inverse.  This behavior is possible
## because R supports lexical scoping, so that the returned functions
## always refer to the the environment in which they were created.
##
## Note that we are assuming that the underlying matrix is invertible,
## so no checks are included to enforce this condition.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      ## y must be a matrix
      x <<- y ## <<- operator is used so that a new local binding is
              ## not created
      inv <<- NULL ## Updating x invalidates the previously cached (if
                   ## any) inverse, so we set it to NULL
  }
  get <- function() x       ## Simply return the matrix
  getinv <- function () inv ##Simply return the cached inverse
  setinv <- function (y) inv <<- y ## Set the value of the cached
                                   ## inverse.  Note that correctness
                                   ## of this operation is to be
                                   ## enforced by the function which
                                   ## calls `setinv`.

  ## Now return the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv);
}


## A sister function for `makeCacheMatrix`.  This usesxthe methods
## returned by the `makeCacheMatrix` function to return the inverse of
## a matrix object created using `makeCacheMatrix`.  If such an
## inverse is in the cache, then this function does not need to do any
## computations.  However, if the inverse is not in the cache, then it
## updates the cached inverse before returning it, so that future
## calls to `cacheSolve` can used the cached value directly.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.

  ## First check if the cache is valid.  If so return it.  Else,
  ## compute the inverse and update the cache. Any extra arguments
  ## passed as part of `...` are simply passed /as is/ to `solve`.

  inv <- x$getinv()
  if (!is.null(inv)){
      message("getting cached inverse") ## Issue a warning that the
                                        ## cache is being used
      return(inv)
  } else {
      ## The cache did not exist
      ## Compute the inverse of the stored matrix
      inv <- solve(x$get(), ...)
      x$setinv(inv) ## Cache the inverse...
      inv ## ..and return it
  }
}
