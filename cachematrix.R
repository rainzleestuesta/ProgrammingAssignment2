## This script defines two functions that work together to cache the inverse of
## a square, invertible matrix so that the expensive inversion is performed at
## most once per matrix.  The design mirrors the example given for caching the
## mean of a numeric vector, but is adapted to matrices and their inverses.
##
##   * makeCacheMatrix() – creates a special "matrix" object (really a list of
##     closures) that stores a matrix and, optionally, its inverse.
##   * cacheSolve()     – retrieves the cached inverse from that object or, if
##     not yet cached, computes it with solve(), stores it, and returns it.
##
## Assumptions:
##   * The matrix supplied is always square and invertible.
##   * If a new matrix is supplied via set(), any previously cached inverse is
##     invalidated (set to NULL).
##-----------------------------------------------------------------------------


##-----------------------------------------------------------------------------
## makeCacheMatrix -------------------------------------------------------------
##-----------------------------------------------------------------------------
## Constructor for the special "matrix" object that can cache its inverse.
##
## Args:
##   x : A square numeric matrix (default: empty matrix()).
##
## Returns:
##   A list containing four functions that operate on the enclosed matrix:
##     set(y)      – replace the stored matrix with y and reset cached inverse
##     get()       – return the stored matrix
##     setinv(inv) – store the inverse matrix inv
##     getinv()    – return the cached inverse (or NULL if not yet cached)
##-----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                       # cached inverse (initially NULL)
  
  set <- function(y) {
    if (!is.matrix(y))
      stop("Input must be a matrix.")
    x   <<- y                     # update matrix in parent env
    inv <<- NULL                  # reset cached inverse
  }
  
  get <- function() x               # return the current matrix
  
  setinv <- function(inverse) inv <<- inverse   # cache the inverse
  
  getinv <- function() inv          # return cached inverse (may be NULL)
  
  # Expose the four functions as a list
  list(set    = set,
       get    = get,
       setinv = setinv,
       getinv = getinv)
}


##-----------------------------------------------------------------------------
## cacheSolve ------------------------------------------------------------------
##-----------------------------------------------------------------------------
## Compute the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not been
## changed), the cached value is retrieved to avoid redundant computation.
##
## Args:
##   x   : An object created by makeCacheMatrix().
##   ... : Additional arguments forwarded to solve() (e.g., tol).
##
## Returns:
##   A matrix that is the inverse of the matrix stored in x.
##-----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  inv <- x$getinv()                 # attempt to fetch cached inverse
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()                    # retrieve original matrix
  inv <- solve(mat, ...)            # compute its inverse
  x$setinv(inv)                     # cache the inverse for next time
  inv                               # return the inverse
}


## Test
m  <- matrix(c(2, 1, 1, 2), 2, 2)
cm <- makeCacheMatrix(m)

cacheSolve(cm)  # First call: computes inverse
cacheSolve(cm)  # Second call: retrieves cached inverse

cm$set(matrix(c(1, 2, 3, 4), 2, 2))
cacheSolve(cm)  # Recomputes because cache was invalidated