## Put comments here that give an overall description of what your
## functions do

## This object factory creates objects which take a matrix as input and additionally store the inverse
## calculated by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  internal_matrix <<- x
  cached_inv <<- NULL

  list(
    get = function() {
      internal_matrix
    },
    set = function(input_matrix = matrix() ){
      internal_matrix <<- input_matrix
      cached_inv <<- NULL #since we're modifying the stored matrix, invalidate the cache
    },
    getcache = function() {
      cached_inv
    },
    setcache = function(inv_input) {
      if(is.null(cached_inv)) {
        print("Setting Cache")
      } else {
        print("Overwriting Cache")
      }
      cached_inv <<- inv_input
    }
  )
}

## This function takes a makeCacheMatrix object as it's input to caluculate the inverse matrix
## storing the result in the original object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getcache()
  if(!is.null(inv_mat)){
    print("Returning inverse matrix from cache")
  } else {
    print("Calculating Inverse of Matrix")
    inv_mat=solve(x$get())
    x$setcache(inv_mat)
  }
  inv_mat
}