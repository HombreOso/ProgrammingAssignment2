## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_m) matrix_inv <<- inverse_m
  getinv <- function() matrix_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$getinv()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(invisible(matrix_inv))
  }
  data <- x$get()
  matrix_inv <- solve(data)
  x$setinv(matrix_inv)
  invisible(matrix_inv)
}

library(tictoc) 

example_matrix <- matrix(rnorm(1000*1000,0,1), 1000, 1000)
cached_example_matrix <- makeCacheMatrix(example_matrix)

tic()
cacheSolve(cached_example_matrix)
toc()

tic()
cacheSolve(cached_example_matrix)
toc()

example_matrix <- matrix(rnorm(1000*1000,0,1), 1000, 1000)
cached_example_matrix <- makeCacheMatrix(example_matrix)

tic()
cacheSolve(cached_example_matrix)
toc()



