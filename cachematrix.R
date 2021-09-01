## takes matrix-structured data as an argument and caches its inversion
## returns list of functions that read/modify data

makeCacheMatrix <- function(x = matrix()) {
     
     inverted <- NULL
     
     set_matrix <- function(arg) {
          x <<- arg
          inverted <<- NULL
     }
     set_inverted <- function(arg)
          inverted <<- arg
     
     get_matrix <- function() x
     get_inverted <- function() inverted
     
     list(set_matrix = set_matrix, set_inverted = set_inverted, get_matrix = get_matrix, get_inverted = get_inverted)
}


## takes an object created by makeCacheMatrix function as an argument, checks if inverted data have been cached, computes inverted matrix otherwise
## returns inverted matrix

cacheSolve <- function(x, ...) {
     if (is.null(x$get_inverted())) {
          data <- x$get_matrix()
          x$set_inverted(solve(data, ...))
     }
 
     x$get_inverted()
}
