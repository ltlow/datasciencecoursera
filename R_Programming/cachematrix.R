      ## As required in the assignment, this script contains two functions, namely  
      ## makeCacheMatrix and casheSolve, which caches and computes its inverse of a matrix.     

      ## (1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

      makeCacheMatrix <- function(x = matrix()) {

          ## Set the matrix
          i <- NULL
          set <- function(y) {
          x <<- y
          i <<- NULL
          }
          ## Get the matrix
          get <- function() 
          x
          }
          ## Set the inverse of the matrix
          setInverse <- function(inverse) {
          i <<- inverse
          }
          ## Get the inverse of the matrix
          getInverse <- function() 
          i
          }
          
          ## Return a list containing the functions
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
      }

      ## (2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
      ##     If the inverse has already been calculated (and the matrix has not changed), 
      ##     then the cachesolve should retrieve the inverse from the cache.
  
      cacheSolve <- function(x, ...) {
          
          ## Return a matrix that is the inverse of 'x'
          i <- x$getInverse()
          if(!is.null(i)) {
          message("getting cached data")
          return(i)
          }
          data <- x$get()
          i <- solve(data,...)
          x$setInverse(i)
          i
          }

      }
