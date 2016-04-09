## The functions below cache the inverse of a matrix

## This first function creates a list of function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setInvertedMatrix <- function(InvertedMatrix) m <<- InvertedMatrix
      
      getInvertedMatrix <- function() m
      
      list(set = set, get = get,
           setInvertedMatrix = setInvertedMatrix,
           getInvertedMatrix = getInvertedMatrix)
}


## This second function gets the inversion of a matrix if the matrix has already
## been inverted, or inverts it if the inversion has not yet been done

cacheSolve <- function(x, ...) {
      f <- x$getInvertedMatrix()
      
      if(!is.null(f)) {
            message("getting cached data")
            return(f)
      }
      
      data <- x$get()
      
      f <- solve(data, ...)
      x$setInvertedMatrix(f)
      f
}
