##Kareem Phillip-Jackson
##Assignment 2
##Cache matrix

##takes a matrix as an argument
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##create an empty variable to hold cached content
          setMatrix <- function(y){ 
            ##Save the matrix in the cache
              x <<- y
              m <<- NULL
        }
        ##getMatrix function returns matrix stored in x
        getMatrix <- function () x
        ## setInverse function returns the inverse of the matrix and stores it in the cache
        setInverse <- function(solve) m <<- solve
        ## gets the inverse that is stored in the cache
        getInverse <- function() m
        ##Swap Inverse values
        list(setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
         }
     

##Solves the matrix of a function 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ##once the cache is not empty get stored inverse
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    ##Get matrix 
    data <- x$getMatrix()
    ##Solve for the inverse of the matrix and store in cache
    m <- solve (data,...)
    ##Assign new value for matrix in the cache
    x$setInverse(m)
    m
  }
