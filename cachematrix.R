#Creating the special main function (a vector) which contains other pertinent 
#functions:
makeCacheMatrix <- function(x= matrix()) {
     inv <-  NULL
     #Defining a function that will contain or set the value of the matrix;
  #The use of the <<- operator suggests that the assigned value to the variable 
  #is a different environment, i.e. in the parent level:
     set <-  function(y) {
          x <<- y
          inv <<- NULL
     }
     #Defining a function that will get the value of the matrix:
     get <-  function() {x}
     #Setting & getting the value of the inverse of the matrix:
     setInverse <-  function(solve) {inv <<- solve}
     getInverse <- function() {inv}
     list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}

#The functions below will be caching the inverse of a matrix rather than
#computing it, if the inverse is already available. This will help the system in 
#becoming more efficient.
cacheSolve <- function(x, ...){
     #This code returns the inverse matrix of a variable:
     inv <- x$getInverse()
     #Checking if the inverse of the matrix has already been computed; if so,
  #computation of its inverse will be skipped:
     if(!is.null(inv)) {
          #This message will be displayed if the inverse is already present in the 
    #previous cache:
          message("Getting cached data")
          return(inv)
     }
     #This part will compute for the inverse of the matrix if it is still not 
  #provided:
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}
