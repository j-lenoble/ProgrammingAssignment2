#Prepared December 31, 2019 to fulfill requirements for the Coursera R Programming Week 3 Assignment
#JLN

## This function creates a special "matrix" object that can cache its inverse.
#input the matrix
makeCacheMatrix <- function(x = matrix()) {
  iMat <- NULL                                        #create empty object
  setM <- function(y) {                               #define setM to assign new matrix
    x <<- y                                           #cache y to the parent environment stored as x
    iMat <<- NULL                                     #empty the object
  }

#set the matrix values and invert the matrix
  getM <- function() x                                #returns the value of the matrix argument
  setInverse <- function(inverse) iMat <<- inverse    #assign value to iMat in parent environment
  getInverse <- function() iMat                       #get value of iMat
  list(setM = setM, getM = getM,                      #list functions in cached matrix
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  iM <- x$getInverse()                               #get the inverted matrix from the makeCacheMatrix function
  if(!is.null(iM)) {                                 #if value of matrix is not Null
    message("Getting Cached Invertible Matrix")      #give the programmer a message that the matrix is being retrieved
    return(invMatrix)                                #retrieve the inverted matrix
  }
  
  mDat <- x$getM()                                  #if the value of matrix is NUll
  iM <- solve(mDat, ...)                            #solve the matrix inversion
  x$setInverse(iM)                                  #set the inverted matrix
  return(iM)                                        #return the inverted matrix
}
  
  