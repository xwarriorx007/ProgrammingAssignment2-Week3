## Put comments here that give an overall description of what your
## functions do
## Function makeCacheMatrix - gets a Matrix as an input (matrix), sets the value of
##sets the value of the matrix 
##gets the value of the matrix 
##set the Inverse Matrix 
##gets the Inverse Matrix 

##<<- operator is used to assign a value to an object in an environment that is different
##from the current environment 

## Write a short comment describing this function
##declaring that x is a matrix input by default

makeCacheMatrix <- function(x = matrix()) {
       m<-NULL
       
       ##set the value for the matrix 
       setMat<-function(y){
         x<<-y
         m<<-NULL
       }
       getMat<-function() x  ##get the value of the matrix
       setinv<-function(inverse) m <<-inverse ##set the value of the invertible matrix
       getinv <- function() m  ##gets the value of the invertible matrix 
       list(setMat=setMat,getMat=getMat,setinv=setinv,getinv=getinv)##outputs the values above and stores in 'CacheMatrix'
       
       }
}


## Write a short comment describing this function

##Takes the output of the previous function makeCacheMatrix(matrix)as an 
##input and checks the inverse matrix from makeCacheMatrix(matrix) is empty, it gets the original
##matrix data
##Using the solve function, sets the invertible matrix
##Returns Getting Caches Invertible Matrix and the cached object after running the function
##more than once

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m))                    ##inverse of the matrix is not null
  {
    message("Getting Cached Invertible Matrix")
    return(m)
  }
  MatData<-x$getMat()                ##get the original Matrix data 
  m<- solve(MatData, ...)            ## use solve function to inverse the matrix
  x$setinv(m)                        ##set the invertible matrix
  m                                  ## invertible matrix
  }
        ## Return a matrix that is the inverse of 'x'

