## Put comments here that give an overall description of what your
## functions do
#functions that create a special matrix that caches its inverse

## Write a short comment describing this function
#create the casheable matrix named "makeCacheMatrix"


makeCacheMatrix <- function(x = matrix()) {
#create a null object
      inverted.matrix<-NULL
      set <- function(y){
            x <<- y
            inverted.matrix <<- NULL
      }   
#get and set cached inverted matrix value
      get <- function() x
      set.inverted.matrix <- function(solve) inverted.matrix <<- solve
      get.inverted.matrix <- function() inverted.matrix
      #create the list tha contains all the relevant values
      list(set=set, get=get, 
           set.inverted.matrix=set.inverted.matrix,
           get.inverted.matrix=get.inverted.matrix)
}


## Write a short comment describing this function
#calcualte the inverse of this matrix created by the "makeCacheMatrix" function
#but first check if the inverse has been calculated
#if so, the function "get" the inverse from the cache and just return the value
#if not, it calculates the inverse of the matrix and sets the value in the cache via the "set.inverted.matrix function


cacheSolve <- function(z, ...) {
        inverted.matrix <- z$get.inverted.matrix()
        #check if a cached matrix is already there
        if(!is.null(inverted.matrix)){
              message("getting cached inverted matrix")
              return(inverted.matrix)
        }
        #if not, create the inverted matrix and set the value in the cache
        data <- z$get()
        inverted.matrix <- solve(data, ...)
        z$set.inverted.matrix(inverted.matrix)
        inverted.matrix

}
