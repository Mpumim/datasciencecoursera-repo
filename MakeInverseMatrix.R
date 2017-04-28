makeCacheMatrix <- function(x=matrix()){
  cacheMatrix  <- NULL
  set <- function(y){
    
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  
  setInverseMatrix <- function(inverse) cacheMatrix <<- inverse
  getInverseMatrix <- function() cacheMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}
cacheSolve <-function(x, ...){
  cacheMatrix <-x$getInverseMatrix()
  if(!is.null(cacheMatrix)){
    message("getting cached data")
    return(cacheMatrix)
    }
    temp <- x$get()
    cacheMatrix <- solve(temp)
    x$setInverseMatrix(cacheMatrix)
    cacheMatrix
    


}