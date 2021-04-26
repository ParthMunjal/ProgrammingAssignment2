
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL                             
  set <- function(y) {                     
    x <<- y                             
    inv_matrix <<- NULL                       
  }
  get <- function() x                   
  
  set_inverse <- function(inverse) inv_matrix <<- inverse  
  get_inverse <- function() inv                     
  list(set = set, get = get, setinverse = set_inverse, getinverse = get_inverse) 
}




cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv_matrix)
  inv_matrix     
}
