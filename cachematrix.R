## Copyright by Xiou Cao
## The function makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the matrix, get the matrix, set the inverse of the mean, get the inverse of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
	set <- function(y)
	{
		x <<- y
		inv <<- matrix()
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computing the inverse matrix of a given matrix by first checking if the inverse has been computed. If not, 
## theinverse will be computed by solve().

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.na(inv[1,1])) 
	{
	  message("getting cached data")
	  return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
