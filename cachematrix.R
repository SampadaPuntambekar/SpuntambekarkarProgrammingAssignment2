##Below are the two function one of which creates and stores the 
##matrix and another one caches its inverse.

## This fucntion creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y) {
		x <<- y
		inver <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inver <<- inverse
	getInverse <- function() inver
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)

}


## This fuction inverse the matrix created by makeCacheMatrix
##fucntion. It Checks if inverse has been calculated before and
##matrix is not changed. if it is, then it will retrive the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if(!is.null(inver)) {
        	message("getting cached data")
        	return(inver)
        }
        matrix <- x$get()
        inver <- solve(matrix, ...)
        x$setInverse(inver)
        inver
}
