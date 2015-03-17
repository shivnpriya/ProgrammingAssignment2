## Caching inverse of a matrix

## Make a special matrix which is actually a list of 4 functions 
## get, set, getInverse and setInverse

makeCacheMatrix <- function(x = matrix()) 
{
	mat <- NULL
	set <- function(y)
	{
		x <<- y  ## assigning value of x in global environment
		mat <<- NULL
	}
	get <- function()
	{
		x
	}
	setInverse <- function(inv) 
	{
		mat<<- inv ## assigning value of mat in global environment
	}
	getInverse <- function()
	{
		mat
	}
	list(get = get,set = set, getInverse = getInverse,setInverse = setInverse)
}


## Function that computes the matrix inverse using solve function; 
## Assuming square matrices

cacheSolve <- function(x= matrix(), ...) 
{
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv))
	{
		message("getting cached data")
		
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
