## Programming Assignment 2 for the Coursera R Programming course - February, 2015

## Overall description: Matrix Inversion
##   These functions inverse a matrix.  They improve the performance of this 
##   time-consuming computation by storing values in cache and recalling them
##   ss needed rather than repeatedly computing the values (as in loops).


## MakeCacheMatrix
##   The MakeCacheMatrix function creates special matrixes for the original matrix
##   and its inverse and stores them in cache.  It returns a list of 4 functions 
##   to the user which do the following:
##      set the value of the original matrix
##      get the value of the original matrix
##      set the value of the inverse
##      get the value of the inverse
##   Datanames: om is the orginal matrix
##              im is the inverse matrix
makeCacheMatrix <- function(om = matrix()) {    
		# create matrix om to hold the original matrix from argument passed to it.
		# create empty matrix im to hold the inverse matrix
        im <- NULL
		# function to store the matrix om and im in cache
        set <- function(y) {
                om <<- y         # store the original matrix  om
                im <<- NULL      # store the inverse matrix   im  which is empty
        }
		# create function to get the original matrix om that is stored in cache
        get <- function() om
		# create function named setinverse to store the inverse in cache
        setinverse <- function(inverse) im <<- inverse
		# create function named getinverse to return the inverse stored in cache
        getinverse <- function() im
		# create a list of the 4 functions as output
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
##   The cacheSolve function calculates the inverse of the matrix
##   created in the makeCacheMatrix function.  If the inverse already 
##   exists in cache it uses that value.  If the inverse does not exist
##   in cache it calculates the inverse and places it cache.
cacheSolve <- function(om, ...) {
			# get the inverse from cache vector.
	        im <- x$getinverse()
		# check to see if the inverse is in cache.  If it is post a message and exit
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
		# inverse was not in cache so caclulate it
        # get the data from the original matrix vector into data
		data <- om$get()
		# calculate the new inverse matrix
		im <- solve(data, ...)
		# stores the newly calculated inverse in cache
        x$setinverse(im)
		# return the inverse im
        im
}