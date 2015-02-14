
## Programming Assignment 2 for the Coursera R Programming course - February, 2015
 
## Overall description:
##   This function improves the performance of time-consuming computations
##   by caching some re-occuring values and recalling them from cache
##   rather than repeatedly computing them (as in loops).


## MakeCacheMatrix
##   The MakeCacheMatrix function creates several different functions and 
##   returns them to the user in a list.  This contains functions to 
##     set the value of the vector
##     get the value of the vector
##     set the value of the mean
##     get the value of the mean 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		# create function names set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		# create function named get
        get <- function() x
		# create function named setmean
        setmean <- function(mean) m <<- mean
		# create function named getmean
        getmean <- function() m
		# create a list of the 4 functions as output
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## cacheSolve
##   The cacheSolve function calculates the mean of the vector
##   created in the makeCacheMatrix function.  If the mean already 
##   exists in cache it uses that value.  If the mean does not exist
##   in cache it caculates the mean and places it cache.
cacheSolve <- function(x, ...) {
			# create a vector to hold the mean
	        m <- x$getmean()
		# check to see if the mean is in cache, get it and exit
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		# mean was not in cache so caclulate it
        # get the data into vector named data
		data <- x$get()
		# calculate the new mean
        m <- mean(data, ...)
		# set the newly calculated mean into cache
        x$setmean(m)
		# return the mean
        m
}
