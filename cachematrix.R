## These functions create a matrix, obtain its inverse and save it in the cache


## The makeCacheMatrix function takes a matrix as its argument and produces  
## a list of functions that operate on it, namely  setting a matrix, getting it, 
## setting its inverse and obtaining this inverse


makeCacheMatrix <- function(x = matrix()) {
			
			inv<-NULL
			
			set<-function(y){
					x<<-y                 ## we use the superassignment operator, so that the new matrix is set 
										## and stored and known for the rest of the makeCacheMatrix function, 
										## and for the same reason we use it to reset the inverse (inv) to NULL.
					inv<<-NULL
			}
		
			get<-function() x
			setinverse<-function(inverse) inv<<-inverse  ##the setinverse function makes the variable "inv" to store
														##the inverse of the matrix, again with the superassignment 
														##operator in order to overrride any other values of the variable 
														##within the makeCacheMatrix function.
			getinverse<-function() inv							
			
			list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)   ## the makeCacheMatrix function returns a list.

}


##  The cacheSolve function takes the list created in makeCacheMatrix as and argument and operates with 
## it in order to calculate the inverse of the matrix and store it in the cache. 
## But, if the inverse has already been calculated, it does not do it again; it simply retrieves 
## the cached data. In the end, it returns the inverse of the matrix that we used as an argument for makeCacheMatrix.

cacheSolve <- function(x, ...) {
			
			inv<-x$getinverse()		##cacheSolve starts by assigning the value of getinverse() to "inv" in order to check if it was already calculated.
			
			if(!is.null(inv)){		## here it checks if "inv" has a value different than 0 and, if so, it returns it from the cache.
					message("getting cached data")
					return(inv)
			}
			
			data<-x$get()			## in the case there was no value in inv, cacheSolve, calculates it by getting the matrix 
			inv<-solve(data,...)    ##from makeCacheMatrix, calculating its inverse with the function solve and setting the value 
			x$setinverse(inv)		## as the variable "inv" in the makeCacheMatrix with $setinverse, so that the following time it is remembered.
			inv
	
}
