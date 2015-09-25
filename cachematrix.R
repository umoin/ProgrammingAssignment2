## makeCacheMatrix returns a list object of functions to get and set the values of our special matrix. It also has functions to get and
## set the inverse of matrix. It it the function to create our special matrix

## cacheSolve function returns the cached inverse of matrix if already computed, otherwise it computes, caches and returns the inverse

##Example: 
## myMat <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2, ncol=2)). This call will create our matrix and return a list object, i.e. myMat will be a list of functions
## It will also initialize our speical matrix of 2 by 2 
## 1 2
## 3 4
## myMat$get() will display the data in our matrix
## myMat$set() can be used to change the values in our matrix, this will reset the inverse to NULL
## myMat$getinv() will return the inverse (it may be null, or already computed)
## myMat$setinv() is used to set the inverse. This is used by cacheSolve to cache the computed inverse
## myMatInv <- cacheSolve(myMat) will compute and cahce the inverse of our matrix myMat

## function  to create a matrix and return a list of function to get and set the values of matrix, and get and set the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
	# Initialize inverse matrix to be null
	invMat <- NULL

	# function to set values in matrix
	set <- function(y) {

		#assign  the values to matrix
		x <<- y

		#since the matrix has changed, set the inverse to NULL
		invMat <<- NULL
	}

	# function  to see the value of matrix
	get <- function() x

	# function to set the inverse of matrix
	setinv <- function(invrs) invMat <<- invrs

	#function to get the inverse of matrix
	getinv <- function() {
		if(!is.null(invMat)) {
			message("getting cached inverse")
			return(invMat) 
		}
		else {
			message("Inverse has not yet been computed  for this matrix.")
		}
	}

	#return a  list object where elements are functions
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}


#function to return cached inverse  of a matrix  if already computed, otherwise to compute and cache the inverse
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	#try to get the inverse matrix if already computed  
	invMat <- x$getinv()
	if(!is.null(invMat)) {
		return(invMat)
	}

	#otherwise compute and cache the inverse matrix
	data <- x$get()
	
	message("Computing Inverse of matrix.")
	invMat <- solve(data) # Solve(X) computes  the inverse

	#cache the computed inverse
	message("Caching computed inverse")
	x$setinv(invMat)

	#return the computed inverse
	invMat
}
