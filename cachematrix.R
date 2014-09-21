## There are two functions below
##makeCacheMatrix - for creating a matrix
##cacheSolve - to calculate the inverse of matrix returned by makeCacheMatrix
## Variable 'mat' used to hold matrix
## Variable 'inv_mat' used to hold inverse matrix

## Function to create matrix


makeCacheMatrix <- function(mat = matrix()) {
			inv_mat <- NULL
			set <- function(y) { 
			mat <<- y
			inv_mat <<- NULL
			}
			get <- function()  mat 
			setmatrix <- function(solve) inv_mat <<- solve
			getmatrix <- function()  inv_mat 
			list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
			
}


## Function to Calculate inverse of the matrix

cacheSolve <- function(mat=matrix(), ...) {
        ## Return a matrix that is the inverse of 'mat'
		inv_mat <- mat$getmatrix()
		if(!is.null(inv_mat)) {
		message("getting cached data")
		return(inv_mat)
		}
		 matrix <- mat$get()
		  inv_mat <- solve(matrix, ...)	
		mat$setmatrix(inv_mat)

		inv_mat

}
