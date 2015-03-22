# To calculate the inverse, call cacheSolve like this cacheSolve(matrix). The symbol matrix is the input matrix.



# This function is used to retuan a special list which is responsioble for get/set the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL
	getCache <- function() {
		cache
	}

	setCache <- function(inverse) {
		cache <<- inverse
	}

	getMatrix <- function() {			
		x
	}

	setMatrix <- function(newMatrix) {
		x <<- newMatrix
		cache <<- NULL
	}

	list(setCache = setCache, getCache = getCache, getMatrix = getMatrix, setMatrix = setMatrix)


}

func <- makeCacheMatrix()

# This function is used to calculate the inverse of certain matrix.
# newMatrix is the input matrix whose inverse will be return.
# funcList is a list of function return by makeCacheMatrix. We have create one default funcList in the global environment to serve as the default value.
cacheSolve <- function(newMatrix = NULL, funcList = func) {
	data <- NULL
	if (!is.null(newMatrix)) {
		funcList$setMatrix(newMatrix)
		data <- newMatrix
	} else {
		cache <- funcList$getCache()
		if (!is.null(cache)) {
			message("read inverse from cache")
			return (cache)
		} else {
			data <- func$getMatrix()
		}
	}

	result <- solve(data)
	funcList$setCache(result)
	result
}

