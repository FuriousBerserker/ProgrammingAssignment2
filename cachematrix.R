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

cacheSolve <- function(funcList = func, newMatrix = NULL) {
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
			data <- getMatrix()
		}
	}

	result <- solve(data)
	funcList$setCache(result)
	result
}

