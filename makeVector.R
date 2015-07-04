makeVector <- function(x=numeric()) {
	
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setmean <- function(mean) {
		m <<- mean
	}
	
	getmean <- function() {
		m
	}
	invisible(list(set = set, get = get, setmean = setmean, getmean = getmean))
}