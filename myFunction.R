myfunction <- function(x) {
	y <- rnorm(100)
	mean(y)
}

second <- function(x){
	4 + rnorm(length(x))
}
