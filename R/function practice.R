x <- 3
x^2
square <- function(x) {
	square_val <- x^2
	return(square_val)
}

square(53)
53^2

raise <- function(x, power = 2) {
	raise_val <- x^power
  return(raise_val)
}

raise(x=2, power=4)
2^4

raise(x=2)


raise2 <- function(x, power) {
	if(missing(power)) {
		raise_val <- x^2
		return(raise_val)
	}

	else {
		raise_val2 <- x^power
		return(raise_val2)
		}
}

raise2(x=2)
raise2(2,4)
