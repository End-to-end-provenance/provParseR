# single value
s1 <- 5L

# vector
v <- c(as.character(c(11:15)))

# matrix
m <- matrix(c(21:23), nrow=3, ncol=3)

# multi-dimensional array
a <- array(data = c(31:33), dim=c(3,3,3,3))

# list
l <- list(as.integer(c(41:45)), as.character(c(51:61)))

# data frame
df <- data.frame("col1" = as.integer(c(71:75)),
				 "col2" = as.character(81:85),
				 "col3" = as.factor(c(91:95)),
				 stringsAsFactors = FALSE)

# function
fn1 <- function(x) {
	return(as.factor(x))
}

# factor
s2 <- fn1("fruit")

# environment
e <- new.env()