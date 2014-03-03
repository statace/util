sa.as.formatted.factor <- function(var) {
	# Only format with decimals if numeric but not integer
	if (is.numeric(var) && !is.integer(var)) {
		unq <- unique(na.omit(var))
		frmtd <- sprintf("%.3f", unq)
		return(factor(var, levels = unq, labels = frmtd))
	}
	else {
		# Format complex with significant digits
		if (is.complex(var)) {
			unq <- unique(var)
			frmtd <- format(unq, digits = 3)
			return(factor(var, levels = unq, labels = frmtd))
		}
		# Everything else is a straight factor
		else {
			return(factor(var))
		}
	}
}

sa.add.matrix.row.subname <- function(x, name) {
	x = as.matrix(x)
	name = as.character(name)

	row.names = dimnames(x)[[1]]
	col.names = dimnames(x)[[2]]

	num.rows = dim(x)[1]
	new.row.names = paste(row.names, rep(name, num.rows), sep = ":: ")

	dimnames(x) <- list(new.row.names, col.names)

	x
}

sa.get.dot.parameter.names <- function(...) {
	# Get the names of the variables passed (e.g. "a, b, c")
	dots <- substitute(list(...))[-1]
    var.names <- sapply(dots, deparse)

	# Get the given names of the variables that were passed (e.g. "Houses = h, Bikes = b")
	given.var.names <- names(list(...))

	# If there were given names, overwrite the names of the variables	
	if (!is.null(given.var.names)) {
		# Get the indexes of the non-empty names
		non.empty.indexes = which(given.var.names != "")
		
		# Get the non-empty names
		non.empty.names = given.var.names[non.empty.indexes]

		# Set the non-empty given names into the variable names
		var.names = replace(var.names, list = non.empty.indexes, values = non.empty.names)
	}

	var.names
}

sa.set.transposed <- function(x) {
	attr(x, "sa.transp") <- TRUE
	x
}

sa.set.no.na.row.highlight <- function(x) {
	attr(x, "sa.no.na.row.highlight") <- TRUE
	x
}

sa.set.no.na.print <- function(x) {
	attr(x, "sa.no.na.print") <- TRUE
	x
}

sa.mark.object <- function(x) {
	# Mark as a StatAce object
	class(x) <- c(class(x), "sa.object")
	x
}

sa.set.place.plot <- function(fun) {
	assign(".place.plot", fun, envir = .env)
}

sa.place.plot <- function() {
	if (exists(".place.plot", envir = .env)) {
		.env$.place.plot()
	}
	else {
		NULL
	}
}

# Declare an environment to hold the variables
.env = new.env()