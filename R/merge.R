sa.vector.merge <- function(a, b) {
	# Determine step = how many rows from a we skip before adding each b
	step = length(a) / length(b)

	# Ensure step is a whole number (the number or rows of a is multiple of the number of rows of b)
	if (step%%1 != 0) {
		return(NULL)
	}

	# Calculate total length
	length.total = length(a) + length(b)

	# Allocate result vector
	result = numeric(length.total)

	# Determine positions for items from a and b
	seq.b = seq(from = 1 + step, to = length.total, by = 1 + step)
	seq.a = setdiff(1:length.total, seq.b)

	# Add values from a
	j = 1
	for (i in seq.a) {
		result[i] = a[j]
		j = j + 1
	}
	
	# Add values from b
	j = 1
	for (i in seq.b) {
		result[i] = b[j]
		j = j + 1
	}

	result
}

sa.vertical.matrix.merge <- function(a, b) {
	# Convert to matrixes
	a = as.matrix(a)
	b = as.matrix(b)

	# Get col and col names
	col.names = dimnames(a)[[2]]
	a.row.names = dimnames(a)[[1]]
	b.row.names = dimnames(b)[[1]]
	
	# Get dimensions
	a.dim = dim(a)
	b.dim = dim(b)

	# Ensure number of columns is the same
	if (a.dim[2] != b.dim[2]) {
		return(NULL)
	}

	# Merge values
	result = sa.vector.merge(a, b)

	# Merge row names
	row.names = NULL
	if (!is.null(a.row.names) && !is.null(b.row.names)) {
		row.names = sa.vector.merge(a.row.names, b.row.names)
	}

	# Convert result vector to matrix
	matrix(result, nrow = a.dim[1] + b.dim[1], ncol = a.dim[2], dimnames = list(row.names, col.names))
}
