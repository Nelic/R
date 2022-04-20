MetLUSLE = function(A,b,n)
{
	L = matrix(0, nrow = n, ncol = n)
	U = matrix(0, nrow = n, ncol = n)
	
	for(i in 1:n)
		L[i,1] = A[i,1]
	for(j in 1:n)
		U[1, j] = A[1,j]/L[1,1]
	for(i in 1:n)
	{
		for(j in 1:n)
		{
			S = 0
			for(k in 1:i)
				S = S + L[i,k]*U[k,j]
			if( (i >= j) & (j > 1) )
				L[i,j] = A[i,j] - S 
			if( (j > i) & (i > 1) )
				U[i,j] = (A[i,j] - S)/L[i,i]
		}
		U[i,i] = 1
	}
	y = solve(L, b)
	x = solve(U, y)
	return(x)
}

A = cbind(c(1.14, 0.42, -0.71), c(-2.15, -1.13, 0.81), c(-5.11, 7.05, -0.02))
b = c(2.05, 0.80, -1.07)
n = 3
A
b
n
x = MetLUSLE(A, b, n)
x