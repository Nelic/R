MetIterSLE = function(C,d,n,eps,kmax)
{
	x0 = d
	for (k in 1:kmax)
	{
		x = C%*%x0 + d
		if(norm(x-x0) <= eps)
			break
		x0 = x
	}
	return(x)
}

MetJakobiSLE = function(A,b,n,eps,kmax)
{
	C = matrix(0, ncol = n, nrow = n)
  	d = c()
	for (i in 1:n)
	{
		for (j in 1:n)
		{
			C[i,j] = -A[i,j] / A[i,i]
		}
		C[i,i] = 0
		d[i] = b[i]/A[i,i]
	}
	x = MetIterSLE(C,d,n,eps,kmax)
	return(x)
}


A = cbind(c(1.14, 0.42, -0.71), c(-2.15, -1.13, 0.81), c(-5.11, 7.05, -0.02))
b = c(2.05, 0.80, -1.07)
n = 3
eps = 0.0001
kmax = 4

x = MetJakobiSLE(A,b,n,eps,kmax)
x

y = A%*%x -b
y