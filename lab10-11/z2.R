f2 = function(x, y, yd) { return(-2 - (2*sin(x))*y1 - 3*cos(x) ) }
a1 = -2
b1 = 2
A1 = 2
B1 = 2
alpha1 = c(1,1)
beta1 = c(1,0)
n = 10
eps = 0.00001
kmax = 100

Norma = function(x)
{
	return(sqrt(crossprod(x,x)))
}
nLinDEbp = function(f, a, b, alpha, beta, A, B, n, eps, kmax)
{
	h = (b-a)/n
	x = c(1:(n+1))
	y = c(1:(n+1))
	
	for (i in 1:(n+1))
		x[i] = a + (i-1)*h
	y[1] = A
	y[n+1] = B
	for (i in 2:n)
		y[i] = (y[1] + y[n+1])/2
	C = matrix(0, nrow=(n+1), ncol=(n+1))
	d = c(1:(n+1))
	
	C[1,1] = (alpha[1])*h - alpha[2]
	C[1,2] = alpha[2]
	C[(n+1),n] = -beta[2]
	C[(n+1),(n+1)] = (beta[1])*h+beta[2]
	d[1] = A*h
	d[n+1] = B*h
	for (i in 2:n)
	{
		C[i,(i-1)] = 1
		C[i,i] = -2
		C[i,(i+1)] = 1
	}
	for (k in 0:kmax)
	{
		for (i in 2:n)
		{
			ydi = (y[i+1] - y[i-1])/(2*h)
			d[i] = f(x[i], y[i], ydi)*h^2
		}
		y1 = solve(C, d)
		if(Norma(y1-y) <= eps)
			break
		y = y1
	}
	return(cbind(x, y1))
}

XY = nLinDEbp(f2, a1, b1, alpha1, beta1, A1, B1, n, eps, kmax)
XY

plot(XY[,1], XY[,2], type="p")
n1 = 100
x = seq(a1, b1, len = n1)
y = c(1:n1)
for(i in 1:n1) { y[i] = 0.5*(log(x[i]))^2 }
lines(x, y, type="l", col="red")

		

