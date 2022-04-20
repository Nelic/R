p1 = function(x) { return(2*sin(x)) }
q1 = function(x) { return(3*cos(x)) }
f1 = function(x) { return(-2) }
a1 = -2
b1 = 2
A1 = 2
B1 = 2
alpha1 = c(1,1)
beta1 = c(1,0)
n = 10

LinDEbp = function(p, q, f, a, b, alpha, beta, A, B, n)
{
	h = (b-a)/n
	x = c(1:(n+1))
	for (i in 1:(n+1))
	{
		x[i] = a + (i-1)*h
	}
	C = matrix(0, nrow=(n+1), ncol=(n+1))
	d = c(1:(n+1))
	
	C[1,1] = (alpha[1])*h - alpha[2]
	C[1,2] = alpha[2]
	C[(n+1),n] = -beta[2]
	C[(n+1),(n+1)] = (beta[1])*h+beta[2]
	for (i in 2:n)
	{
		C[i,(i-1)] = 1-h*p(x[i])/2
		C[i,i] = (h^2)*q(x[i])-2
		C[i,(i+1)] = 1+h*p(x[i])/2
	}
	d[1] = A*h
	d[n+1] = B*h
	for (i in 2:n)
	{
		d[i] = f(x[i])*h^2
	}
	y = solve(C, d)
	return(cbind(x, y))
}
XY = LinDEbp(p1, q1, f1, a1, b1, alpha1, beta1, A1, B1, n)
XY

plot(XY[,1], XY[,2], type="p")
n1 = 100
x = seq(a1, b1, len = n1)
y = c(1:n1)
for(i in 1:n1) { y[i] = 0.5*(log(x[i]))^2 }
lines(x, y, type="l", col="red")