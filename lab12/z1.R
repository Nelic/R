a = 0; 
b = pi;
lambda = (1/7);
FunCore = function(x, t) 
{ 
	return (1/(5 + sin(x+t)*sin(x+t)))
}

FunF = function (x) 
{ 
	return (cos(x)) 
}

MetTrap = function(x, y, n)
{
	h = x[2] - x[1]
	S = (y[1] + y[n])/2
	for(i in 2:(n-1)) {S = S + y[i] }
	return (h*S)
}

Norma = function(x)
{
	return (sqrt(crossprod(x, x)))
}

n = 10; eps = 1E-5
h = (b-a)/(n-1)
x = c(1:n)
for(i in 1:n) { x[i] = a + h*(i-1) }
t = c(1:n)
t = x

K = matrix(0, ncol=n, nrow=n)
for(i in 1:n)
{
	for(j in 1:n) {K[i,j] = FunCore(x[i], t[j]) }
}
Fi = c(1:n);
FiNext = c(1:n)

for(i in 1:n)
{
	Fi[i] = FunF(-cos(x[i])) 
}

y = c(1:n)
for(ki in 1:100)
{
	for(i in 1:n)
	{
		for(j in 1:n) { y[j] = K[i,j]*Fi[j] }
		FiNext[i] = FunF(x[i]) + lambda*MetTrap(t, y, n)
	}
	if ( Norma(FiNext-Fi) <= eps)
		break
	Fi = FiNext
}
ki

plot(x, FiNext, type="p", col="red")
t = seq(a, b, len=100)
for(i in 1:100) { y[i] = 1+6*t[i]*t[i] }
lines(x, FiNext, type="l", col="blue")


