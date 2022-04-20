F = function(x)
{
	return exp(sin(x))
}
a = 0
b = 1
n = 10

Simpson = function(f, a, b, n)
{
	h = (b-a)/n
	s1 = 0
	s2 = 0
	x = c(1:(n-1))
	for (k in 1:(n/2-1))
	{
		x[2*k] = a + (2*k)*h
		s1 = s1 + f(x[2*k])
	}
	for (k in 1:(n/2))
	{
		x[2*k-1] = a + (2*k-1)*h
		s2 = s2 + f(x[2*k-1])
	}
	return (h/3*(f(a)+f(b)+2*s1+4*s2))
}

S = Simpson(F, a, b ,n)
S