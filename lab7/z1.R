F = function(x)
{
	return exp(sin(x))
}
a = 0
b = 1
n = 10

trapec = function(f, a, b, n)
{
	h = (b-a)/n
	s = (f(a)+f(b))/2
	x = c(1:(n+1))
	for (i in 2:n)
	{
		x[i] = a + (i-1) * h
		s = s + f(x[i])
	}
	return (s*h)
}

S = trapec(F, a, b, n)
S
