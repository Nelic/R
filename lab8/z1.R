FunPr = function(x,y)
{
	return (x + cos(y/sqrt(10)))
}
a = 0.6
b = 4.2
y0 = 0.8
n = 24

MetEiler = function(f, a, b, y0, n)
{
	h = (b-a)/n
	x = c(1:(n+1))
	y = c(1:(n+1))
	x[1] = a
	y[1] = y0
	for (i in 1:n)
	{
		x[i+1] = x[i] + h
		y[i+1] = y[i] + h*f(x[i],y[i])
	}
	return(cbind(x,y))
}

xy = MetEiler(FunPr, a, b, y0, n)
xy

