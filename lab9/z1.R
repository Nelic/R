FunPr = function(x,y)
{
	return (cos(x+y)+0.75*(x-y))
}
a = 0
b = 1
y0 = 0

MetAdamBosh4 = function(fun, a, b, y0, n)
{
	h = (b-a)/n
	x = c(1:(n+1))
	y = c(1:(n+1))
	f = c(1:(n+1))
	x[1] = a
	y[1] = y0
	f[1] = fun(x[1], y[1])
	for (i in 1:3)
	{
		x[i+1] = x[i] + h
		k1 = h*f[i]
		k2 = h*fun(x[i]+h/2, y[i]+k1/2)
		k3 = h*fun(x[i]+h/2, y[i]+k2/2)
		k4 = h*fun(x[i]+h, y[i]+k3)
		y[i+1] = y[i] + 1/6*(k1 + 2*k2 +2*k3 +k4)
		f[i+1] = fun(x[i+1], y[i+1])
	}
for (i in 4:n)
{
	x[i+1] = x[i] + h
	y[i+1] = y[i] + h/24*(55*f[i] - 59*f[i-1] +
			+ 37*f[i-2] - 9*f[i-3])
	f[i+1] = fun(x[i+1], y[i+1])
}
return(cbind(x,y))
}

n = 10
xy = MetAdamBosh4(FunPr, a, b, y0, n)
xy

x = xy[, 1]
y = xy[, 2]
plot(x, y, type = "p", col="red")
#FunAn = function(x)
#{
#	return (2*exp(x) - x - 1)
#}
#x1 = seq(x[1], x[n+1], by=0.01)
#y1 = FunAn(x1)
lines(xy, col="blue")
