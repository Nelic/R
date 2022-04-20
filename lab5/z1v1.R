x = c(0, 1, 2, 3, 4, 5, 6)
y = c(1.8, 1.9, 2.3, 2.5, 2.8, 3.1, 2.5)
n = 7

#x = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
#y = c(9.8, 6.4, 5.2, 1.7, 2.9, 5.6, 7.2, 16.5, 27.5)
#n = 9

FunModel = function(x,a)
{
	return (a[1]+a[2]*x)
}

FunMNK = function(a)
{
	S = 0
	for(i in 1:n)
		S = S + (y[i]-FunModel(x[i],a))^2
	return(S)
}

a0 = c(0,0)
res = optim(fn=FunMNK, par=a0)
a1 = res$par
a1

FunMNK(a1)

ym = FunModel(x,a1)
r = y - ym
a = -1; b = 7; n = 100
h = (b-a)/(n-1)
x1 = c(1:n)
for(i in 1:n) { x1[i] = a + h*(i-1) }
y1m = FunModel(x1,a1)
par(mfrow=c(1, 2))
plot(x, y, type="p", col="red")
lines(x1, y1m, col="blue")
plot(x, r, type="p")
lines(x, r, col="blue")
abline(h=0)
abline(v=0)