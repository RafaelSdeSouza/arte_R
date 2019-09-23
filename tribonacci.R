a <- function(n) {
  if(n <= 1) {
    return(0)
  }
  if(n == 2) {
    return(1)
  }
  else {
    return(a(n-1) + a(n-2) +
             a(n-3)
           
           )
  }
}

ind <- seq(0,25)
sapply(ind,a)



#golden.ratio = (sqrt(5) + 1)/2
gr <- 1.839287
fibonacci.angle=360/(gr ^2)
c=1
num_points=630
x=rep(0,num_points)
y=rep(0,num_points)

for (n in 1:num_points) {
  r=c*sqrt(n)
  theta=fibonacci.angle*(n)
  x[n]=r*cos(theta)
  y[n]=r*sin(theta)
}
plot(x,y,axes=FALSE,ann=FALSE,pch=19,cex=1)



