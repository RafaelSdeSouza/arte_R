require(plot3D)
f1=jitter(sample(c(2,3),1));
f2=jitter(sample(c(2,3),1))
f3=jitter(sample(c(2,3),1));
f4=jitter(sample(c(2,3),1));
f5=jitter(sample(c(2,3),1));
f6=jitter(sample(c(2,3),1))
d1=runif(1,0,1e-02);
d2=runif(1,0,1e-02);
d3=runif(1,0,1e-02);
d4=runif(1,0,1e-02);
d5=runif(1,0,1e-02);
d6=runif(1,0,1e-02);

p1=runif(1,0,pi);
p2=runif(1,0,pi);
p3=runif(1,0,pi);
p4=runif(1,0,pi);
p5=runif(1,0,pi);
p6=runif(1,0,pi)
A.REV<-4
theta <-seq(0, 2*pi*A.REV, ,A.REV*360)
A.RADIUS=1, 
B.RADIUS=-4, 
BC=-2, 
A.REV=4, 
N.PER.A.REV=360,
A.CEN=list(x=0, y=0))


spirograph<-function(R=4,r=1.8,rev=10,a=-2,d=1.2){
theta <-seq(0, 2*pi*rev,0.1)  
xt =  (R-r)*cos(theta)+d*cos(theta*(R-r)/r)
yt =  R*sin(theta)-d*sin(theta*(R-r)/r)
#yt =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*cos(a*theta)
#zt =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*sin(a*theta)
return(data.frame(xt,yt))
}

spirograph3D<-function(R=4,r=1.5,rev=30,a=7,d=1.72){
  theta <-seq(0, 2*pi*rev,0.1)  
  x =  (R-r)*cos(theta)+d*cos(theta*(R-r)/r)
  y =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*cos(a*theta)
  z =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*sin(a*theta)
  return(data.frame(x,y,z))
}
dat<-spirograph3D()

scatter3D(dat$x, dat$y, dat$z,type = "l",
          bty = "b", cex = 5, colkey = FALSE,box=F,alpha=0.8)


library(plotly)
plot_ly(x=dat$x, y=dat$y, z=dat$z,type = "scatter3d",mode="lines") 
