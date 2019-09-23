require(plot3D)
require(ggplot2)
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
theta <-seq(0, 2*pi*A.REV,A.REV*360)


spirograph <-function(R=4,r=1.8,rev=10,a=-2,d=1.2){
theta <- seq(0, 2*pi*rev,0.1)  
xt =  (R-r)*cos(theta) + d*cos(theta*(R-r)/r)
yt =  (R-r)*sin(theta) - d*sin(theta*(R-r)/r)
#yt =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*cos(a*theta)
#zt =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*sin(a*theta)
return(data.frame(xt,yt))
}

spirograph3D <-function(R=4,r=1.5,rev=30,a=7,d=1.72){
  theta <-seq(0, 2*pi*rev,0.1)  
  x =  (R-r)*cos(theta)+d*cos(theta*(R-r)/r)
  y =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*cos(a*theta)
  z =  ((R-r)*sin(theta)-d*sin((R-r)/r*theta))*sin(a*theta)
  return(data.frame(x,y,z))
}

dat <- spirograph(R=5,r=35,rev=150,a=1,d=5)

ggplot(dat,aes(x=xt,y=yt),size=0.01) +
  geom_path(color="cyan3") + 
  theme_void()
  

dat3d <- spirograph3D(R=7,r=2,rev=150,a=5,d=1)

pdf("spir.pdf",height = 20,width = 20)
scatter3D(dat3d$x, dat3d$y, dat3d$z,type = "l",
          bty = "b", cex = 0.5, colkey = FALSE,box=F,alpha=0.8)
dev.off()

library(plotly)
plot_ly(x=dat3d$x, y=dat3d$y, z=dat3d$z,type = "scatter3d",mode="lines") 
