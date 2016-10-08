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
xt = function(t) exp(-d1*t)*sin(t*f1+p1)+exp(-d2*t)*sin(t*f2+p2)
yt = function(t) exp(-d3*t)*sin(t*f3+p3)+exp(-d4*t)*sin(t*f4+p4)
zt = function(t) exp(-d5*t)*sin(t*f5+p5)+exp(-d6*t)*sin(t*f6+p6)
t=seq(1, 100, by=.001)
dat=data.frame(t=t, x=xt(t), y=yt(t), z = zt(t))

op = par(bg = "black", mar = rep(0.1, 4))
with(dat, plot(x,y, type="l",col="cyan",
xlim =c(-2,2), ylim =c(-2,2), xlab = "", ylab = "", xaxt='n', yaxt='n'))


scatter3D(dat$x, dat$y, dat$z,phi=15,theta=10,type = "l",
          bty = "b", cex = 5, colkey = FALSE,box=F,alpha=0.8)

frames = 360

  for(i in 1:frames){
# creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}

  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  ang = i
#saves the plot as a .png file in the working directory
  png(name)
scatter3D(dat$x, dat$y, dat$z,phi=15,theta=ang,type = "l",
          bty = "b", cex = 5, colkey = FALSE,box=F,alpha=0.8)

  dev.off()

}


library(plotly)
plot_ly(x=dat$x, y=dat$y, z=dat$z,type = "scatter3d",mode="lines") 

spheres3d(x=dat$x, y=dat$y, z=dat$z)
