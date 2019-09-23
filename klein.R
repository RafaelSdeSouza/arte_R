require(plot3D)

v <- seq(0, 2*pi, length.out=100); 
u <- seq(0, pi, length.out=100); 

x<-outer(u,v,function(u, v) (-2/15)*cos(u)*(3*cos(v)-30*sin(u)+90*cos(u)^4*sin(u)-
60*cos(u)^6*sin(u)+5*cos(u)*cos(v)*sin(u)))

y <-outer(u,v,function(u,v) (-1/15)*sin(u)*(3*cos(v)-3*cos(u)^2*cos(v)-48*cos(u)^4*cos(v)+
                   48*cos(u)^6*cos(v)-60*sin(u)+ 5*cos(u)*cos(v)*sin(u)-
                   5*cos(u)^3*cos(v)*sin(u)-80*cos(u)^5*cos(v)*sin(u)+
                   80*cos(u)^7*cos(v)*sin(u)))

z <-outer(u,v,function(u,v)(2/15)*(3+5*cos(u)*sin(u))*sin(v))
 

#pdf("klein.pdf",height = 50,width = 50)
par(bg = "black")
surf3D(x, y, z, colvar = z,col  = viridis(400), colkey = FALSE, box = FALSE, 
       theta = 50, phi= 90,alpha=0.75)
#dev.off()
