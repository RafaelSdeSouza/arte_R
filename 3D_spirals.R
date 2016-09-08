require(plot3D)
## 
## sctt3D> # save plotting parameters
 pm <- par("mfrow")

 mypal<-c("#009b3a", "#fedf00", "#002776", "white")
 
 col=c()
 for(i in 1:100){
 col[i] = mypal[i%%5+1]
 }
 
 op = par(bg = "gray95", mar = rep(0.1, 4))
## 
  M  <- mesh(seq(0, 2*pi, length.out = 100), 
             seq(0,  2*pi, length.out = 100))
## 
  t  <- M$x ; s  <- M$y
## 
  x <- 5*sin(t)^3*cos(s)
## 
  y <- 5*sin(t)^3*sin(s)
## 
  z <- (13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t))/16
## 
## sctt3D> # full  panels of box are drawn (bty = "f")
  scatter3D(x, y, z,  col = col,phi=15,theta=10,pch=".",
         bty = "b", cex = 5, colkey = FALSE,box=F,alpha=0.8)

