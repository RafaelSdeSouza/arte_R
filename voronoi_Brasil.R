
require(maps)
require(deldir)
library(mapproj)





mypal<-c("#009b3a", "#fedf00", "#002776", "white")
col=c()
for(i in 1:1000){
  col[i] = mypal[i%%5+1]
}

require(sp)
c<-map("world", "Brazil")
gc<-data.frame(x=c$x,y=c$y)
gc<-na.omit(gc) 

gc.sr = SpatialPolygons(list(Polygons(list(Polygon(gc)), "x")))
dat_r<-spsample(gc.sr, 1000,type="nonaligned")
dat_r<-as.data.frame(dat_r)
colnames(dat_r)<-c("x","y")
all<-rbind(gc,dat_r)

plot(all)


vtess <- deldir(all$x,all$y)
w <- tile.list(vtess)


plot(w, wlines="triang", wpoints="none", number=FALSE, add=TRUE, lty=1,
     fillcol  =col,border=NULL,showpoints = F)

# signature
legend("topleft", legend = "© Rafael S. de Souza", bty = "n", text.col = "gray20",cex=1.25)




require(fields)
data(RCMexample)
set.panel( 1,2)
par(pty="s")
# plot with grid modified
poly.image( c$x, c$y, rnorm(593,1,10))
