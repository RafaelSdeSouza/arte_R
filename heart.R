
# generate pairs of x-y values
theta = seq(0,  24*pi, length = 500)
x = 16*sin(theta)^3
y = 13*cos(theta)-5*cos(2*theta)-2*cos(3*theta)-cos(4*theta) 

mypal<-c("#009b3a", "#fedf00", "#002776", "white")

# set graphical parameters
op = par(bg = "gray95", mar = rep(0.1, 4))
# plot
plot(x, y, type = "n",xlim = c(-100, 100), ylim = c(-17, 15))
for (i in seq(0, 2*pi, length = 500))
{
  lines(i*x, y, col = mypal[i%%4+1], 
        lwd = 1.25)          
}

# signature
legend("topleft", legend = "© Rafael S. de Souza", bty = "n", text.col = "#009b3a")




library(grid)
library(plyr)
grid.newpage()
No <- 3
wo <- 1/3/2
po <- seq(0, 1, by = wo)[(1:No) * 2]
Nc <- 8
tc <- seq(pi * 11/12, pi * 1/12, len = Nc)
px <- c(outer(wo * cos(tc), po, `+`))
wc <- rep(sin(tc), No)
ag <- rep(1:No, each = Nc)
dc <- 21
th <- seq(0, 2 * pi, len = dc)
grid.rect(gp = gpar(col = NA, fill = "#d95f02"))
for (y0 in seq(0, 1, len = 10)) {
  for (i in seq_along(px)) {
    th <- seq(pi/2, pi/2 + 2 * pi, len = 21)
    if (ag[i]%%2==0) th <- rev(th)
    x <- px[i] + 0.5 * 0.04 * cos(th) * wc[i]
    y <- y0 + 0.04 * sin(th)
    grid.polygon(x, y, gp = gpar(fill = "#b3cde3"))
    grid.polyline(x[1:((dc + 1)/2)], y[1:((dc + 1)/2)], gp = gpar(lineend = "butt", lwd = 3, col = gray(0)))
    grid.polyline(x[-(1:((dc - 1)/2))], y[-(1:((dc - 1)/2))], gp = gpar(lineend = "butt", lwd = 3, col = gray(1)))
  }
}


library(rgl)
#Choose the size of the image on the output (800,650 to have 800 x 600)
r3dDefaults$windowRect <- c(0,50, 800, 650) 
open3d()
#If you want to put line on the background
#bg3d(sphere = TRUE, color = c("grey", "white"), lit = TRUE, back = "lines" ,lwd=2)
bg3d(col=rgb(0.2,0.8,0.5,0.8))
theta <- seq(0, 2*pi, len = 50)
knot <- cylinder3d(
  center = cbind(sin(theta) + 3*sin(7*theta), 2*sin(7*theta), cos(theta) - 2*cos(5*theta)),
  e1 = cbind(cos(theta) + 4*cos(5*theta),6*cos(7*theta),sin(theta) + 4*sin(5*theta)),radius = 0.9,closed = TRUE)
shade3d(addNormals(subdivision3d(knot, depth = 2)), col = rgb(0.4,0.2,0.8,0.3))
#For portfolio 
rgl.snapshot( "#20_portfolio_knot_3D.png", fmt="png", top=TRUE  )
#Pour une image mobile en html (attention, doit etre accompagnée de ses 2 fichiers soeurs...)
writeWebGL( filename="#20_portfolio_knot_3D.html" ,  width=1500)