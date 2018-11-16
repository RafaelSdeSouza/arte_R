library(imager)
library(dplyr)
library(deldir)
library(ggplot2)
library(scales)

# Download the image

# Read and convert to grayscale
load.image("FM.jpg") %>% grayscale() -> x

# This is just to define frame limits
x %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()->rw

# Filter image to convert it to bw
x %>%
  threshold("1%") %>% 
  as.cimg() %>% 
  as.data.frame() -> df

short <-  sample_n(df,size=5000, weight=(1-df$value))[,1:2] 
#short$y <- -short$y +max(short$y)
plot(short,cex=0.4)

#write.csv(short,"SuM.csv",row.names = F)

#d <- read.csv("SuM.csv",header = T)
#plot(d,col="blue")





cols = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
         "#fb9a99","#e31a1c","#fdbf6f","#ff7f00",
         "#cab2d6","#6a3d9a","#ffff99")[sample(1:12,nrow(short),replace = T)]
sizes <- runif(nrow(short),0.5,6)
shapes <- sample(1:12,nrow(short),replace = T)
  
ggplot(data=short) +
  geom_point(aes(x = x, y = y), color="purple2",size=1.5,shape=4, alpha=0.7) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
  theme(legend.position  = "none",
        panel.background = element_rect(fill="black"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())




vtess <- deldir(short$x,short$y,sort=TRUE)
data<-vtess$dirsgs

scol<-colorRampPalette(rainbow(6))(nrow(data)) 

ggplot(data=data) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2,color=scol),lwd=0.25) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
  theme(legend.position  = "none",
        panel.background = element_rect(fill="gray5"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

# signature
legend("topleft", legend = "© Rafael S. de Souza", bty = "n", text.col = "whie",cex=1.25)


require(spatstat)
A <- tess(xgrid=0:4,ygrid=0:4)
A
B <- A[c(1, 2, 5, 7, 9)]
B
v <- as.im(function(x,y){factor(round(5 * (x^2 + y^2)))}, W=owin())
levels(v) <- letters[seq(length(levels(v)))]
E <- tess(image=v)
E