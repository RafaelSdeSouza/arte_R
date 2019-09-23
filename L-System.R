library(gsubfn)
library(stringr)
library(dplyr)
library(ggplot2)
source("Lfunc.R")

Brasil <- c("#16B83E","#FFE11F","#1651B8","#FFFFFF")
cc <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
colpal <- c(cc,rev(cc)) %>%  colorRampPalette()



# System 1 
axiom = "-L"
rules = list("L"="LF+RFR+FL-F-LFLFL-FRFR+", 
           "R" = "-LFLF+RFRFR+F+RF-LFL-FR")
angle = 90
depth = 4


axiom="X"
rules=list("X"="F-[[X]+X]+F[+FX]-F + [[FX+X]]", "F"="FF")
angle=20
depth=5


axiom="FF-F-F-F-FF"
rules=list("F"="FF-F-F-F-FF")
angle=90
depth=4


# Gosper curve
axiom="F+F+F+F+F+F"
rules=list("F" = "R-F-R",
           "R" = "F+R+F")
angle=60
depth=5


# Gosper curve
axiom="F"
rules=list("F" = "R-F-R",
           "R" = "F+R+F")
angle=60
depth=7


# System 2 
#axiom = "LF"
#rules = list("L"="LF+RF++RF-LF--LFLF-RF+", 
#             "R" = "-LF + RFRF++RF+RL--LF-RF")
#angle = 120
#depth = 4


# Kock
#axiom = "F"
#rules = "F"="F+FF-FF-F-F+F+F"



Ldata <- Lfunc(axiom,rules,angle,depth)

#pdf("art.pdf",width = 12,height = 6)
ggplot() + 
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2,alpha=0.1), 
               colour="white",
               data=na.omit(Ldata)) + 
  
  coord_fixed(ratio = 1) +
  theme(legend.position="none",
        panel.background = element_rect(fill="black"),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank()) 
#+
#  annotate("text", x = -125, y = 170, label = "Rafael S. de Souza",color="white")

#dev.off()

#

# Julia Set
imageN <- 500;
centre <- -1.0
L <- 2.0
file <- "julia1a.png"
C <- 1-1.6180339887# Golden Ration
image <- JuliaImage(imageN,centre,L,C)

png("julia.png",height = 2000,width = 2000)
levelplot(t(image),par.settings=RdBuTheme(),contour = TRUE,color="white")
dev.off()

# Mandelbrot Set
imageN <- 5;
centre <- 0.0
L <- 4.0
file <- "mandelbrot1.png"
pdf(file,width = 1200,height = 1200)
MandelImage(imageN,centre,L)
dev.off()


