# Read Rafael Sequence
require(forcats)
require(ggplot2)
require(dplyr)
require(MASS)
require(bossMaps)
x <- seq(0,10000L)
y <- read.table("yseries.txt",header = F)

rafseq <- data.frame(x,y) %>% setNames(c("x","y"))
write.matrix(rafseq,"b327742.txt")


png("A327742.png",height = 500,width = 1.618*500)
ggplot(rafseq,aes(x=x,y=y/72)) +
geom_line(size=0.1,alpha=0.5) +
  ylab("A327742(n)") + xlab("n") +
  theme_bw() + ggtitle("A327742(n)") +
  theme(
        axis.title = element_text(color="black", size=17.5),
        axis.text  = element_text(size=16),
        strip.text = element_text(size=10),
        strip.background = element_rect("gray85"))
dev.off()

ggplot(rafseq,aes(x=x,y=y)) +
  geom_point(size=0.1,alpha=0.5) +
  ylab("A327742(n)") + xlab("n")

raf <- function(x){x^2*(2 + x)^2*(3 + x)/((1 + x^2)*(1 + x^3)*(1 + x^4)*(1 + x^5)*(1 + x^6))}