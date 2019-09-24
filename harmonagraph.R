require(ggplot2)
require(gganimate)
f1=2;f2=6;f3=1;f4=3;f5=7;f6=0.1
d1=0.02;d2=0.0315;d3=0.02;d4=0.02
p1=pi/16;p2=3*pi/2;p3=pi/2;p4=3*pi/2

xt = function(t) exp(-d1*t)*sin(t*f1+p1) + exp(-d2*t)*sin(t*f2+p2) 

yt = function(t) exp(-d3*t)*sin(t*f3+p3) + exp(-d4*t)*sin(t*f4+p4) 

t= exp(seq(log(1),log(1e3),length.out = 1e4))
dat=data.frame(t=t, x=xt(t), y=yt(t))

p <- ggplot(dat,aes(x=x,y=y,cumulative = TRUE),size=0.01) +
  geom_path(color="gray20") + 
  transition_reveal(t) 

animate(p, fps = 0.1)

anim_save("harm.gif", animation = p3)


library('tuneR')

sound <- bind(sine(440, bit = 16),
              sine(880, bit = 16),
              sine(440, bit = 16) + sine(880, bit = 16))

writeWave(sound, 'octaves.wav')
