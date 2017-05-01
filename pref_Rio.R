library(GGally)
library(ggnet)
library(network)
library(sna)
library(ggplot2)
require(made4)
require(circlize)
M<-matrix(data=c(1,0.5,0,1,0,0,0.5,0,
                 1,0,0,1,0,0,1,1,
                 0.5,1,1,1,1,1,1,1,
                 0.5,1,1,0.5,1,1,1,1,
                 1,1,0,1,0,0,0,1,
                 0.5,0,0,1,0,0,0,0,
                 1,1,1,1,1,0.5,1,1,
                 0.5,0,0,0,1,0,0,0,
                 0.5,1,1,1,1,1,0,1,
                 1,1,1,1,1,1,1,0),ncol=8,nrow=10,byrow = T)
colnames(M)<-c("Bolsonaro","Crivella","Freixo","Índio","Jandira","Molon","Osorio",
               "Pedro Paulo")
rownames(M)<-c("Escola sem partido","Guarda armada","Mototáxis","Transporte alternativo",
               "OSs na saúde","Remoção de favelas","Apoio ao carnaval","Subsídio ao ônibus",
               "Aporte ao metrô","Uber")

M2<-t(M)

png(filename='test.png', width=800, height=600)
heatplot(M2, scale="none",dend="row",dualScale=F,zlim
=c(0,1),margins=c(12,8))

pdf("chord_rio.pdf")
par(mar=c(0,2,2,4)+0.1) 
chordDiagram(M2,link.border = 1,directional = 1,
          annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
dev.off()

quartz.save(type = 'pdf', file = 'Rio_pref.pdf',width = 12, height = 10)

