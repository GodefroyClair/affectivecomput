library(reshape)
library(rggobi)
library(clusterfly)
library(kohonen)
library(som)
d.music <- read.csv("http://www.ggobi.org/book/data/music-all.csv")

music <- rescaler(d.music)[complete.cases(d.music), 1:10]
music.som <- som::som(music[,-(1:3)], 6, 6, neigh="bubble", rlen=1000)
ggobi(music.som)
## End(Not run)
## Not run:
d.music <- read.csv("http://www.ggobi.org/book/data/music-all.csv")
music <- rescaler(d.music)[complete.cases(d.music), 1:10]
music.hex <- kohonen::som(as.matrix(music[,-(1:3)]), grid = somgrid(3, 3, "hexagonal"), rlen=1000)
music.rect <- kohonen::som(as.matrix(music[,-(1:3)]), grid = somgrid(6, 6, "rectangular"), rlen=1000)
ggobi(music.rect)


