library('dplyr')
library('readr')
require('graphics')
library('SOMbrero')
library('yasomi')
library('class')
library('popsom')
#library(devtools)
#install_github("fabrice-rossi/yasomi")
#install.packages("yasomi", repos="http://R-Forge.R-project.org")


#import
pca_data <-read_csv('./data/pca_data.csv')
pca <- pca_data %>% select(-nom.experience)

# work with class package
gr <- somgrid(topo = "hexagonal", xdim = 20, ydim = 20)
pca_som  <- class::SOM(pca, grid = gr, rlen = 10000,
           alpha = list(seq(0.05, 0, len = 1e4), seq(0.02, 0, len = 1e5)),
           radii = list(seq(8, 1, len = 1e4), seq(4, 1, len = 1e5)))
plot(pca_som)
dim(pca_som$codes)
plot(gr, type = "p")


# SOMbrero
gr2 <- initGrid(dimension=c(10,10), topo="square", dist.type=c("euclidean"))
pca_som <- trainSOM(x.data=pca)
print(gr2)
summary(gr2)
