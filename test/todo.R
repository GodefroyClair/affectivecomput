#TODO 
#
my_scale <- function(x) c(scale(x))
set.seed(7)
df <- data.frame(x=10*runif(9), y=20*rnorm(9), group=rep(c("a","b","c"),3))
df2 <- df %>% group_by(group) %>% mutate_each(funs(scale))
df3 <- df %>% group_by(group) %>% mutate_each(funs(my_scale))
dim(df2)
dim(df3)
View(df2  )
df2[[1]]
df3[[1]]

#Moi
##problème des graphes pour la représentation des hexagones
##trouver des intermédiaires entre scale & scale_scale : tester center_scale...

#Anastase:
##PCA
##10x10
##radius


#Cotrell:
##100x1
##10x10
##graphique en 3D
##variables exogènes
##faire un mapping sur 0-1
##vérifier que les variables ont bien été normalisées et si oui prq certaines var sont toutes grandes sur les graphiques...