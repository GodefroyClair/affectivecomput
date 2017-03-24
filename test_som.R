library(som)
som.init(data, xdim, ydim, init="linear")
som(data, xdim, ydim, init="linear", alpha=NULL, alphaType="inverse",
    neigh="gaussian", topol="rect", radius=NULL, rlen=NULL, err.radius=1,
    inv.alp.c=NULL)
som.train(data, code, xdim, ydim, alpha=NULL, alphaType="inverse",
          neigh="gaussian", topol="rect", radius=NULL, rlen=NULL, err.radius=1, inv.alp.c=NULL)
som.update(obj, alpha = NULL, radius = NULL, rlen = NULL, err.radius =
             1, inv.alp.c = NULL)
som.project(obj, newdat)

data(yeast)
yeast <- yeast[, -c(1, 11)]
yeast.f <- filtering(yeast)
yeast.f.n <- normalize(yeast.f)
foo <- som(yeast.f.n, xdim=5, ydim=6)
foo <- som(yeast.f.n, xdim=5, ydim=6, topol="hexa", neigh="gaussian")
plot(foo)
