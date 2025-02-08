# seriate a grid
library(seriation)
# comparing the order of hierarchical clustering and hc with reordering leafes
x <- ga
r <- getRatingLayer(x)
d <- dist(t(r))
den <- as.dendrogram(hclust(d))

method <- "OLO"
s <- seriate(d, method)
den2 <- as.dendrogram(s[[1]])
layout(matrix(1:2, nrow=2))
plot(den)
plot(den2)

m <- as.matrix(d)
colnames(m) <- 1:ncol(m)
rownames(m) <- paste(1:nrow(m), rownames(m))
round(m, 2)
####
x <- ga
method <- "OLO"
xd <- doubleEntry(x)
r <- getRatingLayer(xd)
d.c <- dist(r)
s.c <- seriate(d.c, method)
o.c <- get_order(s.c)

r <- getRatingLayer(xd)
d.e <- dist(t(r))
s.e <- seriate(d.e, method)
o.e <- get_order(s.e)

bertin(xd[o.c, o.e], colors = c("red", "green"))
