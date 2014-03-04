morfoclara <-
function(input,cluster,ssize,nsamples,output)
{
library(spgrass6)
x <- readRAST6(c (input))
grd <- x@grid
proj <- x@proj4string
xx <- x@data
rm(x)
NArow <- apply(xx, 1, function(x) !(all(is.na(x))))
xx1 <- xx[NArow,]
library(cluster)
res <- clara(xx1, k=cluster, metric='euclidean', stand=FALSE, sampsize=ssize, samples=nsamples, rngR=TRUE)
rm(xx1)
out_class <- as.integer(rep(NA, length(NArow))) 
out_class[NArow] <- res$clustering
out <- SpatialGridDataFrame(grd, proj4string=proj, data=data.frame(cls=out_class))
writeGDAL(out, fname=output, drivername="GTiff", type="Byte")
}
