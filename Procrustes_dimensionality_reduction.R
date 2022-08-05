### Procrustes

library(vegan)

pca = read.table('data/pca.tsv', hea=T, sep=",")
mds = read.table('data/mds.tsv', hea=T, sep=",")
tsne = read.table('data/tsne.tsv', hea=T, sep=",")
umap = read.table('data/umap.tsv', hea=T, sep=",")

pca = pca[,c('X0', 'X1')]
names(pca) = c('DIM1', 'DIM2')
mds = mds[,c('DIM1', 'DIM2')]
tsne = tsne[,c('DIM1', 'DIM2')]
umap = umap[,c('DIM1', 'DIM2')]

myproc<-procrustes(pca, mds)
test1<-protest(pca, mds)
myproc<-procrustes(pca, tsne)
test2<-protest(pca, tsne)
myproc<-procrustes(pca, umap)
test3<-protest(pca, umap)
myproc<-procrustes(mds, tsne)
test4<-protest(mds, tsne)
myproc<-procrustes(mds, umap)
test5<-protest(mds, umap)
myproc<-procrustes(umap, tsne)
test6<-protest(umap, tsne)

mean(test1$t0, test2$t0, test3$t0, test4$t0, test5$t0, test6$t0)

