
# lab 3
library(ggplot2)
library(GGally)
library(seriation)
library(colorspace)

# assignment 1
protein <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\protein.txt", sep="\t", header = TRUE)

# 1

#code to add regression curves
regCurve <- function(data, mapping, method="lm"){
  Plot <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, se=FALSE) + theme_minimal() 
  Plot
}

ggpairs(protein, columns = 2:10, upper = list(continuous = regCurve), lower = list(continuous = regCurve), title = 
          ("Scatterplot matrix for Protein data set"), diag =  list(continuous = "blankDiag"),showStrips = TRUE) 

# 2
scaleP <- as.matrix(scale(protein[,2:10]))
heatmap(scaleP, Rowv = NA, Colv = NA,scale = "none")

# 3
HC <- hclust(dist(protein[,2:10]))
ordHC <- HC$order
pl <- heatmap(scaleP, scale = "none")
reOrd <- scaleP[rev(pl$rowInd), pl$colInd]
pimage(reOrd, col=sequential_hcl(14), key=FALSE, axes = "x", main="Heatmap with hclust order")

protein[rev(pl$rowInd), c(1,pl$colInd +1)]

# 4
# pca
seri_pca <- seriate(scaleP, method="PCA")
ordPCA <- get_order(seri_pca)
reOrdPCA <- scaleP[rev(ordPCA), ]
pimage(reOrdPCA, col=sequential_hcl(14), key=FALSE, axes = "x", main="Heatmap with PCA seriate order")

# anti-robinson
rowdist<-dist(scaleP)
coldist<-dist(t(scaleP))
order1<-seriate(rowdist, "BBURCG")
order2<-seriate(coldist, "BBURCG")
ord1<-get_order(order1)
ord2<-get_order(order2)
reordmatr<-scaleP[rev(ord1),ord2]
heatmap(reordmatr, Colv=NA, Rowv=NA,
        col=heat.colors(20))
ordBB <- ser_permutation(ord1, ord2)
pimage(scaleP,order,col=sequential_hcl(14), key=FALSE, axes = "x", main="Heatmap with BBURCG seriate order")



# Assignment 2
library(tm)
library(wordcloud)
library(RColorBrewer)

pleased <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\five.txt",header=F, sep='\n')

mycorpus <- Corpus(DataframeSource(pleased)) #Creating corpus (collection of text data)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
# Remove some more words
mycorpus <- tm_map(mycorpus,function(x) removeWords(x,c("watch","one","watches")))
tdm <- TermDocumentMatrix(mycorpus) #Creating term-document matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE) #Sum up the frequencies of each word
d <- data.frame(word = names(v),freq=v) #Create one column=names,second=frequences
pal <- brewer.pal(4,"Dark2")
pal <- pal[-(1:2)] #Create palette of colors
wordcloud(d$word,d$freq, scale=c(4,.3),min.freq=2,max.words=100, random.order=F,
          rot.per=.15, colors=pal, vfont=c("sans serif","plain"))


unpleased <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\OneTwo.txt",header=F, sep='\n') #Read file

mycorpus <- Corpus(DataframeSource(unpleased)) #Creating corpus (collection of text data)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
# Remove some more words
mycorpus <- tm_map(mycorpus,function(x) removeWords(x,c("watch","one","watches")))
tdm <- TermDocumentMatrix(mycorpus) #Creating term-document matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE) #Sum up the frequencies of each word
d <- data.frame(word = names(v),freq=v) #Create one column=names,second=frequences
pal <- brewer.pal(4,"Dark2")
pal <- pal[-(1:2)] #Create palette of colors
wordcloud(d$word,d$freq, scale=c(4,.3),min.freq=2,max.words=100, random.order=F,
          rot.per=.15, colors=pal, vfont=c("sans serif","plain"))


