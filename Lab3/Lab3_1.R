## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(ggplot2)
library(GGally)
library(seriation)
library(colorspace)

# assignment 1
protein <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\protein.txt", sep="\t", header = TRUE)

## ---- echo=FALSE, fig.height=4.5, fig.width=8----------------------------
# 1
#code to add regression curves
regCurve <- function(data, mapping, method="lm"){
  Plot <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, se=FALSE)  
  Plot
}
ggpairs(protein, columns = 2:10, upper = list(continuous = regCurve), lower = list(continuous = "blank"), title = 
          ("Scatterplot matrix for Protein data set"), diag =  list(continuous = "blankDiag"),showStrips = TRUE,axisLabels="internal") 

## ---- echo=FALSE, fig.height=3.5, fig.width=6----------------------------
# 2
row.names(protein) <- protein[,1]
scaleP <- as.matrix(scale(protein[,2:10]))

heatmap(scaleP, Rowv = NA, Colv = NA, scale = "none")

## ---- echo=FALSE, fig.height=3.5, fig.width=6----------------------------
# 3
pl <- heatmap(scaleP, scale = "none")


## ---- echo=FALSE, fig.height=5, fig.width=6------------------------------
reOrd <- scaleP[(pl$rowInd), pl$colInd]
heatmap(reOrd, scale = "none", Rowv = NA, Colv = NA, col=sequential_hcl(5), main="Heatmap with hclust order")
#pimage(reOrd, col=sequential_hcl(5), key=TRUE, axes = "x", main="Heatmap with hclust order")
#protein[rev(pl$rowInd), c(1,pl$colInd +1)]

## ---- echo=FALSE, fig.height=5, fig.width=6------------------------------
# 4
# pca
seri_pca <- seriate(scaleP, method="PCA")
seri_pca2 <- seriate(t(scaleP), method="PCA")
ordPCARow <- get_order(seri_pca)
ordPCACol <- get_order(seri_pca2)
reOrdPCA <- scaleP[rev(ordPCARow), ordPCACol]

heatmap(reOrdPCA, Rowv = NA, Colv = NA ,scale = "none", col=sequential_hcl(5), main="Heatmap with PCA seriate order")

# protein[rev(ordPCA),]

## ---- echo=FALSE, fig.height=5, fig.width=6------------------------------
# anti-robinson
rowdist<-dist(scaleP)
coldist<-dist(t(scaleP))
order1<-seriate(rowdist, "BBURCG")
order2<-seriate(coldist, "BBURCG")
ord1<-get_order(order1)
ord2<-get_order(order2)
reordmatr<-scaleP[(ord1),ord2]

ordBB <- ser_permutation(ord1, ord2)
heatmap(reordmatr, Rowv = NA, Colv = NA ,scale = "none", col=sequential_hcl(5), main="Heatmap with BBURCG seriate order")

#pimage(scaleP,ordBB,col=sequential_hcl(5), key=FALSE, axes = "x", main="Heatmap with BBURCG seriate order")
# protein[rev(ord1),c(1,ord2+1)]

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
# Assignment 2
library(tm)
library(wordcloud)
library(RColorBrewer)
pleased <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\five.txt",header=F, sep='\n')
unpleased <- read.table("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\OneTwo.txt",header=F, sep='\n') #Read file

## ---- echo=FALSE---------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
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

## ----code=readLines(knitr::purl("C:\\Users\\Gustav\\Documents\\Visualization\\Lab3\\Lab3_1.Rmd",documentation = 1)), eval = FALSE, tidy=TRUE----
## NA

