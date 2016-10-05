## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(XLConnect)
wb = loadWorkbook("C:\\Users\\Gustav\\Documents\\Visualization\\Lab4\\cars.xlsx")
cars = readWorksheet(wb, sheet = "Blad1", header = TRUE)

head(cars)
colMeans(cars[,3:8])
diag(var(cars[,3:8]))


## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
car.numeric <- scale(cars[,3:8])
distC <- dist(car.numeric)
coordC <- cmdscale(distC, k=2)
coordCf <- data.frame(coordC, country=cars$Country, cylinders=cars$Cylinders)

p1 <- ggplot(coordCf, aes(X1, X2)) + geom_point(col="black") + theme_classic()
p1
#p1_1 <- ggplot(coordCf, aes(X1, X2)) + geom_point(col="black") + theme_classic()+ geom_text_repel(aes(label=country),col="royalblue")
#p1_2 <- ggplot(coordCf, aes(X1, X2))+geom_point(col="black")+theme_classic()+geom_text_repel(aes(label=cylinders),col="royalblue")
#grid.arrange(p1, p1_1, p1_2, ncol=3)

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
plot(distC,dist(coordC), pch=21, bg="black")

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=4, fig.width=7----
library(MASS)
coord4 <- isoMDS(distC, k=2, p=2)
coords4 <- data.frame(coord4$points, country=cars$Country, cylinders=cars$Cylinders)

p2 <- ggplot(coords4, aes(X1, X2)) + geom_point(col="black") + theme_classic()
p2
#p2_1 <- ggplot(coords4, aes(X1, #X2))+geom_point(col="black")+theme_classic()+geom_text_repel(aes(label#=country),col="royalblue")
#p2_2 <- ggplot(coords4, aes(X1, #X2))+geom_point(col="black")+theme_classic()+geom_text_repel(aes(label#=cylinders),col="royalblue")
#grid.arrange(p2, p2_1, p2_2, ncol=3)

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
sh <- Shepard(distC, coord4$points)
plot(distC,dist(coord4$points), pch=21, bg="black")
lines(sh$x, sh$yf, type = "S", col="red")

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
ggplot(coords4, aes(X1, X2,col=country))+geom_point(size=2)+theme_classic()+geom_text_repel(aes(label=country),col="royalblue")

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
coord6 <- isoMDS(distC, k=2, p=1)
coords6 <- data.frame(coord6$points, country=cars$Country, cylinders=cars$Cylinders)

ggplot(coords6, aes(X1, X2,col=country))+geom_point(size=2)+theme_classic()+geom_text_repel(aes(label=country),col="royalblue")

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
sh <- Shepard(distC, coord6$points)
plot(distC,dist(coord6$points), pch=21, bg="black")
lines(sh$x, sh$yf, type = "S", col="red")

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
set.seed(1897)
initVal <- matrix(runif(76, min=-1,max= 1),ncol=2)
coord7 <- isoMDS(distC, k=2, p=2, maxit = 500, y = initVal)
coords7 <- data.frame(coord7$points, country=cars$Country, cylinders=cars$Cylinders)

ggplot(coords7, aes(X1, X2,col=country))+geom_point(size=2)+theme_classic()+geom_text_repel(aes(label=country),col="royalblue")

## ---- echo=FALSE, fig.height=4, fig.width=7------------------------------
sh <- Shepard(distC, coord7$points)
plot(distC,dist(coord7$points), pch=21, bg="black")
lines(sh$x, sh$yf, type = "S", col="red")

## ----code=readLines(knitr::purl("C:\\Users\\Gustav\\Documents\\Visualization\\Lab4\\Lab4.Rmd",documentation = 1)), eval = FALSE, tidy=TRUE----
## NA

