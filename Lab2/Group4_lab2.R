## ---- echo=FALSE---------------------------------------------------------
# 1)
jobs <- read.table(file = "C:\\Users\\Gustav\\Documents\\Visualization\\Lab2\\jobs.txt", header = TRUE, sep = "\t")
jobs$group <- 0
jobs[c(1,3,4,5,7,8,9,10,17) ,11] <- "W"
jobs[c(2,11,13,16), 11] <- "Sc"
jobs[c(6,12,14,15,18), 11] <- "S"
jobs[c(19,20,21,22,23,24,25,26), 11] <- "E"
head(jobs)

## ---- echo=FALSE, fig.height=3.75, fig.width=6, warning=FALSE, message=FALSE, fig.align='center'----
# 2)
library(portfolio)
map.market(id=jobs$Country, area=jobs$SI, group=jobs$group, color=jobs$Fin, main="Jobs in service(area) and finance(color)",lab=c(T,T) )


## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
# 3)
library(aplpack)
jobs[,12:17] <- 1
par(mar=c(1.1,4.1,4.1,2.1))
faces(as.matrix(jobs[,c(2:10, 12:17)]), labels=jobs$Country, print.info = TRUE)

## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(gclus)
library(graphics)
jobs <- jobs[,1:11]
d<-dist(t(scale(jobs[,2:10])))
jobs_order <- order.single(d)
jobs2 <- jobs[, jobs_order +1 ]
head(jobs2, 1)

## ---- echo=FALSE---------------------------------------------------------
palette(heat.colors(9))
stars(jobs2, labels=as.character(jobs$Country), draw.segments = TRUE, key.labels = names(jobs2), key.loc = c(14,1.85))


## ---- echo=FALSE, fig.height=3.5, fig.width=7, fig.align='center'--------
# 5)
parallelplot(jobs[,2:10], horizontal.axis=FALSE)

## ---- echo=FALSE, fig.height=3.5, fig.width=7, fig.align='center', warning=FALSE----
# 6)
set.seed(123445)
## a)
job_cor <- 1 - cor(jobs[,2:10], )
## b)
library(TSP)
res<-solve_TSP(TSP(job_cor))
jobs2<-jobs[,as.integer(res)+1]
## c)
parallelplot(jobs2, horizontal.axis=FALSE)

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.height=3.5, fig.width=7, fig.align='center'----
library(seriation)
res2 <- seriate(as.dist(job_cor), method="HC")
jobs_order2 <- get_order(res2)
jobs3 <- jobs[, jobs_order2+1]

color=1+(jobs3$Fin-min(jobs3$Fin)>0.3*(max(jobs3$Fin)-min(jobs3$Fin)))
color[18] <- 3
color[26] <- 4
color[color==1] <- "royalblue"
color[color==2] <- "darkorange"
color[color==3] <- "red"
color[color==4] <- "black"
parallelplot(jobs3, horizontal.axis=FALSE, col=color)

## ---- echo=FALSE---------------------------------------------------------
group=1+(jobs3$Fin-min(jobs3$Fin)>0.3*(max(jobs3$Fin)-min(jobs3$Fin)))
group[18] <- 3
group[26] <- 4
jobs$group2 <- group
subset(jobs, jobs$group2 == 1)[,c(1,11)]

## ---- echo=FALSE---------------------------------------------------------
subset(jobs, jobs$group2 == 2)[,c(1,11)]

## ---- echo=FALSE---------------------------------------------------------
subset(jobs, jobs$group2 == 3 | jobs$group2 == 4)[,c(1,11)]

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
houseP <- read.csv("C:\\Users\\Gustav\\Documents\\Visualization\\Lab2\\prices_0510.csv", sep = ";")

map1 <- readRDS("C:\\Users\\Gustav\\Downloads\\SWE_adm1.rds")

temp<-map1@data
new<-merge(temp, houseP, by.x="NAME_1",
           by.y="County", all.y=F, all.x=T, sort=FALSE)

map1@data$Price2005 <-  new$X2005
map1@data$Price2010 <-  new$X2010

spplot(map1, zcol=c("Price2005", "Price2010"), main="Average purchase price 2005 and 2010", col.regions = heat.colors(16))


## ----code=readLines(knitr::purl("C:\\Users\\Gustav\\Documents\\Visualization\\Lab2\\Group4_lab2.Rmd",documentation = 1)), eval = FALSE, tidy=TRUE----
## 

