### Lab 2 ###
#Assignment 1

# 1)
jobs <- read.table(file = "C:\\Users\\Gustav\\Documents\\Visualization\\jobs.txt", header = TRUE, sep = "\t")

jobs$group <- 0
jobs[c(1,3,4,5,7,8,9,10,17) ,11] <- "W"
jobs[c(2,11,13,16), 11] <- "Sc"
jobs[c(6,12,14,15,18), 11] <- "S"
jobs[c(19,20,21,22,23,24,25,26), 11] <- "E"

# 2)
library(portfolio)
map.market(id=jobs$Country, area=jobs$SI, group=jobs$group, color=jobs$Fin, main="Jobs in service(area) and finance(color)",lab=c(F,T) )
# The results seem to be very reasonable. Finance and service much more common jobs in western Europe and in scandinavia.
# Least common in the eastern Europe, which was to be expected. The exception is yugoslavia where finance jobs are much more common.

# 3)
library(aplpack)
jobs[,12:17] <- 1
faces(as.matrix(jobs[,c(2:10, 12:17)]), labels=jobs$Country)
# Can be used for analysing which countries are similar and if there are any outliers. 
# The eastern countreis are fairly similar. Yugoslavia and Turkey looks like outliers
# In the other groups of countries are the countries pretty much the same as the others in their group. The exceptions are
# Luxembourg who looks like an outlier and Netherlands who appear to be more similar the Scandinavian countries than the
# Other Western countries. 

# 4)
library(gclus)
jobs <- jobs[,1:11]
d<-dist(t(scale(jobs[,2:10])))
jobs_order <- order.single(d)
jobs2 <- jobs[, jobs_order +1 ]

library(graphics)
stars(jobs2, labels=as.character(jobs$Country), draw.segments = TRUE, key.labels = names(jobs2), key.loc = c(14,1.85))

# 5)
parallelplot(jobs[,2:10], horizontal.axis=FALSE)
# Would like to say that it is hard to make any analysis what so ever from this graph. Some clusters can maybe be seen
# One outlier is pretty evident. Hard to say anything about correlations. Perhaps between PS and Con, but not easy to see. 

# 6)
## a)
job_cor <- 1 - cor(jobs[,2:10], )
## b)
library(TSP)
res<-solve_TSP(TSP(job_cor))
jobs2<-jobs[,as.integer(res)]
## c)
parallelplot(jobs2, horizontal.axis=FALSE)

# 7)
library(seriation)
res2 <- seriate(as.dist(job_cor), method="HC")
jobs_order2 <- get_order(res2)
jobs3 <- jobs[, jobs_order2+1]

parallelplot(jobs3, horizontal.axis=FALSE)
jobs3$Fin
color=1+(jobs3$Fin-min(jobs3$Fin)>0.3*(max(jobs3$Fin)-min(jobs3$Fin)))
color[18] <- 3
color[26] <- 4
parallelplot(jobs3, horizontal.axis=FALSE, col=color)



### Assignment 2 ###
library(sp)
houseP <- read.csv("C:\\Users\\Gustav\\Documents\\Visualization\\prices_0510.csv", sep = ";")

map1 <- readRDS("C:\\Users\\Gustav\\Downloads\\SWE_adm1.rds")

temp<-map1@data
new<-merge(temp, houseP, by.x="NAME_1",
           by.y="County", all.y=F, all.x=T, sort=FALSE)

map1@data$Price2005 <-  new$X2005
map1@data$Price2010 <-  new$X2010

spplot(map1, zcol="Price2005", main="Average price 2005", col.regions = heat.colors(16))
spplot(map1, zcol="Price2010", main="Average price 2010", col.regions = heat.colors(16))





