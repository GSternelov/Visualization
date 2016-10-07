# Lab 5

# Assignment 1
library(rpart)
library(partykit)
library(ggplot2)
library(plyr)
library(scales)
library(fields)
library(animation)
library(ggdendro)
library(rggobi)


olive <- read.csv("C:\\Users\\Gustav\\Documents\\Visualization\\Lab5\\olive.csv")
olive$Region <- as.factor(olive$Region)

# 1.2
colnames(olive)
for(i in 4:11){
  cat(colnames(olive)[i], "+")
}
dT <- rpart(Region~palmitic +palmitoleic +stearic +oleic +linoleic +linolenic +arachidic + 
              eicosenoic, data=olive)

plot(as.party(dT))
# Eicosenoic and linoleic selected for making decisions-
# The depth of the tree is 2. 

classific <- predict(dT, olive[,4:11])
classific <- data.frame(pred=c(classific[1:323,1],classific[324:421,2],classific[422:572,3] ))
classific[324:421,] <- 2 ; classific[422:572,] <- 3  
table(classific[,1], olive$Region)
# All but one classified correctly

# 1.3
olive$group <- classific[,1]
olive$group <- as.factor(olive$group)
ggplot(olive, aes(x=eicosenoic, y=linoleic, col=group)) +  geom_rect(aes(xmin=0,xmax=6.5,ymin=0,ymax=1053.5), fill="skyblue", alpha=.1) +
  geom_rect(aes(xmin=0,xmax=6.5,ymin=1053.5,ymax=1500), fill="lightgreen", alpha=.1) +
  geom_rect(aes(xmin=6.5,xmax=60,ymin=0,ymax=1500), fill="red2", alpha=.01) + 
  geom_point()  + theme_classic() + ggtitle("Decision boundaries")

# 1.4
scaleOlive <- scale(olive[,4:11])
compHc <- hclust(dist(scaleOlive), method = "complete")
plot(compHc)
# Two natural clusters, or three


# Assignment 2
library(googleVis)
library(XLConnect)
wb = loadWorkbook("C:\\Users\\Gustav\\Documents\\Visualization\\Lab5\\Oilcoal.xls")
Oilcoal = readWorksheet(wb, sheet = "Sheet2", header = TRUE)

# 2.1
Oilcoal$Year <- as.numeric(Oilcoal$Year)
mCh <- gvisMotionChart(Oilcoal, idvar="Country", timevar="Year")
plot(mCh)

# 2.4
#Yes, it is more informative

# 2.5
Oilcoal$OilP <- Oilcoal$Oil / (Oilcoal$Oil + Oilcoal$Coal)
Oilcoal$Country <- as.factor(Oilcoal$Country)
for(i in 1:8){
  cat("=", levels(Oilcoal$Country)[i],",")  
}
Oilcoal$Country <- seq_along(levels(Oilcoal$Country))[Oilcoal$Country]

splineM <- Tps(x = as.matrix(Oilcoal[,1:2]), Y = Oilcoal$OilP)
summary(rep(1:8, each=135))

# 2.6 - 2.7
k <- 0
yearSeq <- seq(0,0.75,0.25)
extFrame <- data.frame(Country=rep(1:8, each=180), Year=0)
for(i in 1:360){
  for(j in 1:4){
  k <- 1+k
  extFrame[k,2] <- Oilcoal$Year[i] + yearSeq[j]   
  }
}

extFrame$OilP <- predict.Krig(object = splineM, x = as.matrix(extFrame))
extFrame <- rbind(extFrame, data.frame(Country=extFrame[,1], Year= extFrame[,2],
                                       OilP= as.vector(1 - extFrame$OilP)))
extFrame$ConsumptionP <- rep(c("Oil", "Coal"), each=1440)
extFrame$Country <- as.factor(extFrame$Country) 

extFrame <- AllFrame[ order(AllFrame[,4], AllFrame[,2]), ]

ani.options(ffmpeg="C:\\Program Files\\ImageMagick-7.0.3-Q16\\ffmpeg.exe")
saveVideo({
  for(i in 1:180){
    print(ggplot(extFrame[c((1:8)+8*(i-1), 1441:1448+8*(i-1)),], aes(y=OilP, x=Country, 
  fill=ConsumptionP)) +  geom_bar(stat="identity") + theme_classic() +
  scale_y_continuous(labels = percent)+scale_fill_manual(values=c("royalblue","darkorange"))+
    scale_x_discrete(labels=c("1"= "Brazil" ,"2"= "China" ,"3"= "France" ,"4"= "Germany" ,
                              "5"= "India" ,"6"= "Japan" ,"7"= "United Kingdom" ,"8"= "US"))+
    labs(y="Percentage of\nconsumption", title=paste("Year:",AllFrame[(1)+8*(i-1),2])) +
    theme(axis.title.y = element_text(angle=0),axis.text.x = element_text(angle=15))
    )
  }
},video.name="C:\\Users\\Gustav\\Documents\\Visualization\\Lab5\\stackedBars.mp4", interval=0.1,
ani.width=600,ani.height=600
)


