
# Assignment 1
library(MASS)
df1=aggregate(Price~Type, data=Cars93, FUN=mean)
barplot(df1$Price, names.arg=df1$Type)


# Assignment 2
senic <- read.csv(file="C:\\Users\\Gustav\\Documents\\Visualization\\Senic.csv", 
                  sep=";")
colNames <- c("ID", "Length of Stay", "Age", "Infection risk",
          "Routine Culturing Ratio", "Routine Chest X-ray Ratio", "Number of Beds",
          "Medical School Affiliation", "Region","Average Daily Census",
          "Number of Nurses", "Available Facilities & Services")

library(ggplot2)
library(gridExtra)
library(ggrepel)
p1 <- ggplot(senic, aes(x=X7)) + geom_bar(fill="royalblue", col="gold") + theme_light() + 
  xlab("Medical School Affiliation")+ scale_x_continuous(breaks=c(1,2), labels=c("Yes", "No"))+
  ggtitle("Number of medical school affiliated hospitals") +
  theme(axis.text=element_text(size=12), axis.title=element_text(face="bold"),
        plot.title = element_text(size=16, face="bold"))
senic$X8 <- factor(senic$X8, levels = c("3", "2", "1", "4"))
p2 <- ggplot(senic, aes(x=X8, fill=X8)) + geom_bar(fill=c("blue","royalblue","skyblue","lightgrey")) + theme_light() + 
  xlab("Region")+ scale_x_discrete(labels=c("S", "NC","NE","W"))+
  ggtitle("Geographic region for the hospitals") +
  theme(axis.text=element_text(size=12), axis.title=element_text(face="bold"),
        plot.title = element_text(size=16, face="bold"))
pL1 <- list(p1, p2)
pLot1 <- arrangeGrob(grobs=pL1, nrow=2)
plot(pLot1)

quantVar <- c(2:7, 10:12)
k <- 0
pL2 <- list()
for(i in quantVar){
  k <- k+1
  pL2[[k]] <-  ggplot(senic, aes_string(y=colnames(senic)[i], x=1)) +
    xlab("")+ ylab("") +  geom_boxplot(fill="royalblue") +
    theme_light() + scale_x_continuous(breaks=c(1), labels=c("")) +
    ggtitle(colNames[i])
    if(nrow(subset(senic, senic[,i] < quantile(senic[,i], 0.25) - 1.5 * IQR(senic[,i]) |
                   senic[,i] > quantile(senic[,i], 0.75) + 1.5 * IQR(senic[,i]))) > 0){
      pL2[[k]] <- pL2[[k]] +  geom_text_repel(data=subset(senic, senic[,i] < quantile(senic[,i], 0.25) - 1.5 * IQR(senic[,i]) |
                    senic[,i] > quantile(senic[,i], 0.75) + 1.5 * IQR(senic[,i])),
        aes(label=Obs))
    } else{
      pL2[[k]] <-  pL2[[k]] 
    }
}
pLot2 <- arrangeGrob(grobs=pL2, nrow=3)
plot(pLot2)

# Assignment 3
ggplot(senic,aes(y=senic$X10/senic$X6, x=Obs)) + geom_point() + 
  geom_text(aes(label=senic$Obs),hjust = 0, nudge_x = 0.05, check_overlap=TRUE) +
  theme_light()

ggplot(senic, aes(x=X3, y=Obs)) + geom_point() + geom_point(data=senic[107,], 
    aes(x=X3, y=Obs), size=5, col="royalblue") + theme_light() +
  geom_text(data=senic[107,],aes(x=X3, y=Obs, label=Obs),hjust = 0, nudge_x = 0.05)

library(fANCOVA)
mod=loess.as(senic$Obs, senic$X3, criterion="gcv", degree=2)
result=predict(mod, se=TRUE)

LimFits <- data.frame(Obs= senic$Obs, fits=result$fit, Lower=result$fit - 2*result$se.fit, Upper=
             result$fit + 2*result$se.fit)

ggplot(LimFits, aes(x=Obs, y=fits))  + geom_line() +
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.25, fill="royalblue") + theme_light() +
  geom_point(data=senic, aes(y=X3, x=Obs))


