# Lab 5

# Assignment 1
library(rpart)
library(partykit)
library(rggobi)
library(classifly)

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
classific <- data.frame(pred=c(classific[1:323,1],classific[324:421,2],classific[423:572,3] ))
classific[324:421,] <- 2 ; classific[423:572,] <- 3  
table(classific[,1], olive$Region)
# All but one classified correctly

# 1.3
plot(olive$linoleic, olive$eicosenoic)
classifly(data = olive, Region~palmitic +palmitoleic +stearic +oleic +linoleic +linolenic +
            arachidic + eicosenoic, classifier = rpart)

