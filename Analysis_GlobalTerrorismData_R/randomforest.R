rm(list=ls())
library(randomForest)
setwd("C:/Users/Sam/Documents/Course_Sem2/Data Mining/R Projects/project")
data <- read.csv("globalterrorismdb_0616dist.csv")

str(data$nkill)
str(data$extended)
as.integer(data$nkill)
#unique(as.integer(data$nkill))
#summary(data)

data[,"y"] <- NA 
data$nkill[is.na(data$nkill)] <- 0
data[,"y"] <- 0
#data$nkill[is.na(data$nkill)] <- round(mean(data$nkill, na.rm = TRUE))
smp_size <- floor(0.80 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

model <- randomForest(y ~ nkill, data = train, ntree=70, #number of trees to grow
                      mtry=5, # m
                      sampsize = round(0.50*nrow(train)), # n
                      replace=FALSE, #sampling without replacement
                      importance = TRUE, # assess importance of predictors
                      do.trace = TRUE #verbose output)
)
                      
pred <- predict(model,newdata = test)



predicted = predict(model,test[,-1])
output = cbind(ID = test$ID, target = predicted)





#Manoj
library(ggplot2)
terror_data <-read.csv("/home/manoj/Desktop/spring_2017/r/Project/globalterrorismdb_0616dist.csv")
HDI <- read.csv("/home/manoj/Desktop/spring_2017/r/Project/HDI_2016.csv")
terror_data$weaptype1_txt <- as.character(terror_data$weaptype1_txt)
terror_data$weaptype1_txt[terror_data$weaptype1_txt=='Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)'] <- 'vehicle'
terror_data$country_txt <- as.character(terror_data$country_txt)
HDI$Country <- as.character(HDI$Country)
terror_2k_2k15 <- subset(terror_data, terror_data$iyear >=2000 & terror_data$iyear <=2015)
terror_1970_99 <- subset(terror_data, terror_data$iyear >=1970 & terror_data$iyear <=1999)
for (i in 1:194){
  x = 0
  for (j in 1:87009)
    if(terror_2k_2k15$country_txt[j] == HDI$Country[i]){
      x = x+1
    }
  HDI$attack_count[i] <- x 
}

for (i in 1:194){
  x = 0
  for (j in 1:69763)
    if(terror_2k_2k15$country_txt[j] == HDI$Country[i]){
      x = x+1
    }
  HDI$attack_count_till99[i] <- x 
}

plain_hijack <- subset(terror_data, terror_data$attacktype1_txt=="Hijacking" & terror_data$targtype1_txt == "Airports & Aircraft")
ggplot(HDI, aes(x = Value, y = attack_count)) + geom_line()
ggplot(terror_2k_2k15,aes(x = weaptype1_txt))+ geom_bar() + coord_flip()
ggplot(terror_1970_80, aes(weaptype1_txt)) + geom_bar() + coord_flip()

#Volume of plain hijacking 
ggplot(plain_hijack, aes(plain_hijack$iyear)) + geom_bar() + coord_flip()
ggplot(plain_hijack, aes(x = iyear)) + geom_density()

#Terror attacks and 9/11
attack9_11 <- subset(terror_data, terror_data$imonth== 9 & terror_data$iday == 11)
unique(terror_data$attacktype1_txt)

India <- subset(terror_data, country_txt=="India")
ggplot(India, aes(India$iyear)) + geom_bar() + coord_flip()
ggplot(India, aes(India$imonth)) + geom_bar() + coord_flip()

# Prediction Model 
# Random Forest
model_data <- terror_data
model_data[is.na(model_data)] <- FALSE
model_data$eventid <- as.character(model_data$eventid)
model_data$eventid <- substr(model_data$eventid, 1, 4)
model_data$success <- as.factor(model_data$success)
ind <- sample.split(Y = model_data$success, SplitRatio = .7)
                    trainDF <- model_data[ind,]
                    testDF <- model_data[!ind,]
                    model_rf <- randomForest(success ~ country+natlty1+region+eventid+targtype1+weapsubtype1+nkill+nwound+attacktype1+weaptype1+multiple+property+guncertain1+doubtterr+crit3+ishostkid+ransom+suicide+crit1, data = model_data, method = "class",ntree=100,ntry = 2)
                    pred_rf <- predict(model_rf,newdata = testDF, type = "class")
                    errs_rf <- 100*(1.0 - (model_rf$confusion[1,1]+model_rf$confusion[2,2])/(sum(model_rf$confusion)))
                    accuracy_rf<- 100*sum(diag(model_rf$confusion))/sum(model_rf$confusion)
                    precision_rf<- 100*(model_rf$confusion[1,1]/(model_rf$confusion[1,1]+model_rf$confusion[2,1]))
                    recall_rf <- 100*(model_rf$confusion[1,1]/(model_rf$confusion[1,1]+model_rf$confusion[1,2]))
                    pred_rf_roc <- predict(model_rf,newdata = testDF, type = "prob")
                    value = NULL
                    value <- rbind(value,data.frame(testDF$success,pred_rf_roc[,2]))
                    auc <- auc(value$testDF.success,value$pred_rf_roc...2.)
                    f_measure <- 2*(precision_rf*recall_rf)/(precision_rf + recall_rf)
                    plot(roc(value$testDF.success,value$pred_rf_roc...2.))
                    