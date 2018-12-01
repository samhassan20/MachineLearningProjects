library(ggplot2)
setwd("C:/Users/Sam/Documents/Course_Sem2/Data Mining/R Projects/project")
data <- read.csv("globalterrorismdb_0616dist.csv")
HDI <- read.csv("HDI_2016.csv")
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

