setwd("C:/r/SDM/Final_Project")
rm(list=ls())

#########################################################################################################
#########################################################################################################
#########################################################################################################
###############################################DATA LOADING##############################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################



#Combining the Datasets from the different regions

data_northeast <- read.csv("Output_File_New_NorthEast.csv")
data_southeast <- read.csv("Output_File_New_SouthEast.csv")
data_greatLakes <- read.csv("Output_File_New_Great_LAkes.csv")
data_pacific    <- read.csv("Output_File_Pacific.csv")
data_gulfCoast <- read.csv("Output_File_New_Gulfcoast.csv")
colnames(data_northeast)=tolower(make.names(colnames(data_northeast)))
colnames(data_southeast)=tolower(make.names(colnames(data_southeast)))
colnames(data_greatLakes)=tolower(make.names(colnames(data_greatLakes)))
colnames(data_pacific)=tolower(make.names(colnames(data_pacific)))
colnames(data_gulfCoast)=tolower(make.names(colnames(data_gulfCoast)))
attach(data_northeast)
attach(data_southeast)
attach(data_greatLakes)
attach(data_pacific)
attach(data_gulfCoast)



data_northeast$region ='NorthEast'
data_southeast$region ='SorthEast'
data_greatLakes$region ='Great Lakes'
data_pacific$region ='Pacific'
data_gulfCoast$region ='Gulf Coast'






completed_data <- rbind(data_northeast,data_southeast,data_greatLakes,data_pacific,data_gulfCoast)

#########################################################################################################
#########################################################################################################
#########################################################################################################
##############################################DATA CLEANING##############################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################



#Filtering listings with no price
data_No_Price<-subset(completed_data, price=='Request a Price')
completed_data<-subset(completed_data, price!='Request a Price')
#Cleaning the lenth column for "'" values
completed_data$length<-gsub("'", "", completed_data$length)
#Cleaning the price column for "$" values
completed_data$price<-gsub("\\$", "", completed_data$price)
#Cleaning the price column for "," values
completed_data$price<-gsub(",", "", completed_data$price)

#Setting Price Column as numeric data
completed_data$price<-as.numeric(completed_data$price)
#Setting Length Column as numeric data
completed_data$length<-as.numeric(completed_data$length)

#Deleting duplicate data based on posting link
completed_data<-completed_data[!duplicated(completed_data$posting_link), ]

#Calculating Boat Age
completed_data$age<-abs(as.numeric(completed_data$year)-as.numeric(format(Sys.Date(),"%Y")))

#########################################################################################################
#########################################################################################################
#########################################################################################################
###########################################Data Visualization############################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


#File to be used to export to Tableau Data Visualization Project

write.csv(completed_data,"C:/r/SDM/Final_Project/Dataframe.csv", row.names = TRUE)

hist(completed_data$length,main="Histogram for Length",xlab = "length")
hist(completed_data$age,main="Histogram for Age",xlab = "age")
hist(completed_data$year,main="Histogram for Year",xlab = "year")
plot(completed_data$length,completed_data$price,ylab="price",xlab="length",main="Price Vs Length")
plot(completed_data$age,completed_data$price,ylab="price",xlab="age",main="Price Vs Age")
summary(completed_data$length)

#Factor Of: Class, Category, Make, Material, Fuel, Zipcode

#Numerical: Age, lenght, year

#########################################################################################################
#########################################################################################################
#########################################################################################################
###########################################Statistical Analysis##########################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

#linear model

#Including Continuous Variables:

regout=lm(price ~ age+length, data = completed_data)
summary(regout)
stdresids=rstandard(regout)
plot(regout$fitted.values,stdresids,pch=19)
abline(0,0,col="red",lwd=3)
#heteroskedasticity seems to be violated
qqnorm(regout$residuals)
qqline(regout$residuals,col="red",lwd=3)
hist(regout$residuals,col="red")
plot(density(regout$residuals),lwd=3,main="Density Plot of Residuals")

#Evidence of correlation between continuous varibles:
sub.completed.data=subset(completed_data,select = c("length","age"))
xx=cor(sub.completed.data)
library(corrplot)
corrplot(xx,method="ellipse")

#Including Continuous Variables+Categorial Variables:


regout=lm(price ~ age+
                  length+
                  as.factor(region)+
                  as.factor(make)+
                  as.factor(material)+
                  as.factor(year)+
                  as.factor(fuel)
            
          
          
          ,data = completed_data)
summary(regout)
filtered_summary=data.frame(summary(regout)$coef[summary(regout)$coef[,4] <= .05, 4])
stdresids=rstandard(regout)
plot(regout$fitted.values,stdresids,pch=19)
abline(0,0,col="red",lwd=3)
#heteroskedasticity seems to be violated
qqnorm(regout$residuals)
qqline(regout$residuals,col="red",lwd=3)
hist(regout$residuals,col="red")
plot(density(regout$residuals),lwd=3,main="Density Plot of Residuals")

#Including also Zipcode and Category
regout=lm(price ~ age+
            length+
            as.factor(make)+
            as.factor(material)+
            as.factor(year)+
            as.factor(region)+
            as.factor(fuel)+
            as.factor(category)+
            as.factor(zipcode)
            
          
          
          
          ,data = completed_data)

summary(regout)


#GAM Model
library(mgcv)
regout2=gam(completed_data$price ~ completed_data$age+
              completed_data$length+
               as.factor(completed_data$make)+
               as.factor(completed_data$material)+
               as.factor(completed_data$year)+
               as.factor(completed_data$region)+
               as.factor(completed_data$fuel)+
              as.factor(completed_data$zipcode)+
              as.factor(completed_data$category))
summary(regout2)

#Interaction Between price and age
regout_interaction=lm(price ~ age+
            length+
            as.factor(make)+
            as.factor(material)+
            as.factor(year)+
            as.factor(region)+
            as.factor(fuel)+
            as.factor(category)+
            as.factor(zipcode)+age*
              length,data = completed_data)
summary(regout_interaction)

#Interaction Between Length and Region

regout_interaction2=lm(price ~ age+
                        length+
                        as.factor(make)+
                        as.factor(material)+
                        as.factor(year)+
                        as.factor(region)+
                        as.factor(fuel)+
                        as.factor(category)+
                        as.factor(zipcode)+length*
                        as.factor(region),data = completed_data)
summary(regout_interaction2)

#Interaction Between Age and Region
regout_interaction3=lm(price ~ age+
                         length+
                         as.factor(make)+
                         as.factor(material)+
                         as.factor(year)+
                         as.factor(region)+
                         as.factor(fuel)+
                         as.factor(category)+
                         as.factor(zipcode)+age*
                         as.factor(region),data = completed_data)
summary(regout_interaction3)

#Interaction Between Length/Region, Age/Region and Age/Length
regout_interaction4=lm(price ~ age+
                         length+
                         as.factor(make)+
                         as.factor(material)+
                         as.factor(year)+
                         as.factor(region)+
                         as.factor(fuel)+
                         as.factor(category)+
                         as.factor(zipcode)+age*
                         as.factor(region)+
                         age*length+
                         as.factor(region)*length,data = completed_data)
summary(regout_interaction4)
stdresids=rstandard(regout_interaction4)
plot(regout_interaction4$fitted.values,stdresids,pch=19)
abline(0,0,col="red",lwd=3)
#heteroskedasticity seems to be violated
qqnorm(regout_interaction4$residuals)
qqline(regout_interaction4$residuals,col="red",lwd=3)
hist(regout_interaction4$residuals,col="red")
plot(density(regout_interaction4$residuals),lwd=3,main="Density Plot of Residuals")





